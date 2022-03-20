package midgard.frontside

import  chisel3._
import  chisel3.util._
import  chisel3.util.random._
import  midgard._
import  midgard.util._


class VLBReq  (val P: Param) extends Bundle {
  val idx   = UInt(P.vlbIdx.W)
  val vpn   = UInt(P.vpnBits.W)
  val kill  = Bool()
}

class VLBResp (val P: Param) extends Bundle {
  val idx   = UInt(P.vlbIdx.W)
  val vld   = Bool()
  val err   = Bool()
  val mpn   = UInt(P.mpnBits.W)
  val attr  = UInt(4.W)
}

class VMA     (val P: Param) extends Bundle {
  val vld   = Bool()
  val err   = Bool()
  val attr  = UInt(4.W)
  val base  = UInt(P.vpnBits.W)
  val bound = UInt(P.vpnBits.W)
  val offs  = UInt(P.vpnBits.W)

  def gt (v: UInt): Bool = {
    vld && (v > bound)
  }
  def lt (v: UInt): Bool = {
    vld && (v < base)
  }
}

class VLBEntry(val Q: Param) extends VMA(Q) {
  val asid  = UInt(Q.asidBits.W)

  // TODO: global
  def hit(v: UInt, a: UInt): Bool = {
    vld && !gt(v) && !lt(v) && (asid === a)
  }
}


object VLBReq {
  def apply(P: Param, i: UInt, v: UInt, k: Bool): VLBReq = {
    val ret = Wire(new VLBReq(P))

    ret.idx  := i
    ret.vpn  := v
    ret.kill := k
    ret
  }
}

object VLBResp {
  def apply(P: Param, i: UInt, v: Bool, e: Bool, m: UInt, a: UInt): VLBResp = {
    val ret = Wire(new VLBResp(P))

    ret.idx  := i
    ret.vld  := v
    ret.err  := e
    ret.mpn  := m
    ret.attr := a
    ret
  }
}

object VMA {
  def apply(P: Param): VMA = {
    Wire(new VMA(P))
  }
  def apply[T <: VMA](P: Param, ret: T, e: Bool): T = {
    ret.vld   := false.B
    ret.err   := e
    ret.attr  := DontCare
    ret.base  := DontCare
    ret.bound := DontCare
    ret.offs  := DontCare
    ret
  }
  def apply[T <: VMA](P: Param, ret: T, a: UInt, b: UInt, c: UInt, o: UInt): T = {
    ret.vld   := true.B
    ret.err   := false.B
    ret.attr  := a
    ret.base  := b
    ret.bound := c
    ret.offs  := o
    ret
  }
  def apply(P: Param, e: Bool): VMA = {
    apply(P, apply(P), e)
  }
  def apply(P: Param, a: UInt, b: UInt, c: UInt, o: UInt): VMA = {
    apply(P, apply(P), a, b, c, o)
  }
}

object VLBEntry {
  def apply(P: Param): VLBEntry = {
    Wire(new VLBEntry(P))
  }
  def apply(P: Param, e: Bool, a: UInt): VLBEntry = {
    val ret = VMA(P,
                  apply(P),
                  e)
    ret.asid  := a
    ret
  }
  def apply(P: Param, r: VMA, a: UInt): VLBEntry = {
    val ret = VMA(P,
                  apply(P),
                  r.attr,
                  r.base,
                  r.bound,
                  r.offs)
    ret.asid  := a
    ret
  }
}


class VLB(val P: Param) extends Module {

  val Prv = 0
  val Iss = 1
  val Inv = 2


  // --------------------------
  // io

  val vlb_req_i  = IO(Flipped(    Valid(new VLBReq (P))))
  val vlb_resp_o = IO(            Valid(new VLBResp(P)))
  val vlb_fill_o = IO(            Valid(new VLBResp(P)))

  val ptw_req_o  = IO(        Decoupled(new VLBReq (P)))
  val ptw_resp_i = IO(Flipped(    Valid(new VMA    (P))))

  val asid_i     = IO(            Input(UInt(P.asidBits.W)))

  // asserted when
  // bit 0: the request issued in the last cycle should be flushed
  // bit 1: the request waiting for ptw should be flushed
  // bit 2: the whole vlb should be invalidated
  val kill_i     = IO(            Input(UInt(3.W)))


  // --------------------------
  // logic

  val
     (fsm_idle ::
      fsm_req  ::
      fsm_resp ::
      fsm_null) = Enum(3)


  //
  // stage 0

  val s2_fill_vld      = dontTouch(Wire(Bool()))
  val s2_fill_vld_qual = dontTouch(Wire(Bool()))
  val s2_fill_rpl_q    = dontTouch(Wire(UInt(log2Ceil(P.vlbWays).W)))
  val s2_fill_pld      = VLBEntry(P, ptw_resp_i.bits, asid_i)

  // killed request doesn't expect a response
  val s0_vld = vlb_req_i.valid && !vlb_req_i.bits.kill
  val s0_idx = vlb_req_i.bits.idx
  val s0_vpn = vlb_req_i.bits.vpn
  val s0_hit = dontTouch(Wire(Vec(P.vlbWays, Bool())))

  // hit the refilling one
  val s0_ptw_hit = s0_vld           &&
                   s2_fill_vld_qual &&
                   s2_fill_pld.hit(s0_vpn, asid_i)

  // body
  val vld_q = dontTouch(Wire(Vec(P.vlbWays, Bool())))
  val vlb_q = dontTouch(Wire(Vec(P.vlbWays, new VLBEntry(P))))

  val vlb_vld = !kill_i(Inv)

  for (i <- 0 until P.vlbWays) {
    val sel = s2_fill_rpl_q === i.U
    val set = s2_fill_vld_qual && sel

    // calculated for stage 1. not qualified yet
    s0_hit(i) := vlb_vld &&
                    (sel && s0_ptw_hit ||
                    !set && vlb_q(i).hit(s0_vpn, asid_i))

    vld_q (i) := RegEnable(vlb_vld && set, false.B, set || kill_i(Inv))
    vlb_q (i) := RegEnable(s2_fill_pld,             set)
  }


  //
  // stage 1

  val s1_adv     = dontTouch(Wire(Bool()))
  val s1_vld_q   = RegNext  (s0_vld)
  val s1_idx_q   = RegEnable(s0_idx, s0_vld)
  val s1_vpn_q   = RegEnable(s0_vpn, s0_vld)
  val s1_hit_q   = RegEnable(s0_hit, s0_vld).U

  // qualified
  val s1_hit_way = s1_hit_q & vld_q.U
  val s1_hit_any = Any(s1_hit_way)

  val s1_vld     = s1_vld_q && !kill_i(Prv)
  val s1_hit     = s1_vld   &&  s1_hit_any
  val s1_mis     = s1_vld   && !s1_hit_any
  val s1_hit_mux = OrM(s1_hit_way, vlb_q)

  // not a second mux
  // the two muxes actually mux different parts of vlb_q
  val s1_mpn     = s1_vpn_q + RegEnable(s0_ptw_hit ?? ptw_resp_i.bits.offs ::
                                                      OrM(s0_hit.U & vld_q.U, vlb_q.map(_.offs)),
                                        s0_vld)

  // really start ptw
  val s1_mis_vld = s1_mis && s1_adv


  //
  // stage 2

  val s2_idx_q   = RegEnable(s1_idx_q, s1_mis_vld)
  val s2_vpn_q   = RegEnable(s1_vpn_q, s1_mis_vld)

  // fsm
  val s2_fsm_en  = dontTouch(Wire(Bool()))
  val s2_fsm_q   = dontTouch(Wire(UInt(2.W)))
  val s2_fsm_nxt = dontTouch(Wire(UInt(2.W)))

  val s2_kill    = Any(kill_i(Inv := Iss))
  val s2_stop    = dontTouch(Wire(Bool()))

  s2_fsm_en  := false.B
  s2_fsm_nxt := s2_fsm_q

  switch (s2_fsm_q) {
    is (fsm_idle) {
      s2_fsm_en  := s1_mis_vld
      s2_fsm_nxt := fsm_req
    }
    is (fsm_req) {
      s2_fsm_en  := ptw_req_o.ready  || s2_kill
      s2_fsm_nxt := s1_mis_vld ?? fsm_req  ::
                    s2_kill    ?? fsm_idle ::
                                  fsm_resp
    }
    is (fsm_resp) {
      s2_fsm_en  := ptw_resp_i.valid || s2_kill
      s2_fsm_nxt := s1_mis_vld ?? fsm_req  :: fsm_idle
    }
  }

  s2_fsm_q := RegEnable(s2_fsm_nxt, fsm_idle, s2_fsm_en)

  val s2_fsm_is_idle = s2_fsm_q === fsm_idle
  val s2_fsm_is_req  = s2_fsm_q === fsm_req
  val s2_fsm_is_resp = s2_fsm_q === fsm_resp

  // TODO: other replacement policies
  s2_fill_vld      := ptw_resp_i.valid && s2_fsm_is_resp && !s2_kill
  s2_fill_vld_qual := s2_fill_vld && ptw_resp_i.bits.vld && !ptw_resp_i.bits.err
  s2_fill_rpl_q    := LFSR(log2Ceil(P.vlbWays), s2_fill_vld_qual)

  // stop the working ptw
  s2_stop := s2_fsm_is_resp && s2_kill

  // save one cycle. the vlb requests can go out-of-order
  s1_adv  := s2_fsm_is_idle || s2_stop || s2_fill_vld


  //
  // output

  vlb_resp_o.valid     := s1_vld
  vlb_resp_o.bits      := VLBResp(P,
                                  s1_idx_q,
                                  s1_hit,
                                  s1_hit_mux.err,
                                  s1_mpn,
                                  s1_hit_mux.attr)

  vlb_fill_o.valid     := s2_fill_vld
  vlb_fill_o.bits      := VLBResp(P,
                                  s2_idx_q,
                                  ptw_resp_i.bits.vld,
                                  ptw_resp_i.bits.err,
                                  0.U,
                                  ptw_resp_i.bits.attr)

  ptw_req_o.valid      := s2_fsm_is_req && !s2_kill
  ptw_req_o.bits       := VLBReq (P,
                                  s2_idx_q,
                                  s2_vpn_q,
                                  s2_stop)
}