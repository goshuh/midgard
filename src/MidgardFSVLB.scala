package midgard.frontside

import  chisel3._
import  chisel3.util._
import  midgard._
import  midgard.util._


class VLBReq  (val P: Param) extends Bundle {
  def wid   = if (P.tlbEn) 3 else 2

  val idx   = UInt(P.vlbIdx.W)
  val vpn   = UInt(P.vpnBits.W)
  val kill  = UInt(wid.W)
}

class VLBResp (val P: Param) extends Bundle {
  val idx   = UInt(P.vlbIdx.W)
  val vld   = Bool()
  val err   = Bool()
  val mpn   = UInt(P.mpnBits.W)
  val attr  = UInt(P.attrBits.W)
}

class VMA     (val P: Param) extends Bundle {
  val vld   = Bool()
  val err   = Bool()
  val attr  = UInt(P.attrBits.W)
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
    vld && !gt(v) && !lt(v) && (a === asid)
  }
  def clr(e: Bool, a: UInt): Bool = {
    vld &&  e && (a === asid)
  }
}

class TLBEntry(val P: Param) extends Bundle {
  val vld   = Bool()
  val err   = Bool()
  val vpn   = UInt(P.vpnBits.W)
  val mpn   = UInt(P.mpnBits.W)
  val asid  = UInt(P.asidBits.W)
  val attr  = UInt(P.attrBits.W)

  def hit(v: UInt, a: UInt): Bool = {
    vld && (v === vpn) && (a === asid)
  }
  def clr(e: Bool, a: UInt): Bool = {
    vld &&  e && (a === asid)
  }
}


object VLBReq {
  def apply(P: Param, i: UInt, v: UInt, k: UInt): VLBReq = {
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

object TLBEntry {
  def apply(P: Param, e: Bool, v: UInt, m: UInt, t: UInt, s: UInt): TLBEntry = {
    val ret = Wire(new TLBEntry(P))

    ret.vld   := true.B
    ret.err   := e
    ret.vpn   := v
    ret.mpn   := m
    ret.attr  := t
    ret.asid  := s
    ret
  }
}


class VLB(val P: Param, N: Int) extends Module {

  val Iss = 0
  val Inv = 1

  // vlb always has a single requester
  require(!((N > 1) && !P.tlbEn))


  // --------------------------
  // io

  val vlb_req_i   = IO(Vec(N, Flipped(    Valid(new VLBReq (P)))))
  val vlb_resp_o  = IO(Vec(N,             Valid(new VLBResp(P))))
  val vlb_fill_o  = IO(                   Valid(new VLBResp(P)))

  val ptw_req_o   = IO(               Decoupled(new VLBReq (P)))
  val ptw_resp_i  = IO(       Flipped(    Valid(new VMA    (P))))

  val asid_i      = IO(                   Input(UInt(P.asidBits.W)))

  // asserted when
  // bit 0: the request waiting for ptw should be flushed
  // bit 1: all the vlb entries with the given asid should be invalidated
  val kill_i      = IO(                   Input(UInt(2.W)))
  val kill_asid_i = IO(                   Input(UInt(P.asidBits.W)))


  // --------------------------
  // logic

  val
     (fsm_idle ::
      fsm_req  ::
      fsm_resp ::
      fsm_null) = Enum(3)

  // common
  val sx_kill =  kill_i(Inv)
  val sx_qual = !kill_i(Inv)


  //
  // stage pre

  // killed request doesn't expect a response
  val sp_vld     = vlb_req_i.map(e => e.valid && !e.bits.kill(0))
  val sp_idx     = vlb_req_i.map(e => e.bits.idx)
  val sp_vpn     = vlb_req_i.map(e => e.bits.vpn)
  val sp_hit     = dontTouch(Wire(Vec(N, Bool())))
  val sp_mis     = dontTouch(Wire(Vec(N, Bool())))
  val sp_hit_mux = dontTouch(Wire(Vec(N, new TLBEntry(P))))

  // forward decl
  val s0_vld     = dontTouch(Wire(Bool()))
  val s0_idx     = dontTouch(Wire(UInt(P.vlbIdx.W)))
  val s0_vpn     = dontTouch(Wire(UInt(P.vpnBits.W)))

  val s1_kill    = dontTouch(Wire(Bool()))

  val sp_fill_vld   = dontTouch(Wire(Bool()))
  val sp_fill_pld   = dontTouch(Wire(new TLBEntry(P)))
  val sp_fill_rpl_q = dontTouch(Wire(UInt(P.tlbWays.W)))

  if (P.tlbEn) {
    // body
    val tlb_q = dontTouch(Wire(Vec(P.tlbWays, new TLBEntry(P))))

    for (i <- 0 until N) {
      val hit_way = tlb_q.map(_.hit(sp_vpn(i), asid_i)).U
      val hit_any = Any(hit_way)

      // also consider the case of refilling tlb
      val sp_ptw_hit = sp_fill_vld &&
                       sp_fill_pld.hit(sp_vpn(i), asid_i)

      sp_hit(i) := sp_vld(i) &&  hit_any
      sp_mis(i) := sp_vld(i) && !hit_any && !sp_ptw_hit

      // refilling case excluded for timing
      sp_hit_mux(i) := OrM(hit_way, tlb_q)

      assert(sp_vld(i) -> OHp(hit_way ## sp_ptw_hit, true.B))
    }

    for (i <- 0 until P.tlbWays) {
      val set = sp_fill_vld && sp_fill_rpl_q(i)
      val clr = tlb_q(i).clr(sx_kill, kill_asid_i)

      tlb_q(i)     := RegEnable(sp_fill_pld,  set)
      tlb_q(i).vld := RegEnable(set, false.B, set || clr)
    }

    // don't issue the same req b2b to vlb
    val sp_req = sp_mis.U & sp_vpn.map(v => !(s0_vld && (v === s0_vpn))).U

    // round-robin
    val sp_req_any = Any(sp_req) && sx_qual
    val sp_sel     = RRA(sp_req,    sp_req_any)
    val s0_sel_q   = RegEnable(sp_sel,   sp_req_any)
    val s1_sel_q   = RegEnable(s0_sel_q, s0_vld)

    s0_vld  := RegNext  (sp_req_any) && Non(s0_sel_q & vlb_req_i.map(_.bits.kill(1)).U) && sx_qual
    s0_idx  := RegEnable(OrM(sp_sel, sp_idx), sp_req_any)
    s0_vpn  := RegEnable(OrM(sp_sel, sp_vpn), sp_req_any)

    s1_kill := Any(s1_sel_q & vlb_req_i.map(_.bits.kill(2)).U)

  } else {
    sp_hit    (0) := DontCare
    sp_mis    (0) := DontCare
    sp_hit_mux(0) := DontCare

    s0_vld  := sp_vld(0)
    s0_idx  := sp_idx(0)
    s0_vpn  := sp_vpn(0)

    s1_kill := vlb_req_i(0).bits.kill(1)
  }


  //
  // stage 0

  val s2_fill_vld      = dontTouch(Wire(Bool()))
  val s2_fill_vld_qual = dontTouch(Wire(Bool()))
  val s2_fill_rpl_q    = dontTouch(Wire(UInt(P.vlbWays.W)))
  val s2_fill_pld      = VLBEntry(P, ptw_resp_i.bits, asid_i)

  // hit the refilling one
  val s0_ptw_hit = s0_vld           &&
                   s2_fill_vld_qual &&
                   s2_fill_pld.hit(s0_vpn, asid_i)

  val s0_hit     = dontTouch(Wire(Vec(P.vlbWays, Bool())))

  // body
  val vld_q = dontTouch(Wire(Vec(P.vlbWays, Bool())))
  val vlb_q = dontTouch(Wire(Vec(P.vlbWays, new VLBEntry(P))))

  for (i <- 0 until P.vlbWays) {
    val rpl = s2_fill_rpl_q(i)
    val set = s2_fill_vld_qual && rpl
    val clr = vlb_q(i).clr(sx_kill, kill_asid_i)

    // calculated for stage 1. not qualified yet
    s0_hit(i) := rpl &&         s0_ptw_hit ||
                !set && !clr && vlb_q(i).hit(s0_vpn, asid_i)

    vlb_q (i) := RegEnable(s2_fill_pld,  set)
    vld_q (i) := RegEnable(set, false.B, set || clr)
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

  assert(s1_vld_q -> OHp(s1_hit_way, true.B))

  val s1_vld     = s1_vld_q && !s1_kill
  val s1_hit     = s1_vld   &&  s1_hit_any
  val s1_mis     = s1_vld   && !s1_hit_any
  val s1_hit_mux = OrM(s1_hit_way, vlb_q)

  // not a second mux
  // the two muxes actually mux different parts of vlb_q
  val s1_mpn     = s1_vpn_q + RegEnable(s0_ptw_hit ?? ptw_resp_i.bits.offs ::
                                                      OrM(s0_hit.U & vld_q.U, vlb_q.map(_.offs)),
                                        s0_vld)

  // incoming ptw fill can also hit missed s1 req
  val s1_ptw_hit = s1_vld           &&
                   s2_fill_vld_qual &&
                   s2_fill_pld.hit(s1_vpn_q, asid_i)

  // really start ptw
  val s1_mis_vld = s1_mis && s1_adv && !s1_ptw_hit && sx_qual

  // refill tlb upon hitting vlb. the hit entry is always valid
  if (P.tlbEn) {
    sp_fill_vld   := s1_hit && sx_qual
    sp_fill_pld   := TLBEntry(P,
                              s1_hit_mux.err,
                              s1_vpn_q,
                              s1_mpn,
                              s1_hit_mux.attr,
                              asid_i)
    sp_fill_rpl_q := PRA(P.tlbWays, sp_fill_vld)

  } else {
    sp_fill_vld   := DontCare
    sp_fill_pld   := DontCare
    sp_fill_rpl_q := DontCare
  }


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
  s2_fill_rpl_q    := PRA(P.vlbWays, s2_fill_vld_qual)

  // stop the working ptw
  s2_stop := s2_fsm_is_resp && s2_kill

  // save one cycle. the vlb requests can go out-of-order
  s1_adv  := s2_fsm_is_idle || s2_stop || s2_fill_vld


  //
  // output

  if (P.tlbEn) {
    for (i <- 0 until N) {
      vlb_resp_o(i).valid := sp_vld(i)
      vlb_resp_o(i).bits  := VLBResp(P,
                                     sp_idx(i),
                                     sp_hit(i),
                                     sp_hit_mux(i).err,
                                     sp_hit_mux(i).mpn,
                                     sp_hit_mux(i).attr)
    }
  } else {
    for (i <- 0 until N) {
      vlb_resp_o(i).valid := s1_vld
      vlb_resp_o(i).bits  := VLBResp(P,
                                     s1_idx_q,
                                     s1_hit,
                                     s1_hit_mux.err,
                                     s1_mpn,
                                     s1_hit_mux.attr)
    }
  }

  vlb_fill_o.valid := s2_fill_vld
  vlb_fill_o.bits  := VLBResp(P,
                              s2_idx_q,
                              ptw_resp_i.bits.vld,
                              ptw_resp_i.bits.err,
                              0.U,
                              ptw_resp_i.bits.attr)

  ptw_req_o.valid  := s2_fsm_is_req && !s2_kill
  ptw_req_o.bits   := VLBReq (P,
                              s2_idx_q,
                              s2_vpn_q,
                              false.B ## s2_stop)
}