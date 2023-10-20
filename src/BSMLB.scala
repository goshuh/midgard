package midgard.backside

import  chisel3._
import  chisel3.util._
import  midgard._
import  midgard.util._


class MLBReq(val P: Param) extends Bundle {
  val idx  = UInt(P.mrqIdx.W)
  val wnr  = Bool()
  val mpn  = UInt(P.mpnBits.W)
}

class MLBRes(val P: Param) extends Bundle {
  val rdy  = Bool()
  val idx  = UInt(P.mrqIdx.W)
  val err  = Bool()
  val ppn  = UInt(P.ppnBits.W)
  val attr = UInt(4.W)
}

class MLBEntry(val P: Param) extends Bundle {
  val vld  = Bool()
  val err  = Bool()
  val lvl  = UInt(3.W)
  val mpn  = UInt(P.mpnBits.W)
  val ppn  = UInt(P.ppnBits.W)
  val attr = UInt(4.W)

  def r = attr(0)
  def w = attr(1)
  def a = attr(2)
  def d = attr(3)

  def hit(m: UInt): Bool = {
    val arr = Pin(Vec(P.ptwLvl, Bool()))

    for (i <- 0 until P.ptwLvl) {
      val b = P.mpnBits - P.ptwTop - i * 9
      val w = if (i == 0) P.ptwTop else  9

      arr(i) := mpn(b :+ w) === m(b :+ w)
    }

    Any(Dec(lvl)(P.ptwLvl.W) & Rev(ArR(Rev(arr.U))))
  }
  def hpn(m: UInt): UInt = {
    val arr = Pin(Vec(P.ptwLvl, UInt(P.ppnBits.W)))

    for (i <- 0 until P.ptwLvl) {
      val b = P.mpnBits - P.ptwTop - i * 9

      if (b >= P.ppnBits)
        arr(i) := m  (P.ppnBits.W)
      else if (b > 0)
        arr(i) := ppn(P.ppnBits := b) ## m(b.W)
      else
        arr(i) := ppn
    }

    OrM(Dec(lvl)(P.ptwLvl.W), arr)
  }
}


object MLBReq {
  def apply(P: Param, i: UInt, w: Bool, m: UInt): MLBReq = {
    val ret = Pin(new MLBReq(P))

    ret.idx  := i
    ret.wnr  := w
    ret.mpn  := m
    ret
  }
}

object MLBRes {
  def apply(P: Param, r: Bool, i: UInt, e: Bool, p: UInt, a: UInt): MLBRes = {
    val ret = Pin(new MLBRes(P))

    ret.rdy  := r
    ret.idx  := i
    ret.err  := e
    ret.ppn  := p
    ret.attr := a
    ret
  }
}

object MLBEntry {
  def apply(P: Param): MLBEntry = {
    val ret = Pin(new MLBEntry(P))

    ret.vld  := false.B
    ret.err  := false.B
    ret.lvl  := DontCare
    ret.mpn  := DontCare
    ret.ppn  := DontCare
    ret.attr := DontCare
    ret
  }
  def apply(P: Param, e: Bool, l: UInt, m: UInt, p: UInt, a: UInt): MLBEntry = {
    val ret = Pin(new MLBEntry(P))

    ret.vld  := true.B
    ret.err  := e
    ret.lvl  := l
    ret.mpn  := m
    ret.ppn  := p
    ret.attr := a
    ret
  }
}


class MLB(val P: Param) extends Module {

  // ---------------------------
  // io

  val mrq_req_i = IO(Flipped(Valid(new MLBReq(P))))
  val mrq_res_o = IO(        Valid(new MLBRes(P)))
  val ptw_res_o = IO(        Valid(new MLBRes(P)))

  val ptw_req_o = IO(        Valid(new MLBReq(P)))
  val ptw_res_i = IO(Flipped(Valid(new MLBEntry(P))))

  val rst_i     = IO(        Input(Bool()))


  // ---------------------------
  // logic

  //
  // stage 0/1

  val sx_qual        = Non(rst_i)

  val s0_req         = mrq_req_i.valid
  val s0_req_pld     = mrq_req_i.bits

  val s1_req_q       = RegNext  (s0_req,     false.B)
  val s1_req_pld_q   = RegEnable(s0_req_pld, s0_req)

  val s1_hit         = Pin(Bool())
  val s1_mis         = Pin(Bool())
  val s1_res         = Pin(new MLBRes(P))

  val s2_ptw_rdy     = Pin(Bool())
  val s2_ptw_res_raw = Pin(Bool())
  val s2_ptw_res     = Pin(Bool())
  val s2_ptw_res_pld = Pin(new MLBEntry(P))

  val s0_ptw_hit     = s2_ptw_res && sx_qual && s2_ptw_res_pld.hit(s0_req_pld  .mpn)
  val s1_ptw_hit     = s2_ptw_res && sx_qual && s2_ptw_res_pld.hit(s1_req_pld_q.mpn)

  if (P.mlbEn) {
    val mlb_q = Pin(Vec(P.mlbWays, new MLBEntry(P)))

    val s2_inv_way = mlb_q.map(!_.vld).U
    val s2_rpl_way = Any(s2_inv_way) ?? PrL(s2_inv_way) ::
                                        PRA(P.mlbWays, s2_ptw_res)

    val s0_hit_way = Pin(Vec(P.mlbWays, Bool()))

    for (i <- 0 until P.mlbWays) {
      val rpl = s2_rpl_way(i)
      val set = s2_ptw_res && rpl

      val hit = mlb_q(i).hit(s0_req_pld.mpn)
      val clr = rst_i

      // calculated for stage 1. not qualified yet
      s0_hit_way(i) := rpl && s0_ptw_hit ||
                       hit && sx_qual && !set && !clr

      mlb_q(i)     := RegEnable(s2_ptw_res_pld, set)
      mlb_q(i).vld := RegEnable(set, false.B,   set || clr)
    }

    // qualified
    val s1_hit_way = mlb_q.map(_.vld).U & RegEnable(s0_hit_way.U, s0_req)
    val s1_hit_any = Any(s1_hit_way)
    val s1_hit_mux = OrM(s1_hit_way, mlb_q)

    Chk(s1_req_q  -> OHp(s1_hit_way, true.B))

    // perm fault
    val s1_ptw_res_pf  = Non(s2_ptw_res_pld.a && (s1_req_pld_q.wnr ?? (s2_ptw_res_pld.w && s2_ptw_res_pld.d) ::
                                                                       s2_ptw_res_pld.r))
    val s1_hit_res_pf  = Non(s1_hit_mux.a     && (s1_req_pld_q.wnr ?? (s1_hit_mux.w     && s1_hit_mux.d) ::
                                                                       s1_hit_mux.r))

    val s1_ptw_res_pld = MLBRes(P,
                                s2_ptw_rdy,
                                s1_req_pld_q.idx,
                                s2_ptw_res_pld.err || s1_ptw_res_pf,
                                s2_ptw_res_pld.hpn(s1_req_pld_q.mpn),
                                s2_ptw_res_pld.attr)
    val s1_hit_res_pld = MLBRes(P,
                                s2_ptw_rdy,
                                s1_req_pld_q.idx,
                                s1_hit_mux.err || s1_hit_res_pf,
                                s1_hit_mux.hpn(s1_req_pld_q.mpn),
                                s1_hit_mux.attr)

    s1_hit := s1_req_q && sx_qual && (s1_hit_any ||  s1_ptw_hit)
    s1_mis := s1_req_q && sx_qual && !s1_hit_any && !s1_ptw_hit

    s1_res := s1_ptw_hit ?? s1_ptw_res_pld :: s1_hit_res_pld

  } else {

    s1_hit := s1_req_q && sx_qual &&  s1_ptw_hit
    s1_mis := s1_req_q && sx_qual && !s1_ptw_hit

    s1_res := MLBRes(P,
                     s2_ptw_rdy,
                     s1_req_pld_q.idx,
                     s2_ptw_res_pld.err,
                     s2_ptw_res_pld.hpn(s1_req_pld_q.mpn),
                     s2_ptw_res_pld.attr)
  }

  val s1_mis_vld = s1_mis && s2_ptw_rdy


  //
  // stage 2

  val s2_req_q     = Pin(Bool())
  val s2_req_pld_q = RegEnable(s1_req_pld_q, s1_mis_vld)

  s2_req_q := RegEnable(s1_mis_vld,
                        false.B,
                        s1_mis_vld || s2_ptw_res_raw || rst_i)

  // stop receiving res on rst
  s2_ptw_res_raw := sx_qual        &&  ptw_res_i.valid
  s2_ptw_res     := s2_ptw_res_raw && !ptw_res_i.bits.err
  s2_ptw_res_pld :=                    ptw_res_i.bits

  // enable b2b
  s2_ptw_rdy := sx_qual && (s2_ptw_res_raw || !s2_req_q)


  //
  // output

  mrq_res_o.valid := s1_hit
  mrq_res_o.bits  := s1_res

  ptw_res_o.valid := s2_ptw_res_raw
  ptw_res_o.bits  := MLBRes(P,
                            true.B,
                            s2_req_pld_q.idx,
                            s2_ptw_res_pld.err,
                            s2_ptw_res_pld.hpn(s2_req_pld_q.mpn),
                            s2_ptw_res_pld.attr)

  ptw_req_o.valid := s1_mis_vld
  ptw_req_o.bits  := MLBReq(P,
                            s1_req_pld_q.idx,
                            false.B,
                            s1_req_pld_q.mpn)
}