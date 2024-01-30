package midgard.frontside

import  chisel3._
import  chisel3.util._
import  midgard._
import  midgard.util._


class VLBReq(val P: Param) extends Bundle {
  val idx   = UInt(P.vlbIdx.W)
  val vpn   = UInt(P.vpnBits.W)
  val kill  = UInt(2.W)
}

class VLBRes(val P: Param) extends Bundle {
  val idx   = UInt(P.vlbIdx.W)
  val vld   = Bool()
  val err   = Bool()
  val mpn   = UInt(P.mpnBits.W)
  val attr  = UInt(8.W)

  def r = attr(1)
  def w = attr(2)
  def x = attr(3)
  def u = attr(4)
  def g = attr(5)
  def p = attr(6)
  def e = attr(7)
}

class TTWExt(val P: Param) extends Bundle {
  val idx   = UInt(P.vlbIdx.W)
  val vpn   = UInt(P.vpnBits.W)
  val err   = Bool()
}

class VMA(val P: Param) extends Bundle {
  val vld   = Bool()
  val asid  = UInt(P.asidBits.W)
  val csid  = UInt(P.csidBits.W)
  val base  = UInt(P.vpnBits.W)
  val bound = UInt(P.vpnBits.W)
  val offs  = UInt(P.vpnBits.W)
  val attr  = UInt(8.W)
  val pmt   = UInt(P.pmtBits.W)

  def g = attr(5)

  def gt (v: UInt): Bool = {
    vld && (v > bound)
  }
  def lt (v: UInt): Bool = {
    vld && (v < base)
  }
  def hit(v: UInt, a: UInt, c: UInt): Bool = {
    if (P.vscEn)
      vld &&
         (v === base) &&
         (a === asid) &&
        ((c === csid) || g)
    else
      vld && !gt(v) && !lt(v) &&
         (a === asid)
  }
  def old(a: UInt, c: UInt): Bool = {
    vld &&
      ((a =/= asid) ||
       (c =/= csid) && !g)
  }
  def err(v: UInt): Bool = {
    if (P.vscEn)
      vld &&  gt(v)
    else
      false.B
  }
  def clr(k: UInt, a: UInt, c: UInt, d: VTDReq): Bool = {
    val p = d.mcn(P.pmtBits.W)

    vld &&
      ((a === asid) &&
      ((c === csid) || g) && k(1) || k(2) ||
       (p === pmt ) && d.cmd(1))
  }
}

class VMP(val P: Param) extends Bundle {
  val vld   = Bool()
  val asid  = UInt(P.asidBits.W)
  val csid  = UInt(P.csidBits.W)
  val vpn   = UInt(P.vpnBits.W)
  val mpn   = UInt(P.mpnBits.W)
  val attr  = UInt(8.W)
  val pmt   = UInt(P.pmtBits.W)

  def g = attr(5)

  def hit(v: UInt, a: UInt, c: UInt): Bool = {
    vld &&
       (v === vpn ) &&
       (a === asid) &&
      ((c === csid) || g)
  }
  def old(a: UInt, c: UInt): Bool = {
    vld &&
      ((a =/= asid) ||
       (c =/= csid))
  }
  def clr(k: UInt, a: UInt, c: UInt, d: VTDReq): Bool = {
    val p = d.mcn(P.pmtBits.W)

    vld &&
      ((a === asid) &&
      ((c === csid) || g) && k(1) || k(2) ||
       (p === pmt ) && d.cmd(1))
  }
}


object VLBReq {
  def apply(P: Param, i: UInt, v: UInt, k: UInt): VLBReq = {
    val ret = Pin(new VLBReq(P))

    ret.idx   := i
    ret.vpn   := v
    ret.kill  := k
    ret
  }
}

object VLBRes {
  def apply(P: Param, i: UInt, v: Bool, e: Bool, m: UInt, a: UInt): VLBRes = {
    val ret = Pin(new VLBRes(P))

    ret.idx   := i
    ret.vld   := v
    ret.err   := e
    ret.mpn   := m
    ret.attr  := a
    ret
  }
}

object TTWExt {
  def apply(P: Param, i: UInt, v: UInt, e: UInt): TTWExt = {
    val ret = Pin(new TTWExt(P))

    ret.idx   := i
    ret.vpn   := v
    ret.err   := e
    ret
  }
}

object VMA {
  def apply(P: Param): VMA = {
    val ret = Pin(new VMA(P))

    ret.vld   := false.B
    ret.asid  := DontCare
    ret.csid  := DontCare
    ret.base  := DontCare
    ret.bound := DontCare
    ret.offs  := DontCare
    ret.attr  := DontCare
    ret.pmt   := DontCare
    ret
  }
  def apply(P: Param, v: Bool, a: UInt, s: UInt, b: UInt, c: UInt, o: UInt, t: UInt, p: UInt): VMA = {
    val ret = Pin(new VMA(P))

    ret.vld   := v
    ret.asid  := a
    ret.csid  := s
    ret.base  := b
    ret.bound := c
    ret.offs  := o
    ret.attr  := t
    ret.pmt   := p
    ret
  }
}

object VMP {
  def apply(P: Param, a: UInt, c: UInt, v: UInt, m: UInt, t: UInt, p: UInt): VMP = {
    val ret = Pin(new VMP(P))

    ret.vld   := true.B
    ret.asid  := a
    ret.csid  := c
    ret.vpn   := v
    ret.mpn   := m
    ret.attr  := t
    ret.pmt   := p
    ret
  }
}


class VLB(val P: Param, N: Int) extends Module {

  // ---------------------------
  // io

  val vlb_req_i   = IO(Vec(N, Flipped(Decoupled(new VLBReq(P)))))
  val vlb_res_o   = IO(Vec(N,         Decoupled(new VLBRes(P))))
  val vlb_ttw_o   = IO(                   Valid(new VLBRes(P)))

  val vtd_req_i   = IO(                   Input(new VTDReq(P)))

  val ttw_req_o   = IO(                   Valid(new VLBReq(P)))
  val ttw_res_i   = IO(       Flipped(    Valid(new VMA   (P))))
  val ttw_ext_i   = IO(                   Input(new TTWExt(P)))

  val uatc_i      = IO(                   Input(new VSCCfg()))
  val asid_i      = IO(                   Input(UInt(P.asidBits.W)))
  val csid_i      = IO(                   Input(UInt(P.csidBits.W)))

  // 0: kill the inflight ttw
  // 1: kill vlb entries with matching asid/sdid
  // 2: kill all vlb entries
  val kill_i      = IO(                   Input(UInt(3.W)))
  val kill_asid_i = IO(                   Input(UInt(P.asidBits.W)))
  val kill_csid_i = IO(                   Input(UInt(P.csidBits.W)))


  // ---------------------------
  // logic

  val sx_kill        = Any(kill_i(2, 1))
  val sx_qual        = Non(kill_i(2, 1))

  val vtd_req        = vtd_req_i

  val ttw_req        = ttw_req_o.fire
  val ttw_res        = ttw_res_i.fire
  val ttw_res_pld    = ttw_res_i.bits
  val ttw_res_ext    = ttw_ext_i


  //
  // stage 0

  // tlb
  val sp_req         = vlb_req_i.map(_.valid)
  val sp_req_idx     = vlb_req_i.map(_.bits.idx)
  val sp_req_vpn     = vlb_req_i.map(_.bits.vpn)
  val sp_kill        = vlb_req_i.map(_.bits.kill)

  val sp_hit         = Pin(Vec (N, Bool()))
  val sp_hit_err     = Pin(Vec (N, Bool()))
  val sp_hit_inv     = Pin(Vec (N, Bool()))
  val sp_hit_mux     = Pin(Vec (N, new VMP(P)))

  val sp_ttw_res     = Pin(Bool())
  val sp_ttw_res_pld = Pin(new VMP(P))

  // body
  val tlb_q = Pin(Vec(P.tlbWays, new VMP(P)))

  for (i <- 0 until N) {
    val sp_hit_way = tlb_q.map(_.hit(sp_req_vpn(i),
                                     asid_i,
                                     csid_i)).U
    val sp_hit_any = Any(sp_hit_way)

    // also consider the case of refilling tlb
    val sp_ttw_hit = sp_ttw_res &&
                     sp_ttw_res_pld.hit(sp_req_vpn(i),
                                        asid_i,
                                        csid_i)

    sp_hit(i) := sp_req(i) && !sp_hit_err(i) && (sp_hit_any || sp_ttw_hit)

    sp_hit_mux(i) := sp_ttw_hit ?? VMP(P,
                                       sp_ttw_res_pld.asid,
                                       sp_ttw_res_pld.csid,
                                       sp_req_vpn(i),
                                       sp_ttw_res_pld.mpn,
                                       sp_ttw_res_pld.attr,
                                       sp_ttw_res_pld.pmt) ::
                                   OrM(sp_hit_way, tlb_q)

    Chk(sp_req(i) -> OHp(sp_hit_way ## sp_ttw_hit, true.B))
  }

  val sp_inv_way = tlb_q.map(e => !e.vld).U
  val sp_old_way = tlb_q.map(e =>  e.old(asid_i, csid_i)).U
  val sp_rpl_way = Any(sp_inv_way) ?? PrL(sp_inv_way) ::
                   Any(sp_old_way) ?? PrL(sp_old_way) ::
                                      PRA(P.tlbWays, sp_ttw_res)

  for (i <- 0 until P.tlbWays) {
    val set = sp_ttw_res && sp_rpl_way(i)
    val clr = tlb_q(i).clr(kill_i,
                           kill_asid_i,
                           kill_csid_i,
                           vtd_req)

    tlb_q(i)     := RegEnable(sp_ttw_res_pld, set)
    tlb_q(i).vld := RegEnable(set, false.B,   set || clr)
  }

  // tlb results
  val sp_req_q       = Pin(Vec (N, Bool()))
  val sp_req_idx_q   = Pin(Vec (N, UInt(P.vlbIdx.W)))

  val sp_hit_q       = Pin(Vec (N, Bool()))
  val sp_hit_err_q   = Pin(Vec (N, Bool()))
  val sp_hit_inv_q   = Pin(Vec (N, Bool()))
  val sp_hit_mux_q   = Pin(Vec (N, new VMP(P)))

  val sp_kill_q      = Pin(Vec (N, Bool()))

  for (i <- 0 until N) {
    sp_req_q    (i) := RegNext  (sp_req (i) && sx_qual, false.B)
    sp_req_idx_q(i) := RegEnable(sp_req_idx(i),         sp_req(i))

    sp_hit_q    (i) := RegNext  (sp_hit (i) && sx_qual, false.B)
    sp_hit_err_q(i) := RegNext  (sp_hit_err(i),         false.B)
    sp_hit_inv_q(i) := RegNext  (sp_hit_inv(i),         false.B)
    sp_hit_mux_q(i) := RegEnable(sp_hit_mux(i),         sp_hit(i))

    sp_kill_q   (i) := RegNext  (sp_kill(i)(0),         false.B)
  }

  val sp_req_any     = Any(sp_req.U) && sx_qual

  // round-robin
  val s0_req         = sp_req_any
  val s0_req_sel     = RRA(sp_req.U,   sp_req_any)
  val s0_req_idx     = OrM(s0_req_sel, sp_req_idx)
  val s0_req_vpn     = OrM(s0_req_sel, sp_req_vpn)

  // vlb
  val s0_hit_way     = Pin(Vec (P.vlbWays, Bool()))
  val s0_err_way     = Pin(Vec (P.vlbWays, Bool()))
  val s2_rpl_way     = Pin(UInt(P.vlbWays.W))

  val s2_ttw_res_raw = ttw_res && sx_qual
  val s2_ttw_res     = ttw_res && ttw_res_pld.vld
  val s2_ttw_res_pld = ttw_res_pld

  // vsc mode
  def vsc_vpn(vpn: UInt): UInt = {
    if (P.vscEn) {
      val va  = vpn ## 0.U(12.W)
      val vsc = BSR(va, uatc_i.vsc)(5.W) & uatc_i.vmask

      vpn & ~(Ext(BFL(uatc_i.mmask, vsc), P.vaBits))(P.vaBits := 12)

    } else
      vpn
  }

  val s0_req_vpn_vsc = vsc_vpn(s0_req_vpn)

  // hit the refilling one
  val s0_ttw_hit = s0_req     &&
                   s2_ttw_res &&
                   s2_ttw_res_pld.hit(s0_req_vpn_vsc, asid_i, csid_i)
  val s0_ttw_err = s0_req     &&
                   s2_ttw_res &&
                   s2_ttw_res_pld.err(s0_req_vpn)

  // body
  val vlb_q = Pin(Vec(P.vlbWays, new VMA(P)))

  for (i <- 0 until P.vlbWays) {
    val rpl = s2_rpl_way(i)
    val set = s2_ttw_res && rpl

    val hit = vlb_q(i).hit(s0_req_vpn_vsc,
                           asid_i,
                           csid_i)
    val err = vlb_q(i).err(s0_req_vpn)
    val clr = vlb_q(i).clr(kill_i,
                           kill_asid_i,
                           kill_csid_i,
                           vtd_req)

    val vld = sx_qual && !set && !clr

    // calculated for stage 1. not qualified yet
    s0_hit_way(i) := rpl && s0_ttw_hit || hit && vld
    s0_err_way(i) := rpl && s0_ttw_err || err && vld

    vlb_q(i)     := RegEnable(s2_ttw_res_pld, set)
    vlb_q(i).vld := RegEnable(set, false.B,   set || clr)
  }

  val s2_inv_way = vlb_q.map(e => !e.vld).U
  val s2_old_way = vlb_q.map(e =>  e.old(asid_i, csid_i)).U

  s2_rpl_way  := Any(s2_inv_way) ?? PrL(s2_inv_way) ::
                 Any(s2_old_way) ?? PrL(s2_old_way) ::
                                    PRA(P.vlbWays, s2_ttw_res)


  //
  // stage 1

  val s1_req_q     = RegNext  (s0_req)
  val s1_req_sel_q = RegNext  (s0_req_sel, 0.U)
  val s1_req_vpn_q = RegEnable(s0_req_vpn, s0_req)

  val s1_hit_way_q = RegEnable(s0_hit_way, s0_req).U
  val s1_err_way_q = RegEnable(s0_err_way, s0_req).U

  val s1_hit_err_q = RegNext  (Any(s0_req_sel & sp_hit_err.U), false.B)
  val s1_hit_inv_q = RegNext  (Any(s0_req_sel & sp_hit_inv.U), false.B)

  val s1_hit_way   = s1_hit_way_q & vlb_q.map(_.vld).U
  val s1_hit_any   = Any(s1_hit_way)
  val s1_err_any   = Any(s1_hit_way & s1_err_way_q)

  Chk(s1_req_q  -> OHp(s1_hit_way, true.B))

  // send resps back even for reqs that are being killed. upstream may use these
  // resps to generate the kill...
  val s1_qual      = Non(s1_req_sel_q & (sp_kill.map(_(1)).U |
                                         sp_kill_q.U |
                                         sp_hit_q.U)) &&
                         sx_qual

  val s1_hit_raw   = s1_req_q   && !s1_hit_err_q &&  s1_hit_any
  val s1_mis_raw   = s1_req_q   && !s1_hit_err_q && !s1_hit_any
  val s1_hit_err   = s1_req_q   && !s1_hit_err_q &&  s1_err_any

  val s1_hit       = s1_hit_raw &&  s1_qual
  val s1_mis       = s1_mis_raw &&  s1_qual

  val s1_hit_mux   = OrM(s1_hit_way, vlb_q)

  // not a second mux
  // the two muxes actually mux different parts of vlb_q
  val s1_res_mpn   = s1_req_vpn_q + RegEnable(s0_ttw_hit ?? s2_ttw_res_pld.offs ::
                                                            OrM(s0_hit_way.U & vlb_q.map(_.vld).U,
                                                                vlb_q.map(_.offs)),
                                              s0_req)

  // incoming ttw fill can also hit missed s1 req
  // just retry without forwarding
  val s1_ttw_hit   = s1_req_q       &&
                     s2_ttw_res_raw &&
                     s2_ttw_res_pld.hit(vsc_vpn(s1_req_vpn_q), asid_i, csid_i)

  // really start ttw
  val s1_mis_vld   = s1_mis && !kill_i(0) && !s1_ttw_hit

  // refill tlb upon hitting vlb. the hit entry is always valid
  sp_ttw_res      := s1_hit && !s1_err_any
  sp_ttw_res_pld  := VMP(P,
                         asid_i,
                         csid_i,
                         s1_req_vpn_q,
                         s1_res_mpn,
                         s1_hit_mux.attr,
                         s1_hit_mux.pmt)

  // 2-entry error to ensure minimum forward progress
  val s2_ttw_err_set   = s2_ttw_res && (ttw_res_ext.err || !ttw_res_pld.vld)
  val s2_ttw_err_vpn   =                ttw_res_ext.vpn
  val s2_ttw_err_inv   =                                   !ttw_res_pld.vld
  val s2_vsc_err_set   = s1_hit_err && s1_qual
  val s2_vsc_err_vpn   = s1_req_vpn_q

  val s2_err_clr       = sx_kill || Any(sp_hit_err_q)

  val s2_ttw_err_vld_q = RegEnable(s2_ttw_err_set || !s2_err_clr, false.B, s2_ttw_err_set || s2_err_clr)
  val s2_ttw_err_vpn_q = RegEnable(s2_ttw_err_vpn,                         s2_ttw_err_set)
  val s2_ttw_err_inv_q = RegEnable(s2_ttw_err_inv,                         s2_ttw_err_set)
  val s2_vsc_err_vld_q = RegEnable(s2_vsc_err_set || !s2_err_clr, false.B, s2_vsc_err_set || s2_err_clr)
  val s2_vsc_err_vpn_q = RegEnable(s2_vsc_err_vpn,                         s2_vsc_err_set)

  def sx_inv_err(v: UInt): UInt = {
    val hit_ttw_q = s2_ttw_err_vld_q && (v === s2_ttw_err_vpn_q)
    val hit_vsc_q = s2_vsc_err_vld_q && (v === s2_vsc_err_vpn_q)
    val hit_ttw   = s2_ttw_err_set   && (v === s2_ttw_err_vpn)

    val err = hit_ttw_q || hit_vsc_q || hit_ttw
    val inv = hit_ttw_q && s2_ttw_err_inv_q ||
              hit_ttw   && s2_ttw_err_inv

    inv ## err
  }

  for (i <- 0 until N) {
    val sp_inv_err = sx_inv_err(sp_req_vpn(i))

    sp_hit_inv(i) := sp_req(i) && sx_qual && sp_inv_err(1)
    sp_hit_err(i) := sp_req(i) && sx_qual && sp_inv_err(0)
  }


  //
  // output

  val s1_res_hit = s1_hit_raw   ||
                   s1_hit_err_q && !s1_hit_inv_q
  val s1_res_err = s1_hit_err   ||
                   s1_hit_err_q

  for (i <- 0 until N) {
    vlb_req_i(i).ready := true.B

    val sp_res_hit = sp_hit_q    (i) ||
                     sp_hit_err_q(i) && !sp_hit_inv_q(i)
    val sp_res_err = sp_hit_err_q(i)

    vlb_res_o(i).valid := NeQ(sp_kill_q(i), sp_req_q(i) || s1_req_sel_q(i) && s1_req_q)
    vlb_res_o(i).bits  := VLBRes(P,
                                 sp_req_idx_q(i),
                                 sp_res_hit || s1_req_sel_q(i) && s1_res_hit,
                                 sp_res_err || s1_req_sel_q(i) && s1_res_err,
                                 sp_res_hit ?? sp_hit_mux_q(i).mpn  :: s1_res_mpn,
                                 sp_res_hit ?? sp_hit_mux_q(i).attr :: s1_hit_mux.attr)
  }

  vlb_ttw_o.valid := s2_ttw_res_raw
  vlb_ttw_o.bits  := VLBRes(P,
                            ttw_ext_i.idx,
                            ttw_res_i.bits.vld,
                            ttw_ext_i.err,
                            0.U,
                            ttw_res_i.bits.attr)

  ttw_req_o.valid := s1_mis_vld
  ttw_req_o.bits  := VLBReq(P,
                            OrM(s1_req_sel_q, sp_req_idx_q),
                            s1_req_vpn_q,
                            sx_kill || kill_i(0))
}
