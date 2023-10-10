package midgard.frontside

import  chisel3._
import  chisel3.util._
import  midgard._
import  midgard.util._


class VLBReq(val P: Param) extends Bundle {
  val w     = if (P.tlbEn) 3 else 2

  val idx   = UInt(P.vlbIdx.W)
  val vpn   = UInt(P.vpnBits.W)
  val kill  = UInt(w.W)
}

class VLBRes(val P: Param) extends Bundle {
  val idx   = UInt(P.vlbIdx.W)
  val vld   = Bool()
  val err   = Bool()
  val mpn   = UInt(P.mpnBits.W)
  val attr  = UInt(P.attrBits.W)
}

class InvReq(val P: Param) extends Bundle {
  val idx   = UInt(2.W)
  val mcn   = UInt(P.mcnBits.W)
}

class TTWExt(val P: Param) extends Bundle {
  val idx   = UInt(P.vlbIdx.W)
  val vpn   = UInt(P.vpnBits.W)
  val err   = Bool()
}

class VMA(val P: Param) extends Bundle {
  val vld   = Bool()
  val asid  = UInt(P.asidBits.W)
  val sdid  = UInt(P.sdidBits.W)
  val base  = UInt(P.vpnBits.W)
  val bound = UInt(P.vpnBits.W)
  val offs  = UInt(P.vpnBits.W)
  val attr  = UInt(P.attrBits.W)
  val pmt   = UInt(P.pmtBits.W)

  def gt (v: UInt): Bool = {
    vld && (v > bound)
  }
  def lt (v: UInt): Bool = {
    vld && (v < base)
  }
  def hit(v: UInt, a: UInt): Bool = {
    if (P.vscEn)
      vld &&
         (v === base) &&
         (a === asid)
    else
      vld && !gt(v) && !lt(v) &&
         (a === asid)
  }
  def err(v: UInt): Bool = {
    if (P.vscEn)
      vld &&  gt(v)
    else
      false.B
  }
  def clr(k: UInt, a: UInt, s: UInt, x: Bool, i: InvReq): Bool = {
    val p = (i.mcn ## i.idx)(P.pmtBits.W)

    vld &&
      ((a === asid) &&
       (s === sdid) && k(1) || k(2) ||
       (p === pmt ) && x)
  }
}

class VMP(val P: Param) extends Bundle {
  val vld   = Bool()
  val asid  = UInt(P.asidBits.W)
  val sdid  = UInt(P.sdidBits.W)
  val vpn   = UInt(P.vpnBits.W)
  val mpn   = UInt(P.mpnBits.W)
  val attr  = UInt(P.attrBits.W)
  val pmt   = UInt(P.pmtBits.W)

  def hit(v: UInt, a: UInt, s: UInt): Bool = {
    vld && (v === vpn) &&
       (a === asid)
  }
  def clr(k: UInt, a: UInt, s: UInt, x: Bool, i: InvReq): Bool = {
    val p = (i.mcn ## i.idx)(P.pmtBits.W)

    vld &&
      ((a === asid) &&
       (s === sdid) && k(1) || k(2) ||
       (p === pmt ) && x)
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

object InvReq {
  def apply(P: Param, i: UInt, m: UInt): InvReq = {
    val ret = Pin(new InvReq(P))

    ret.idx   := i
    ret.mcn   := m
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
    ret.sdid  := DontCare
    ret.base  := DontCare
    ret.bound := DontCare
    ret.offs  := DontCare
    ret.attr  := DontCare
    ret.pmt   := DontCare
    ret
  }
  def apply(P: Param, v: BT4VMA, a: UInt, s: UInt, p: UInt): VMA = {
    val ret = Pin(new VMA(P))

    ret.vld   := true.B
    ret.asid  := a
    ret.sdid  := s
    ret.base  := v.base
    ret.bound := v.bound
    ret.offs  := v.offs
    ret.attr  := v.attr
    ret.pmt   := p
    ret
  }
  def apply(P: Param, v: VSCVMA, a: UInt, b: UInt, p: UInt): VMA = {
    val ret = Pin(new VMA(P))

    ret.vld   := v.vld
    ret.asid  := a
    ret.sdid  := v.sdid
    ret.base  := b
    ret.bound := v.bound
    ret.offs  := v.offs
    ret.attr  := v.attr
    ret.pmt   := p
    ret
  }
}

object VMP {
  def apply(P: Param, a: UInt, s: UInt, v: UInt, m: UInt, t: UInt, p: UInt): VMP = {
    val ret = Pin(new VMP(P))

    ret.vld   := true.B
    ret.asid  := a
    ret.sdid  := s
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

  val vlb_req_i   = IO(Vec(N, Flipped(Valid(new VLBReq(P)))))
  val vlb_res_o   = IO(Vec(N,         Valid(new VLBRes(P))))
  val vlb_ttw_o   = IO(               Valid(new VLBRes(P)))

  val inv_req_i   = IO(       Flipped(Valid(new InvReq(P))))

  val ttw_req_o   = IO(               Valid(new VLBReq(P)))
  val ttw_res_i   = IO(       Flipped(Valid(new VMA   (P))))
  val ttw_ext_i   = IO(               Input(new TTWExt(P)))

  val satp_i      = IO(               Input(UInt(64.W)))
  val uatp_i      = IO(               Input(UInt(64.W)))
  val uatc_i      = IO(               Input(new VSCCfg()))

  // 0: kill the inflight ttw
  // 1: kill vlb entries with matching asid/sdid
  // 2: kill all vlb entries
  val kill_i      = IO(               Input(UInt(3.W)))
  val kill_asid_i = IO(               Input(UInt(P.asidBits.W)))
  val kill_sdid_i = IO(               Input(UInt(P.sdidBits.W)))


  // ---------------------------
  // logic

  val sx_kill        = Any(kill_i(2, 1))
  val sx_qual        = Non(kill_i(2, 1))

  val inv_req        = inv_req_i.valid
  val inv_req_pld    = inv_req_i.bits

  val ttw_req        = ttw_req_o.fire
  val ttw_res        = ttw_res_i.fire
  val ttw_res_pld    = ttw_res_i.bits
  val ttw_res_ext    = ttw_ext_i

  val asid           = satp_i(44 :+ P.asidBits)
  val sdid           = uatp_i(48 :+ P.sdidBits)


  //
  // stage pre

  val sp_req         = vlb_req_i.map(_.valid)
  val sp_req_idx     = vlb_req_i.map(_.bits.idx)
  val sp_req_vpn     = vlb_req_i.map(_.bits.vpn)
  val sp_kill        = vlb_req_i.map(_.bits.kill)

  val sp_hit         = Pin(Vec(N, Bool()))
  val sp_mis         = Pin(Vec(N, Bool()))
  val sp_hit_err     = Pin(Vec(N, Bool()))
  val sp_hit_inv     = Pin(Vec(N, Bool()))
  val sp_hit_mux     = Pin(Vec(N, new VMP(P)))

  // forward decl
  val s0_req         = Pin(Bool())
  val s0_req_idx     = Pin(UInt(P.vlbIdx.W))
  val s0_req_vpn     = Pin(UInt(P.vpnBits.W))

  val s1_req_sel_q   = Pin(Vec(N, Bool()))

  val s0_kill        = Pin(Bool())
  val s1_kill        = Pin(Bool())
  val s1_qual        = Pin(Bool())

  val sp_ttw_res     = Pin(Bool())
  val sp_ttw_res_pld = Pin(new VMP(P))

  if (P.tlbEn) {
    // body
    val tlb_q = Pin(Vec(P.tlbWays, new VMP(P)))

    for (i <- 0 until N) {
      val sp_hit_way = tlb_q.map(_.hit(sp_req_vpn(i),
                                       asid,
                                       sdid)).U
      val sp_hit_any = Any(sp_hit_way)

      // also consider the case of refilling tlb
      val sp_ttw_hit = sp_ttw_res &&
                       sp_ttw_res_pld.hit(sp_req_vpn(i),
                                          asid,
                                          sdid)

      sp_hit(i) := sp_req(i) && !sp_hit_err(i) && (sp_hit_any ||  sp_ttw_hit)
      sp_mis(i) := sp_req(i) && !sp_hit_err(i) && !sp_hit_any && !sp_ttw_hit

      sp_hit_mux(i) := sp_ttw_hit ?? VMP(P,
                                         sp_ttw_res_pld.asid,
                                         sp_ttw_res_pld.sdid,
                                         sp_req_vpn(i),
                                         sp_ttw_res_pld.mpn,
                                         sp_ttw_res_pld.attr,
                                         sp_ttw_res_pld.pmt) ::
                                     OrM(sp_hit_way, tlb_q)

      Chk(sp_req(i) -> OHp(sp_hit_way ## sp_ttw_hit, true.B))
    }

    val sp_inv_way = tlb_q.map(e => !e.vld).U
    val sp_old_way = tlb_q.map(e =>  e.vld && !((e.asid === asid) && (e.sdid === sdid))).U
    val sp_rpl_way = Any(sp_inv_way) ?? PrL(sp_inv_way) ::
                     Any(sp_old_way) ?? PrL(sp_old_way) ::
                                        PRA(P.tlbWays, sp_ttw_res)

    for (i <- 0 until P.tlbWays) {
      val set = sp_ttw_res && sp_rpl_way(i)
      val clr = tlb_q(i).clr(kill_i,
                             kill_asid_i,
                             kill_sdid_i,
                             inv_req,
                             inv_req_pld)

      tlb_q(i)     := RegEnable(sp_ttw_res_pld, set)
      tlb_q(i).vld := RegEnable(set, false.B,   set || clr)
    }

    // don't issue the same req b2b to vlb to avoid multi-hit
    val sp_req_nxt   = sp_mis.U & sp_req_vpn.map(v => !(s0_req && (v === s0_req_vpn))).U

    // round-robin
    val sp_req_any   = Any    (sp_req_nxt) && sx_qual
    val sp_req_sel   = RRA    (sp_req_nxt, sp_req_any)
    val s0_req_sel_q = RegNext(sp_req_sel, 0.U)

    s0_req     := RegNext  (sp_req_any, false.B) && !s0_kill && sx_qual
    s0_req_idx := RegEnable(OrM(sp_req_sel, sp_req_idx), sp_req_any)
    s0_req_vpn := RegEnable(OrM(sp_req_sel, sp_req_vpn), sp_req_any)

    s0_kill    := OrM(s0_req_sel_q, sp_kill.map(k => k(1) || RegNext(k(0), false.B)))
    s1_kill    := OrM(s1_req_sel_q, sp_kill.map(k => k(2)))
    s1_qual    := true.B

    for (i <- 0 until N) {
      s1_req_sel_q(i) := RegNext(s0_req_sel_q(i), false.B)
    }

  } else {

    for (i <- 0 until N) {
      sp_hit    (i) := DontCare
      sp_mis    (i) := DontCare
      sp_hit_mux(i) := DontCare
    }

    // round-robin
    val sp_req_any = Any(sp_req.U) && sx_qual
    val sp_req_sel = RRA(sp_req.U, sp_req_any)

    s0_req     := sp_req_any
    s0_req_idx := OrM(sp_req_sel, sp_req_idx)
    s0_req_vpn := OrM(sp_req_sel, sp_req_vpn)

    // for better timing and efficiency, upstream should drive these kills
    // using super slow/late signals
    s0_kill    := DontCare
    s1_kill    := OrM(s1_req_sel_q, sp_kill.map(k => k(1)))
    s1_qual    := OrM(s1_req_sel_q, sp_kill.map(k => Non(RegNext(k(0)))))

    for (i <- 0 until N) {
      s1_req_sel_q(i) := RegNext(sp_req_sel(i), 0.U)
    }
  }


  //
  // stage 0

  val s0_hit_way     = Pin(Vec (P.vlbWays, Bool()))
  val s0_err_way     = Pin(Vec (P.vlbWays, Bool()))
  val s2_rpl_way     = Pin(UInt(P.vlbWays.W))

  val s2_ttw_res_raw = ttw_res
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
                   s2_ttw_res_pld.hit(s0_req_vpn_vsc, asid)
  val s0_ttw_err = s0_req     &&
                   s2_ttw_res &&
                   s2_ttw_res_pld.err(s0_req_vpn)

  // body
  val vlb_q = Pin(Vec(P.vlbWays, new VMA(P)))

  for (i <- 0 until P.vlbWays) {
    val rpl = s2_rpl_way(i)
    val set = s2_ttw_res && rpl

    val hit = vlb_q(i).hit(s0_req_vpn_vsc,
                           asid)
    val err = vlb_q(i).err(s0_req_vpn)
    val clr = vlb_q(i).clr(kill_i,
                           kill_asid_i,
                           kill_sdid_i,
                           inv_req,
                           inv_req_pld)

    val vld = sx_qual && !set && !clr

    // calculated for stage 1. not qualified yet
    s0_hit_way(i) := rpl && s0_ttw_hit || hit && vld
    s0_err_way(i) := rpl && s0_ttw_err || err && vld

    vlb_q(i)     := RegEnable(s2_ttw_res_pld, set)
    vlb_q(i).vld := RegEnable(set, false.B,   set || clr)
  }

  val s2_inv_way = vlb_q.map(e => !e.vld).U
  val s2_old_way = vlb_q.map(e =>  e.vld && (e.asid =/= asid)).U

  s2_rpl_way  := Any(s2_inv_way) ?? PrL(s2_inv_way) ::
                 Any(s2_old_way) ?? PrL(s2_old_way) ::
                                    PRA(P.vlbWays, s2_ttw_res)


  //
  // stage 1

  val s1_req_q     = RegNext  (s0_req)
  val s1_req_idx_q = RegEnable(s0_req_idx, s0_req)
  val s1_req_vpn_q = RegEnable(s0_req_vpn, s0_req)

  val s1_hit_way_q = RegEnable(s0_hit_way, s0_req).U
  val s1_err_way_q = RegEnable(s0_err_way, s0_req).U
  val s1_hit_err_q = Pin(Bool())
  val s1_hit_inv_q = Pin(Bool())

  // qualified
  val s1_hit_way = s1_hit_way_q & vlb_q.map(_.vld).U
  val s1_hit_any = Any(s1_hit_way)
  val s1_err_any = Any(s1_hit_way & s1_err_way_q)

  Chk(s1_req_q  -> OHp(s1_hit_way, true.B))

  // send resps back even for reqs that are being killed. upstream may use these
  // resps to generate the kill...
  val s1_req     = s1_req_q &&  s1_qual      &&  sx_qual
  val s1_hit     = s1_req   && !s1_hit_err_q &&  s1_hit_any
  val s1_mis     = s1_req   && !s1_hit_err_q && !s1_hit_any
  val s1_hit_err = s1_req   && !s1_hit_err_q &&  s1_err_any

  val s1_hit_mux = OrM(s1_hit_way, vlb_q)

  // not a second mux
  // the two muxes actually mux different parts of vlb_q
  val s1_res_mpn = s1_req_vpn_q + RegEnable(s0_ttw_hit ?? s2_ttw_res_pld.offs ::
                                                          OrM(s0_hit_way.U & vlb_q.map(_.vld).U,
                                                              vlb_q.map(_.offs)),
                                            s0_req)

  // incoming ttw fill can also hit missed s1 req
  // just retry without forwarding
  val s1_ttw_hit = s1_req         &&
                   s2_ttw_res_raw &&
                   s2_ttw_res_pld.hit(vsc_vpn(s1_req_vpn_q), asid)

  // really start ttw
  val s1_mis_vld = s1_mis && !s1_kill && !kill_i(0) && !s1_ttw_hit

  // refill tlb upon hitting vlb. the hit entry is always valid
  if (P.tlbEn) {
    sp_ttw_res     := s1_hit && !s1_kill && !s1_err_any
    sp_ttw_res_pld := VMP(P,
                          asid,
                          sdid,
                          s1_req_vpn_q,
                          s1_res_mpn,
                          s1_hit_mux.attr,
                          s1_hit_mux.pmt)

  } else {
    sp_ttw_res     := DontCare
    sp_ttw_res_pld := DontCare
  }

  // 2-entry error to ensure minimum forward progress
  val s2_ttw_err_set   = ttw_res && (ttw_res_ext.err || !ttw_res_pld.vld)
  val s2_ttw_err_vpn   =             ttw_res_ext.vpn
  val s2_ttw_err_inv   =                                !ttw_res_pld.vld
  val s2_vsc_err_set   = s1_hit_err
  val s2_vsc_err_vpn   = s1_req_vpn_q

  val s2_err_clr       = Any(sp_hit_err.U ## s1_hit_err_q ## sx_kill)

  val s2_ttw_err_vld_q = RegEnable(s2_ttw_err_set || !s2_err_clr, false.B, s2_ttw_err_set || s2_err_clr)
  val s2_ttw_err_vpn_q = RegEnable(s2_ttw_err_vpn,                         s2_ttw_err_set)
  val s2_ttw_err_inv_q = RegEnable(s2_ttw_err_inv,                         s2_ttw_err_set)
  val s2_vsc_err_vld_q = RegEnable(s2_vsc_err_set || !s2_err_clr, false.B, s2_vsc_err_set || s2_err_clr)
  val s2_vsc_err_vpn_q = RegEnable(s2_vsc_err_vpn,                         s2_vsc_err_set)

  def sx_inv_err(v: UInt): UInt = {
    val hit_ttw_q = s2_ttw_err_vld_q && (v === s2_ttw_err_vpn_q) && !s1_hit_err_q
    val hit_vsc_q = s2_vsc_err_vld_q && (v === s2_vsc_err_vpn_q) && !s1_hit_err_q
    val hit_ttw   = s2_ttw_err_set   && (v === s2_ttw_err_vpn)

    val err = hit_ttw_q || hit_vsc_q || hit_ttw
    val inv = hit_ttw_q && s2_ttw_err_inv_q ||
              hit_ttw   && s2_ttw_err_inv

    inv ## err
  }

  for (i <- 0 until N) {
    if (P.tlbEn) {
      val sp_inv_err = sx_inv_err(sp_req_vpn(i))

      sp_hit_inv(i) := sp_req(i) && sp_inv_err(1)
      sp_hit_err(i) := sp_req(i) && sp_inv_err(0)
    } else {
      sp_hit_err(i) := false.B
      sp_hit_inv(i) := false.B
    }
  }

  val s0_inv_err = sx_inv_err(s0_req_vpn)

  s1_hit_inv_q := RegNext(s0_req && s0_inv_err(1), false.B)
  s1_hit_err_q := RegNext(s0_req && s0_inv_err(0), false.B)


  //
  // output

  def sx_mis_sdid(s: UInt): Bool = {
    Any(sdid) && (s =/= sdid)
  }

  if (P.tlbEn) {
    for (i <- 0 until N) {
      val sp_res_hit = sp_hit_err(i) && !sp_hit_inv(i) ||
                       sp_hit    (i)
      val sp_res_err = sp_hit_err(i) ||
                       sx_mis_sdid(sp_hit_mux(i).sdid)

      vlb_res_o(i).valid := sp_req(i)
      vlb_res_o(i).bits  := VLBRes(P,
                                   sp_req_idx(i),
                                   sp_res_hit,
                                   sp_res_err,
                                   sp_hit_mux(i).mpn,
                                   sp_hit_mux(i).attr)
    }

  } else {
    val s1_res_hit = s1_hit_err_q && !s1_hit_inv_q ||
                     s1_hit
    val s1_res_err = s1_hit_err_q ||
                     s1_hit_err   ||
                     sx_mis_sdid(s1_hit_mux.sdid)

    for (i <- 0 until N) {
      vlb_res_o(i).valid := s1_req && s1_req_sel_q(i)
      vlb_res_o(i).bits  := VLBRes(P,
                                   s1_req_idx_q,
                                   s1_res_hit,
                                   s1_res_err,
                                   s1_res_mpn,
                                   s1_hit_mux.attr)
    }
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
                            s1_req_idx_q,
                            s1_req_vpn_q,
                            sx_kill || kill_i(0))
}
