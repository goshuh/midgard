package midgard.frontside

import  chisel3._
import  chisel3.util._
import  midgard._
import  midgard.util._


class BT4VMA(val P: Param) extends Bundle {
  // 21 bytes in size
  val pad   = Opt (168 - P.attrBits - 3 * P.vpnBits)
  val offs  = UInt(P.vpnBits.W)
  val bound = UInt(P.vpnBits.W)
  val base  = UInt(P.vpnBits.W)
  val attr  = UInt(P.attrBits.W)

  def gt (v: UInt): Bool = {
    v > bound
  }
  def lt (v: UInt): Bool = {
    v < base
  }
}

class BT4Node(val P: Param) extends Bundle {
  // 128 bytes in size
  require(P.sdidBits <= 16)

  val ptr   = Vec (5, UInt(64.W))
  val vma   = Vec (4, new BT4VMA(P))
  val sdid  = UInt(16.W)
  val pad   = UInt(12.W)
  val num   = UInt( 3.W)
  val bot   = Bool()
}


class BT4(val P: Param) extends Module {

  // ---------------------------
  // io

  val vlb_req_i = IO(Vec(P.ttwNum, Flipped(    Valid(new VLBReq(P)))))
  val vlb_res_o = IO(Vec(P.ttwNum,             Valid(new VMA   (P))))
  val vlb_ext_o = IO(Vec(P.ttwNum,            Output(new TTWExt(P))))

  val mem_req_o = IO(                      Decoupled(new MemReq(P)))
  val mem_res_i = IO(              Flipped(Decoupled(new MemRes(P))))

  val inv_req_i = IO(              Flipped(    Valid(new InvReq(P))))

  val satp_i    = IO(                          Input(UInt(64.W)))
  val uatp_i    = IO(                          Input(UInt(64.W)))

  val idle_o    = IO(                         Output(Bool()))


  // ---------------------------
  // logic

  val
     (mem_fsm_idle ::
      mem_fsm_req  ::
      mem_fsm_res  ::
      mem_fsm_dly  ::
      mem_fsm_null) = Enum(4)

  val mem_res       = mem_res_i.fire
  val mem_res_pld   = mem_res_i.bits

  val asid          = satp_i(44 :+ P.asidBits)
  val sdid          = uatp_i(48 :+ P.sdidBits)


  //
  // req

  val ptw_req_raw   = vlb_req_i.map(_.valid).U
  val ptw_req_any   = Any(ptw_req_raw)
  val ptw_req_rdy   = Pin(Bool())

  val ptw_req       = ptw_req_any && ptw_req_rdy
  val ptw_req_sel   = RRA(ptw_req_raw, ptw_req)
  val ptw_req_pld   = OrM(ptw_req_sel,
                          vlb_req_i.map(_.bits))


  //
  // b-tree

  val ptw_req_sel_q = Pin(UInt(P.ttwNum.W))
  val ptw_req_idx_q = Pin(UInt(P.vlbIdx.W))
  val ptw_req_vpn_q = Pin(UInt(P.vpnBits.W))
  val ptw_req_mcn_q = Pin(UInt(P.mcnBits.W))

  val ptw_step      = Pin(Bool())
  val ptw_step_q    = Pin(Bool())

  val ptw_succ      = Pin(Bool())
  val ptw_fail      = Pin(Bool())
  val ptw_done      = ptw_succ || ptw_fail

  val ptw_btn_raw   = ptw_step_q ?? (mem_res_pld.data ## RegEnable(mem_res_pld.data, ptw_step && !ptw_step_q)) ::
                                    (0.U(P.clBits.W)  ## mem_res_pld.data)
  val ptw_btn       = ptw_btn_raw.asTypeOf(new BT4Node(P))

  val ptw_ptr_vld   = OrR(Dec(ptw_btn.num)(5.W)) & (ptw_step_q ?? 31.U(5.W) :: 16.U(5.W))
  val ptw_vma_vld   = ptw_ptr_vld(4, 1)

  val ptw_vma_gt    = ptw_btn.vma.map(_.gt(ptw_req_vpn_q)).U
  val ptw_vma_lt    = ptw_btn.vma.map(_.lt(ptw_req_vpn_q)).U

  // timing
  val ptw_lat       = mem_res_i.valid && !mem_res_i.ready

  val ptw_vma_gt_q  = RegEnable(ptw_vma_vld & ptw_vma_gt, ptw_lat)
  val ptw_vma_lt_q  = RegEnable(ptw_vma_vld & ptw_vma_lt, ptw_lat)

  val ptw_hit_q     = Pin(Vec(4, Bool()))
  val ptw_err_q     = Pin(Vec(4, Bool()))

  for (i <- 0 until 4) {
    ptw_hit_q(i) := RegEnable(ptw_vma_vld(i) && !ptw_vma_gt(i) && !ptw_vma_lt(i), ptw_lat)

    // simple errors
    // 1. base       >  vpn >  bound
    // 2. base (i-1) >  vpn >= base
    // 3. bound(i-1) >= vpn >= base
    // other cases not considered are simply treated as misses
    val prv_vma_gt = if (i == 0)  true.B else ptw_vma_gt(i - 1)
    val prv_vma_lt = if (i == 0) false.B else ptw_vma_lt(i - 1)

    ptw_err_q(i) := RegEnable(ptw_vma_vld(i) && (ptw_vma_lt(i) &&  ptw_vma_gt(i) ||
                                                !ptw_vma_lt(i) &&  prv_vma_lt    ||
                                                !ptw_vma_lt(i) && !prv_vma_gt),
                              ptw_lat)

    if (P.dbg)
      assert(ptw_step -> (ptw_btn.vma(i).bound >= ptw_btn.vma(i    ).base))
    if (P.dbg && (i > 0))
      assert(ptw_step -> (ptw_btn.vma(i).base  >  ptw_btn.vma(i - 1).bound))
  }

  // software mis-configuration
  // 1. simple errors
  // 2. gt is not 1111/0111/0011/0001/0000
  // 3. lt is not 0000/1000/1100/1110/1111
  val ptw_mis_cfg = ptw_step && (Any(ptw_err_q)                         ||
                                 Any(OrR(ptw_vma_gt_q) & ~ptw_vma_gt_q) ||
                                 Any(OrL(ptw_vma_lt_q) & ~ptw_vma_lt_q & ptw_vma_vld))

  // not exact
  // the hit entry is also selected, but the mangled result is never used
  val ptw_ptr_sel = ptw_ptr_vld & (true.B ## ~ptw_vma_gt_q) & (~ptw_vma_lt_q ## true.B)

  val ptw_hit_any = Any(ptw_hit_q)
  val ptw_sel_any = Any(ptw_ptr_sel)

  // medium-sized mux
  val ptw_vma_mux = OrM(ptw_hit_q,   ptw_btn.vma)
  val ptw_ptr_mux = OrM(ptw_ptr_sel, ptw_btn.ptr)

  ptw_succ := ptw_step &&  ptw_hit_any                               && !ptw_mis_cfg
  ptw_fail := ptw_step && !ptw_hit_any && !ptw_sel_any && ptw_step_q ||  ptw_mis_cfg

  ptw_step_q  := RegEnable(ptw_req ?? false.B :: !ptw_step_q,
                           false.B,
                           ptw_req || ptw_step)

  ptw_req_sel_q := RegEnable(ptw_req_sel,     ptw_req)
  ptw_req_idx_q := RegEnable(ptw_req_pld.idx, ptw_req)
  ptw_req_vpn_q := RegEnable(ptw_req_pld.vpn, ptw_req)
  ptw_req_mcn_q := RegEnable(ptw_req    ?? (satp_i(44.W) ## 0.U(6.W))  ::
                             ptw_step_q ??  ptw_ptr_mux(P.maBits := 6) ::
                                           (ptw_req_mcn_q + 1.U(P.mcnBits.W)),
                             ptw_req    ||  ptw_step)

  // error cases among iterations. unfair to check in hardware
  if (P.dbg) {
    val ptw_vma_min   = OrM(PrL(ptw_vma_vld), ptw_btn.vma.map(_.base))
    val ptw_vma_max   = OrM(PrR(ptw_vma_vld), ptw_btn.vma.map(_.bound))

    val ptw_vma_min_q = RegEnable(ptw_req ??  0.U :: ptw_vma_min, ptw_req || ptw_step)
    val ptw_vma_max_q = RegEnable(ptw_req ?? ~0.U :: ptw_vma_max, ptw_req || ptw_step)

    assert((ptw_step && Any(ptw_vma_vld)) ->
              ((ptw_vma_min > ptw_vma_min_q) &&  Any(ptw_vma_min_q) ||
               (ptw_vma_max < ptw_vma_max_q) && !All(ptw_vma_max_q)))
  }


  //
  // fsm

  val mem_fsm_en  = Pin(Bool())
  val mem_fsm_q   = Pin(UInt(2.W))
  val mem_fsm_nxt = Pin(UInt(2.W))

  // vlb wants to kill ptw
  val ptw_kill    = Pin(Bool())
  val ptw_stop    = ptw_kill || ptw_done

  mem_fsm_en  := false.B
  mem_fsm_nxt := mem_fsm_q

  switch (mem_fsm_q) {
    is (mem_fsm_idle) {
      mem_fsm_en  := ptw_req
      mem_fsm_nxt := mem_fsm_req
    }
    is (mem_fsm_req) {
      mem_fsm_en  := mem_req_o.ready
      mem_fsm_nxt := mem_fsm_res
    }
    is (mem_fsm_res) {
      mem_fsm_en  := mem_res_i.valid
      mem_fsm_nxt := mem_fsm_dly
    }
    // delay the vlb kill until now to make mem interface happy
    // if ptc is nevertheless required (mostly impossible), access it now
    is (mem_fsm_dly) {
      mem_fsm_en  := true.B
      mem_fsm_nxt := ptw_req  ?? mem_fsm_req  ::
                     ptw_stop ?? mem_fsm_idle ::
                                 mem_fsm_req
    }
  }

  mem_fsm_q := RegEnable(mem_fsm_nxt, mem_fsm_idle, mem_fsm_en)

  val mem_fsm_is_idle = mem_fsm_q === mem_fsm_idle
  val mem_fsm_is_req  = mem_fsm_q === mem_fsm_req
  val mem_fsm_is_res  = mem_fsm_q === mem_fsm_res
  val mem_fsm_is_dly  = mem_fsm_q === mem_fsm_dly

  ptw_req_rdy := mem_fsm_is_idle ||
                 mem_fsm_is_dly  && ptw_stop

  // keep mem req valid intact
  val ptw_kill_raw = Any(ptw_req_sel_q & vlb_req_i.map(_.bits.kill(0)).U) && !mem_fsm_is_idle

  // ptw can be killed immature
  val ptw_kill_q   = RegEnable(ptw_kill_raw && !ptw_step,
                               false.B,
                               ptw_kill_raw ||  ptw_step)

  ptw_step := mem_fsm_is_dly
  ptw_kill := ptw_kill_raw ||
              ptw_kill_q


  //
  // output

  val vlb_res_vma = ptw_fail ?? 0.U.asTypeOf(new BT4VMA(P)) :: ptw_vma_mux

  for (i <- 0 until P.ttwNum) {
    vlb_res_o(i).valid := ptw_done && ptw_req_sel_q(i) && !ptw_kill
    vlb_res_o(i).bits  := VMA(P,
                              vlb_res_vma,
                              asid,
                              sdid,
                              ptw_req_mcn_q(P.pmtBits.W))

    vlb_ext_o(i)  := TTWExt(P,
                            ptw_req_idx_q,
                            ptw_req_vpn_q,
                            ptw_fail)
  }

  mem_req_o.valid := mem_fsm_is_req
  mem_req_o.bits  := MemReq(P,
                            0.U,
                            ptw_req_mcn_q)

  mem_res_i.ready := mem_fsm_is_dly

  idle_o          := mem_fsm_is_idle
}