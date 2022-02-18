package midgard.frontside

import  chisel3._
import  chisel3.util._
import  midgard._
import  midgard.util._


class VLBTNode(val P: Param) extends Bundle {
  val vlb  = Vec(4, new VLBRange(P))
  val ptr  = Vec(5, UInt(P.mcnBits.W))
}

class MemReq  (val P: Param) extends Bundle {
  val mcn  = UInt(P.mcnBits.W)
}

class MemResp (val P: Param) extends Bundle {
  val data = UInt(P.clBits.W)
}


class PTW(P: Param, N: Int) extends Module {

  // --------------------------
  // io

  // slightly reusable in the future
  val vlb_req_i  = IO(Vec(N, Flipped(Decoupled(new VLBReq(P)))))
  val vlb_resp_o = IO(Vec(N,             Valid(new VLBRange(P))))

  val mem_req_o  = IO(               Decoupled(new MemReq(P)))
  val mem_resp_i = IO(       Flipped(Decoupled(new MemResp(P))))

  val satp_i     = IO(                   Input(UInt(P.mcnBits.W)))
  val idle_o     = IO(                  Output(Bool()))


  // ---------------------------
  // logic

  val
     (fsm_idle ::
      fsm_req  ::
      fsm_resp ::
      fsm_dly  ::
      fsm_null) = Enum(4)


  //
  // arb

  // round-robin among vlb reqs
  val arb_rdy = dontTouch(Wire(Bool()))
  val arb_sel = dontTouch(Wire(UInt(N.W)))
  val arb_gnt = EnQ(arb_rdy, arb_sel)

  val vlb_req_vld     = vlb_req_i.map(_.valid)
  val vlb_req_vld_any = Any(vlb_req_vld) && arb_rdy

  if (N > 1) {
    val arb_q = dontTouch(Wire(UInt(N.W)))
    val fwd   = vlb_req_vld &  arb_q
    val bwd   = vlb_req_vld & ~arb_q

    arb_sel := PrR(Any(fwd) ?? fwd :: bwd)
    arb_q   := RegEnable(OrL(RoL(arb_sel, 1)),
                        ~0.U,
                         vlb_req_vld_any)
  } else {
    arb_sel := 1.U(1.W)
  }

  val req_vld = vlb_req_vld_any
  val req_pld = OrM(arb_gnt, vlb_req_i.map(_.bits))


  //
  // b-tree

  val mem_btn     = mem_resp_i.bits.data.asTypeOf(new VLBTNode(P))
  val mem_btn_vld = mem_btn.vlb.map(_.vld)

  val ptw_src_q   = dontTouch(Wire(UInt(N.W)))
  val ptw_vpn_q   = dontTouch(Wire(UInt(P.vpnBits.W)))
  val ptw_mcn_q   = dontTouch(Wire(UInt(P.mcnBits.W)))

  val ptw_step    = dontTouch(Wire(Bool()))
  val ptw_succ    = dontTouch(Wire(Bool()))
  val ptw_fail    = dontTouch(Wire(Bool()))
  val ptw_done    = ptw_succ || ptw_fail

  val ptw_gt      = mem_btn.vlb.map(_.gt(ptw_vpn_q))
  val ptw_lt      = mem_btn.vlb.map(_.lt(ptw_vpn_q))

  // timing
  val ptw_upd     = mem_resp_i.valid && !mem_resp_i.ready

  val ptw_gt_q    = dontTouch(Wire(Vec(4, Bool())))
  val ptw_lt_q    = dontTouch(Wire(Vec(4, Bool())))
  val ptw_hit_q   = dontTouch(Wire(Vec(4, Bool())))
  val ptw_err_q   = dontTouch(Wire(Vec(4, Bool())))

  for (i <- 0 until 4) {
    ptw_gt_q (i) := RegEnable(mem_btn_vld(i) &&  ptw_gt(i),               ptw_upd)
    ptw_lt_q (i) := RegEnable(mem_btn_vld(i) &&  ptw_lt(i),               ptw_upd)
    ptw_hit_q(i) := RegEnable(mem_btn_vld(i) && !ptw_gt(i) && !ptw_lt(i), ptw_upd)

    // simple errors
    // 1. base       >  vpn >  bound
    // 2. base (i-1) >  vpn >= base
    // 3. bound(i-1) >= vpn >= base
    // other cases not considered are simply treated as misses
    val prv_gt = if (i == 0) true.B  else ptw_gt(i - 1)
    val prv_lt = if (i == 0) false.B else ptw_lt(i - 1)

    ptw_err_q(i) := RegEnable(mem_btn_vld(i) && (ptw_lt(i) &&  ptw_gt(i) ||
                                                !ptw_lt(i) &&  prv_lt    ||
                                                !ptw_lt(i) && !prv_gt),
                              ptw_upd)
  
    if (P.dbg)
      assert(ptw_step -> (mem_btn.vlb(i).bound >= mem_btn.vlb(i    ).base))
    if (P.dbg && (i > 0))
      assert(ptw_step -> (mem_btn.vlb(i).base  >  mem_btn.vlb(i - 1).bound))
  }

  // software mis-configuration
  // 1. simple errors
  // 2. gt is not 0000/1000/1100/1110/1111
  // 3. lt is not 1111/0111/0011/0001/0000
  val ptw_mis_cfg = ptw_step && (Any(ptw_err_q) ||
                                 Any(OrL(ptw_gt_q) & ~ptw_gt_q) ||
                                 Any(OrR(ptw_lt_q) & ~ptw_lt_q))

  // not exact
  // the hit entry is also selected, but the mangled result is never used
  val ptw_sel     = mem_btn_vld & (~ptw_gt_q ## true.B) & (true.B ## ~ptw_lt_q)

  val ptw_hit_any = Any(ptw_hit_q)
  val ptw_sel_any = Any(ptw_sel)

  // medium-sized mux
  val ptw_hit_mux = OrM(ptw_hit_q, mem_btn.vlb)
  val ptw_sel_mux = OrM(ptw_sel,   mem_btn.ptr)

  ptw_succ  := ptw_step &&  ptw_hit_any                 && !ptw_mis_cfg
  ptw_fail  := ptw_step && !ptw_hit_any && !ptw_sel_any ||  ptw_mis_cfg

  ptw_src_q := RegEnable(           arb_gnt,               req_vld)
  ptw_vpn_q := RegEnable(           req_pld.vpn,           req_vld)
  ptw_mcn_q := RegEnable(req_vld ?? satp_i :: ptw_sel_mux, req_vld || ptw_step)

  // error cases among iterations. unfair to check in hardware
  if (P.dbg) {
    val ptw_min   = OrM(PrL(mem_btn_vld), mem_btn.vlb.map(_.base))
    val ptw_max   = OrM(PrR(mem_btn_vld), mem_btn.vlb.map(_.bound))

    val ptw_min_q = RegEnable(req_vld ??  0.U :: ptw_min, req_vld || ptw_step)
    val ptw_max_q = RegEnable(req_vld ?? ~0.U :: ptw_max, req_vld || ptw_step)

    assert((ptw_step && Any(mem_btn_vld)) ->
              ((ptw_min > ptw_min_q) &&  Any(ptw_min_q) ||
               (ptw_max < ptw_max_q) && !All(ptw_max_q)))
  }


  //
  // fsm

  // vlb wants to kill ptw
  val ptw_kill_nq = Any(ptw_src_q & vlb_req_i.map(_.bits.kill))
  val ptw_kill    = dontTouch(Wire(Bool()))

  val mem_fsm_en  = dontTouch(Wire(Bool()))
  val mem_fsm_q   = dontTouch(Wire(UInt(2.W)))
  val mem_fsm_nxt = dontTouch(Wire(UInt(2.W)))

  val ptw_end = ptw_kill || ptw_done

  mem_fsm_en  := false.B
  mem_fsm_nxt := mem_fsm_q

  switch (mem_fsm_q) {
    is (fsm_idle) {
      mem_fsm_en  := req_vld
      mem_fsm_nxt := fsm_req
    }
    is (fsm_req) {
      mem_fsm_en  := mem_req_o.ready
      mem_fsm_nxt := fsm_resp
    }
    is (fsm_resp) {
      mem_fsm_en  := mem_resp_i.valid
      mem_fsm_nxt := fsm_dly
    }
    // delay the vlb kill until now to make mem interface happy
    // if ptc is nevertheless required (mostly impossible), access it now
    is (fsm_dly) {
      mem_fsm_en  := true.B
      mem_fsm_nxt := ptw_end ?? fsm_idle :: fsm_req
    }
  }

  mem_fsm_q := RegEnable(mem_fsm_nxt, fsm_idle, mem_fsm_en)

  val mem_fsm_is_idle = mem_fsm_q === fsm_idle
  val mem_fsm_is_req  = mem_fsm_q === fsm_req
  val mem_fsm_is_resp = mem_fsm_q === fsm_resp
  val mem_fsm_is_dly  = mem_fsm_q === fsm_dly

  arb_rdy  := mem_fsm_is_idle ||
              mem_fsm_is_dly  && ptw_end

  ptw_step := mem_fsm_is_dly
  ptw_kill := Any(mem_fsm_q) && (ptw_kill_nq || RegNext(ptw_kill))


  //
  // output

  val vlb_resp = ptw_fail ?? VLBRange(P, ptw_mis_cfg) :: ptw_hit_mux

  for (i <- 0 until N) {
    vlb_req_i (i).ready := arb_gnt(i)

    vlb_resp_o(i).valid := ptw_done && ptw_src_q(i)
    vlb_resp_o(i).bits  := vlb_resp
  }

  mem_req_o.valid    := mem_fsm_is_req
  mem_req_o.bits.mcn := ptw_mcn_q

  mem_resp_i.ready   := mem_fsm_is_dly

  idle_o             := mem_fsm_is_idle
}