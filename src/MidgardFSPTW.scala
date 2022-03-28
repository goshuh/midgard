package midgard.frontside

import  chisel3._
import  chisel3.util._
import  midgard._
import  midgard.util._


class VLBTNode(val P: Param) extends Bundle {
  def obj  = new VMA(P)
  val wid  = 1024 - 4 * obj.getWidth - 5 * P.mcnBits

  val pad  = UInt(wid.W)
  val bot  = Bool()
  val ptr  = Vec (5, UInt(P.mcnBits.W))
  val vma  = Vec (4, obj)
}

class MemReq  (val P: Param) extends Bundle {
  val mcn  = UInt(P.mcnBits.W)
}

class MemResp (val P: Param) extends Bundle {
  val data = UInt(P.clBits.W)
}


object MemReq {
  def apply(P: Param, m: UInt): MemReq = {
    val ret = Wire(new MemReq(P))

    ret.mcn := m
    ret
  }
}


class PTW(P: Param, N: Int) extends Module {

  // --------------------------
  // io

  // slightly reusable in the future
  val vlb_req_i  = IO(Vec(N, Flipped(Decoupled(new VLBReq (P)))))
  val vlb_resp_o = IO(Vec(N,             Valid(new VMA    (P))))

  val mem_req_o  = IO(               Decoupled(new MemReq (P)))
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

  val arb_rdy = dontTouch(Wire(Bool()))

  val vlb_req_vld     = vlb_req_i.map(_.valid).U
  val vlb_req_vld_any = Any(vlb_req_vld) && arb_rdy

  // round-robin among vlb reqs
  val arb_sel = RRA(vlb_req_vld, vlb_req_vld_any)
  val arb_gnt = EnQ(arb_rdy, arb_sel)

  val req_vld = vlb_req_vld_any
  val req_pld = OrM(arb_gnt, vlb_req_i.map(_.bits))


  //
  // b-tree

  val ptw_tog_q   = dontTouch(Wire(Bool()))
  val ptw_src_q   = dontTouch(Wire(UInt(N.W)))
  val ptw_vpn_q   = dontTouch(Wire(UInt(P.vpnBits.W)))
  val ptw_mcn_q   = dontTouch(Wire(UInt(P.mcnBits.W)))

  // TODO: this is also configurable
  val ptw_mask    = ptw_tog_q ?? 0xf.U(4.W) :: 0x7.U(4.W)

  val ptw_step    = dontTouch(Wire(Bool()))
  val ptw_succ    = dontTouch(Wire(Bool()))
  val ptw_fail    = dontTouch(Wire(Bool()))
  val ptw_done    = ptw_succ || ptw_fail

  val mem_buf_q   = RegEnable(mem_resp_i.bits.data, ptw_step && !ptw_tog_q)
  val mem_buf     = ptw_tog_q ?? (mem_resp_i.bits.data ## mem_buf_q) ::
                                 (0.U(P.clBits.W)      ## mem_resp_i.bits.data)

  val mem_nod     = mem_buf.asTypeOf(new VLBTNode(P))
  val mem_vma_vld = mem_nod.vma.map(_.vld).U & ptw_mask
  val mem_ptr_vld = NeQ(mem_nod.bot, mem_vma_vld ## true.B)

  val ptw_gt      = mem_nod.vma.map(_.gt(ptw_vpn_q)).U
  val ptw_lt      = mem_nod.vma.map(_.lt(ptw_vpn_q)).U

  // timing
  val ptw_upd     = mem_resp_i.valid && !mem_resp_i.ready

  val ptw_gt_q    = RegEnable(mem_vma_vld & ptw_gt, ptw_upd)
  val ptw_lt_q    = RegEnable(mem_vma_vld & ptw_lt, ptw_upd)
  val ptw_hit_q   = dontTouch(Wire(Vec(4, Bool())))
  val ptw_err_q   = dontTouch(Wire(Vec(4, Bool())))

  for (i <- 0 until 4) {
    ptw_hit_q(i) := RegEnable(mem_vma_vld(i) && !ptw_gt(i) && !ptw_lt(i), ptw_upd)

    // simple errors
    // 1. base       >  vpn >  bound
    // 2. base (i-1) >  vpn >= base
    // 3. bound(i-1) >= vpn >= base
    // other cases not considered are simply treated as misses
    val prv_gt = if (i == 0)  true.B else ptw_gt(i - 1)
    val prv_lt = if (i == 0) false.B else ptw_lt(i - 1)

    ptw_err_q(i) := RegEnable(mem_vma_vld(i) && (ptw_lt(i) &&  ptw_gt(i) ||
                                                !ptw_lt(i) &&  prv_lt    ||
                                                !ptw_lt(i) && !prv_gt),
                              ptw_upd)

    if (P.dbg)
      assert(ptw_step -> (mem_nod.vma(i).bound >= mem_nod.vma(i    ).base))
    if (P.dbg && (i > 0))
      assert(ptw_step -> (mem_nod.vma(i).base  >  mem_nod.vma(i - 1).bound))
  }

  // software mis-configuration
  // 1. simple errors
  // 2. gt is not 1111/0111/0011/0001/0000
  // 3. lt is not 0000/1000/1100/1110/1111
  val ptw_mis_cfg = ptw_step && (Any(ptw_err_q)                 ||
                                 Any(OrR(ptw_gt_q) & ~ptw_gt_q) ||
                                 Any(OrL(ptw_lt_q) & ~ptw_lt_q & mem_vma_vld))

  // not exact
  // the hit entry is also selected, but the mangled result is never used
  val ptw_sel     = mem_ptr_vld & (true.B ## ~ptw_gt_q) & (~ptw_lt_q ## true.B)

  val ptw_hit_any = Any(ptw_hit_q)
  val ptw_sel_any = Any(ptw_sel)

  // medium-sized mux
  val ptw_hit_mux = OrM(ptw_hit_q, mem_nod.vma)
  val ptw_sel_mux = OrM(ptw_sel,   mem_nod.ptr)

  ptw_succ  := ptw_step &&  ptw_hit_any                              && !ptw_mis_cfg
  ptw_fail  := ptw_step && !ptw_hit_any && !ptw_sel_any && ptw_tog_q ||  ptw_mis_cfg

  ptw_tog_q := RegEnable(req_vld ?? false.B :: !ptw_tog_q,
                         false.B,
                         req_vld || ptw_step)

  ptw_src_q := RegEnable(arb_gnt,     req_vld)
  ptw_vpn_q := RegEnable(req_pld.vpn, req_vld)
  ptw_mcn_q := RegEnable(req_vld   ?? satp_i      ::
                         ptw_tog_q ?? ptw_sel_mux ::
                                     (ptw_mcn_q + 1.U(P.mcnBits.W)),
                         req_vld   || ptw_step)

  // error cases among iterations. unfair to check in hardware
  if (P.dbg) {
    val ptw_min   = OrM(PrL(mem_vma_vld), mem_nod.vma.map(_.base))
    val ptw_max   = OrM(PrR(mem_vma_vld), mem_nod.vma.map(_.bound))

    val ptw_min_q = RegEnable(req_vld ??  0.U :: ptw_min, req_vld || ptw_step)
    val ptw_max_q = RegEnable(req_vld ?? ~0.U :: ptw_max, req_vld || ptw_step)

    assert((ptw_step && Any(mem_vma_vld)) ->
              ((ptw_min > ptw_min_q) &&  Any(ptw_min_q) ||
               (ptw_max < ptw_max_q) && !All(ptw_max_q)))
  }


  //
  // fsm

  // vlb wants to kill ptw
  val ptw_kill    = dontTouch(Wire(Bool()))

  val mem_fsm_en  = dontTouch(Wire(Bool()))
  val mem_fsm_q   = dontTouch(Wire(UInt(2.W)))
  val mem_fsm_nxt = dontTouch(Wire(UInt(2.W)))

  val ptw_stop    = ptw_kill || ptw_done

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
      mem_fsm_nxt := req_vld  ?? fsm_req  ::
                     ptw_stop ?? fsm_idle ::
                                 fsm_req
    }
  }

  mem_fsm_q := RegEnable(mem_fsm_nxt, fsm_idle, mem_fsm_en)

  val mem_fsm_is_idle = mem_fsm_q === fsm_idle
  val mem_fsm_is_req  = mem_fsm_q === fsm_req
  val mem_fsm_is_resp = mem_fsm_q === fsm_resp
  val mem_fsm_is_dly  = mem_fsm_q === fsm_dly

  arb_rdy  := mem_fsm_is_idle ||
              mem_fsm_is_dly  && ptw_stop

  // keep mem_req_o.valid intact
  val ptw_kill_qual = Any(ptw_src_q & vlb_req_i.map(_.bits.kill(0)).U) && !mem_fsm_is_idle

  // ptw can be killed immature
  val ptw_kill_q    = RegEnable(ptw_kill_qual && !ptw_step,
                                false.B,
                                ptw_kill_qual ||  ptw_step)

  ptw_step := mem_fsm_is_dly
  ptw_kill := ptw_kill_qual || ptw_kill_q


  //
  // output

  val vlb_resp = ptw_fail ?? VMA(P, ptw_mis_cfg) :: ptw_hit_mux

  for (i <- 0 until N) {
    vlb_req_i (i).ready := arb_gnt(i)

    vlb_resp_o(i).valid := ptw_done && ptw_src_q(i)
    vlb_resp_o(i).bits  := vlb_resp
  }

  mem_req_o.valid  := mem_fsm_is_req
  mem_req_o.bits   := MemReq(P,
                             ptw_mcn_q)

  mem_resp_i.ready := mem_fsm_is_dly

  idle_o           := mem_fsm_is_idle
}