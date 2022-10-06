package midgard.backside

import  chisel3._
import  chisel3.util._
import  midgard._
import  midgard.util._


class MRQEntry(val P: Param) extends Bundle {
  val idx  = UInt(P.llcIdx.W)
  val fsm  = UInt(3.W)
  val rnw  = Bool()
  val err  = Bool()
  val src  = UInt(3.W)
  val mcn  = UInt(P.mcnBits.W)
  val pcn  = UInt(P.pcnBits.W)
  val dep  = UInt(P.mrqWays.W)
  val data = UInt(P.clBits.W)

  def mcn_hit(m: UInt): Bool = {
    mcn === m
  }
  def idx_hit(i: UInt, s: UInt): Bool = {
   (idx === i) && Any(src & s)
  }
}


class MRQ(val P: Param) extends Module {

  // ---------------------------
  // io

  val ptw_req_i  = IO(Flipped(Decoupled(new MemReq (P, P.llcIdx))))
  val ptw_resp_o = IO(        Decoupled(new MemResp(P, P.llcIdx)))

  val deq_req_i  = IO(Flipped(Decoupled(new MemReq (P, P.llcIdx))))
  val deq_resp_o = IO(        Decoupled(new MemResp(P, P.llcIdx)))

  val ptc_req_i  = IO(Flipped(Decoupled(new MemReq (P, P.llcIdx))))
  val ptc_resp_o = IO(        Decoupled(new MemResp(P, P.llcIdx)))

  val mlb_req_o  = IO(        Decoupled(new MLBReq (P)))
  val mlb_resp_i = IO(Flipped(    Valid(new MLBResp(P))))

  val mem_req_o  = IO(        Decoupled(new MemReq (P)))
  val mem_resp_i = IO(Flipped(Decoupled(new MemResp(P))))

  val ctl_i      = IO(            Input(Vec (P.ptwLvl + 1, UInt(P.maBits.W))))


  // ---------------------------
  // logic

  val
     (mrq_fsm_idle     ::
      mrq_fsm_mlb_req  ::
      mrq_fsm_mlb_resp ::
      mrq_fsm_mem_req  ::
      mrq_fsm_mem_resp ::
      mrq_fsm_fwd      ::
      mrq_fsm_null) = Enum(6)

  val
     (ptx_fsm_idle     ::
      ptx_fsm_fwd      ::
      ptx_fsm_mem      ::
      ptx_fsm_pend     ::
      ptx_fsm_null) = Enum(4)

  val mmu_on       = ctl_i(0)(0)

  val ptw_req      = ptw_req_i.fire & !P.bsSkip.B
  val deq_req      = deq_req_i.fire & !P.bsSkip.B
  val ptc_req      = ptc_req_i.fire & !P.bsSkip.B

  val mlb_req      = mlb_req_o.fire
  val mlb_resp     = mlb_resp_i.fire

  // combination of real and fake reqs/resps
  val mem_req_vld  = dontTouch(Wire(Bool()))
  val mem_req_rdy  = dontTouch(Wire(Bool()))
  val mem_resp_vld = dontTouch(Wire(Bool()))
  val mem_resp_rdy = dontTouch(Wire(Bool()))
  val mem_resp_pld = dontTouch(Wire(new MemResp(P, P.llcIdx)))

  val mem_req      = mem_req_vld  && mem_req_rdy
  val mem_resp     = mem_resp_vld && mem_resp_rdy


  //
  // arb

  val arb_rdy      = dontTouch(Wire(Bool()))

  val req_vld_raw  = NeQ(P.bsSkip.B,
                         ptc_req_i.valid ##
                         deq_req_i.valid ##
                         ptw_req_i.valid)
  val req_vld_any  = Any(req_vld_raw) && arb_rdy

  // round robin
  val arb_sel      = RRA(req_vld_raw, req_vld_any)

  val req_vld      = ptc_req || deq_req || ptw_req
  val req_src      = ptc_req ## deq_req ## ptw_req
  val req_pld      = OrM(arb_sel,
                         Seq(ptw_req_i.bits,
                             deq_req_i.bits,
                             ptc_req_i.bits))
  val req_wnr      = Non(req_pld.rnw)

  val req_dep      = dontTouch(Wire(Vec (P.mrqWays, Bool())))
  val req_ser      = dontTouch(Wire(Vec (P.mrqWays, Bool())))
  val req_fwd      = dontTouch(Wire(Vec (P.mrqWays, Bool())))
  val req_fwd_sel  = dontTouch(Wire(Vec (P.mrqWays, Bool())))
  val req_fwd_rdy  = dontTouch(Wire(Bool()))
  val req_fwd_imp  = dontTouch(Wire(Bool()))
  val req_fwd_exp  = dontTouch(Wire(Bool()))


  //
  // slots

  val raw_mlb_req  = dontTouch(Wire(Vec (P.mrqWays, Bool())))
  val raw_mem_req  = dontTouch(Wire(Vec (P.mrqWays, Bool())))

  val mrq_req      = dontTouch(Wire(Vec (P.mrqWays, Bool())))
  val mrq_vld      = dontTouch(Wire(Vec (P.mrqWays, Bool())))
  val mrq_inv      = dontTouch(Wire(Vec (P.mrqWays, Bool())))
  val mrq_mlb_req  = dontTouch(Wire(Vec (P.mrqWays, Bool())))
  val mrq_mlb_resp = dontTouch(Wire(Vec (P.mrqWays, Bool())))
  val mrq_mem_req  = dontTouch(Wire(Vec (P.mrqWays, Bool())))
  val mrq_fwd      = dontTouch(Wire(Vec (P.mrqWays, Bool())))

  val mrq_set_raw  = dontTouch(Wire(UInt(P.mrqWays.W)))
  val mrq_clr_raw  = dontTouch(Wire(UInt(P.mrqWays.W)))

  val mrq_set      = EnQ(req_vld,     mrq_set_raw)
  val mrq_clr      = EnQ(mem_resp,    mrq_clr_raw)
  val mrq_set_mul  = Any(mrq_inv.U & ~mrq_set_raw)

  // starting state
  val ptc_mlb_req  = ptc_req && mmu_on

  val mrq_set_nxt  = req_fwd_imp ?? mrq_fsm_idle    ::
                     ptc_mlb_req ?? mrq_fsm_mlb_req ::
                                    mrq_fsm_mem_req

  // body
  val mrq_q = dontTouch(Wire(Vec(P.mrqWays, new MRQEntry(P))))
  val age_q = dontTouch(Wire(Vec(P.mrqWays, Vec(P.mrqWays, Bool()))))

  for (i <- 0 until P.mrqWays) {
    // age matrix
    for (j <- 0 until P.mrqWays) {
      if (i == j)
        age_q(i)(j) := false.B
      else if (i > j)
        age_q(i)(j) := Non(age_q(j)(i))
      else
        age_q(i)(j) := RegEnable(mrq_set(i),
                                 false.B,
                                 mrq_set(i) || mrq_set(j))
    }

    // current slot is younger/older than
    val yts = mrq_vld.U & age_q(i).U
    val ots = mrq_vld.U & age_q.map(_(i)).U

    mrq_q(i).rnw  := RegEnable(req_pld.rnw, mrq_set(i))
    mrq_q(i).mcn  := RegEnable(req_pld.mcn, mrq_set(i))
    mrq_q(i).idx  := RegEnable(req_pld.idx, mrq_set(i))
    mrq_q(i).dep  := RegEnable(req_dep.U,   mrq_set(i))

    // variable fields
    mrq_q(i).src  := RegEnable(EnQ(mrq_set(i) ||  req_fwd(i), req_src) |
                               EnQ(mrq_vld(i) && !mrq_clr(i), mrq_q(i).src),
                               mrq_set(i) ||
                               mrq_clr(i) ||
                               req_fwd(i))

    // page fault
    val mlb_err = mrq_mlb_resp(i) && mlb_resp_i.valid &&
                     (mlb_resp_i.bits.err     ||
                    !(mlb_resp_i.bits.attr(0) || mrq_q(i).rnw))

    mrq_q(i).err  := RegEnable(mlb_err    ||
                               mrq_clr(i) && mem_resp_pld.err,
                               false.B,
                               mrq_set(i) ||
                               mlb_err    ||
                               mrq_clr(i))

    mrq_q(i).pcn  := RegEnable(mrq_set(i) ?? req_pld.pcn ::
                                            (mlb_resp_i.bits.ppn ## mrq_q(i).mcn(12 - P.clWid := 0)),
                               mrq_set(i) ||
                               mlb_resp   && mrq_mlb_resp(i))

    mrq_q(i).data := RegEnable(mrq_set(i) ?? req_pld.data :: mem_resp_pld.data,
                               mrq_set(i) && req_wnr ||
                               mrq_clr(i) && mem_resp_pld.rnw)

    // same-line in ma space
    val mcn_hit = req_vld && mrq_q(i).mcn_hit(req_pld.mcn)
    val idx_hit = req_vld && mrq_q(i).idx_hit(req_pld.idx, req_src)

    // same-line slots which are busy
    req_dep(i) := mcn_hit && mrq_vld(i)
    req_ser(i) := idx_hit && mrq_vld(i)

    // same-line slots which are
    // 1. busy without dependency (youngest)
    // 2. waiting for forwarding
    req_fwd(i) := req_pld.rnw && !mrq_q(i).src(1) &&
                     (req_dep(i) && Non(ots & (mrq_q.map(_.dep(i)).U | req_ser.U)) ||
                      mrq_fwd(i) && mcn_hit)

    // explicit forward may not be possible
    val fwd_rdy = req_fwd(i) && req_fwd_rdy ||
                 !req_fwd(i)

    val hit_wnr = mcn_hit && req_wnr

    // fsm
    val mrq_fsm_en  = dontTouch(Wire(Bool()))
    val mrq_fsm_nxt = dontTouch(Wire(UInt(3.W)))

    mrq_fsm_en  := false.B
    mrq_fsm_nxt := mrq_q(i).fsm

    switch (mrq_q(i).fsm) {
      is (mrq_fsm_idle) {
        mrq_fsm_en  := mrq_set(i)
        mrq_fsm_nxt := mrq_set_nxt
      }
      is (mrq_fsm_mlb_req) {
        mrq_fsm_en  := mlb_req_o.ready && mrq_mlb_req(i)
        mrq_fsm_nxt := mrq_fsm_mlb_resp
      }
      is (mrq_fsm_mlb_resp) {
        mrq_fsm_en  := mlb_resp_i.valid
        mrq_fsm_nxt := mrq_fsm_mem_req
      }
      is (mrq_fsm_mem_req) {
        mrq_fsm_en  := mem_req_rdy && mrq_mem_req(i)
        mrq_fsm_nxt := mrq_fsm_mem_resp
      }
      is (mrq_fsm_mem_resp) {
        mrq_fsm_en  := mrq_clr(i)
        mrq_fsm_nxt := fwd_rdy     ?? mrq_fsm_fwd  :: mrq_fsm_idle
      }
      // another idle state, wait-for-forwarding (and -allocation)
      // slot stops forwarding when hit by a new same-line write. same-line read is just served by it
      is (mrq_fsm_fwd) {
        mrq_fsm_en  := mrq_set(i)  || mcn_hit
        mrq_fsm_nxt := mrq_set(i)  ?? mrq_set_nxt  ::
                       hit_wnr     ?? mrq_fsm_idle ::
                       req_fwd_rdy ?? mrq_fsm_fwd  ::
                                      mrq_fsm_idle
      }
    }

    // make life easier
    mrq_q(i).fsm := RegEnable(mrq_fsm_nxt, mrq_fsm_idle, mrq_fsm_en)

    val mrq_fsm_is_idle     = mrq_q(i).fsm === mrq_fsm_idle
    val mrq_fsm_is_mlb_req  = mrq_q(i).fsm === mrq_fsm_mlb_req
    val mrq_fsm_is_mlb_resp = mrq_q(i).fsm === mrq_fsm_mlb_resp
    val mrq_fsm_is_mem_req  = mrq_q(i).fsm === mrq_fsm_mem_req
    val mrq_fsm_is_mem_resp = mrq_q(i).fsm === mrq_fsm_mem_resp
    val mrq_fsm_is_fwd      = mrq_q(i).fsm === mrq_fsm_fwd

    raw_mlb_req (i) := mrq_fsm_is_mlb_req
    raw_mem_req (i) := mrq_fsm_is_mem_req

    mrq_req     (i) := mrq_fsm_is_mlb_req  ||
                       mrq_fsm_is_mlb_resp ||
                       mrq_fsm_is_mem_req
    mrq_vld     (i) := mrq_req(i)          ||
                       mrq_fsm_is_mem_resp

    mrq_inv     (i) := mrq_fsm_is_idle && !req_fwd_sel(i) ||
                       mrq_fsm_is_fwd  && !req_fwd_sel(i)

    // also arbitrarized
    mrq_mlb_req (i) := mrq_fsm_is_mlb_req  && Non(yts & raw_mlb_req.U)
    mrq_mlb_resp(i) := mrq_fsm_is_mlb_resp
    mrq_mem_req (i) := mrq_fsm_is_mem_req  && Non(yts & mrq_req.U)
    mrq_fwd     (i) := mrq_fsm_is_fwd
  }

  // big partial muxes
  val mrq_fwd_mux = OrM(req_fwd_sel, mrq_q)
  val mrq_mlb_mux = OrM(mrq_mlb_req, mrq_q)
  val mrq_mem_mux = OrM(mrq_mem_req, mrq_q)
  val mrq_clr_mux = OrM(mrq_clr_raw, mrq_q)

  // fake mem req/resp for faulty access
  val mem_err_req        = mem_req && mrq_mem_mux.err
  val mem_err_resp_vld_q = dontTouch(Wire(Bool()))

  mem_err_resp_vld_q := RegEnable(mem_err_req,
                                  false.B,
                                  mem_err_req        ||
                                  mem_err_resp_vld_q && mem_resp_rdy)

  // respect outstanding mem resp (no matter real or fake)
  mem_req_vld  := Any(raw_mem_req)
  mem_req_rdy  := mrq_mem_mux.err ?? !(mem_resp_vld && !mem_resp_rdy) ::
                                       mem_req_o.ready

  // once inserted, fake mem resp has higher priority
  mem_resp_vld := mem_err_resp_vld_q || mem_resp_i.valid
  mem_resp_pld := MemResp(P,
                          mrq_clr_mux.idx,
                          mem_err_resp_vld_q        || mem_resp_i.bits.err,
                          mrq_clr_mux.rnw,
                          mem_err_resp_vld_q ?? 0.U :: mem_resp_i.bits.data,
                          P.llcIdx)

  // also mix in fake mem resp
  val mem_req_err_q = RegEnable(mrq_mem_req.U, mem_err_req)

  mrq_set_raw := PrR(mrq_inv.U)
  mrq_clr_raw := mem_err_resp_vld_q ?? mem_req_err_q :: Dec(mem_resp_i.bits.idx)

  // at least ptw can issue reqs
  arb_rdy := Any(mrq_inv)

  // at most one forwarder
  // if one slot is in fwd state, then all other same-line slots must be idle
  assert(OHp(req_fwd.U, true.B))

  // no support for issuing multiple reqs with the same ma
  assert(Non(OrM(req_dep, mrq_q.map(_.src)) & req_src))


  //
  // output

  val ptx             = dontTouch(Wire(Vec(3, Decoupled(new MemResp(P, P.llcIdx)))))
  val ptx_fwd_vld     = dontTouch(Wire(Vec(3, Bool())))
  val ptx_mem_rdy     = dontTouch(Wire(Vec(3, Bool())))

  val ptx_fwd_any     = Any(ptx_fwd_vld)

  val ptx_mem_rdy_q   = dontTouch(Wire(Vec(3, Bool())))
  val ptx_mem_rdy_byp = dontTouch(Wire(Vec(3, Bool())))

  // explicit forwarding occurs only after the slot finishes or is finishing, even for write
  req_fwd_rdy := Non(ptx_fwd_vld)
  req_fwd_imp := Any(req_fwd.U                        ) && req_fwd_rdy
  req_fwd_exp := Any(req_fwd.U & (mrq_fwd.U | mrq_clr)) && req_fwd_rdy

  // slot that is currently forwarding
  req_fwd_sel := NeQ(req_fwd_rdy, RegEnable(req_fwd, req_fwd_exp))

  // 0: ptw
  // 1: deq
  // 2: ptc
  for (i <- 0 until 3) {
    val rdy = ptx(i).ready

    val fwd_vld   = req_fwd_exp  && req_src(i)
    val mem_vld   = mem_resp_vld && mrq_clr_mux.src(i) && !ptx_mem_rdy_q(i)

    // collision
    val fwd_rdy   = fwd_vld && rdy

    val fwd_idx_q = RegEnable(req_pld.idx, fwd_vld)
    val fwd_resp  = MemResp(P,
                            fwd_idx_q,
                            mrq_fwd_mux.err,
                            true.B,
                            mrq_fwd_mux.data,
                            P.llcIdx)

    // fsm
    val ptx_fsm_en  = dontTouch(Wire(Bool()))
    val ptx_fsm_q   = dontTouch(Wire(UInt(2.W)))
    val ptx_fsm_nxt = dontTouch(Wire(UInt(2.W)))

    ptx_fsm_en  := false.B
    ptx_fsm_nxt := ptx_fsm_q

    switch (ptx_fsm_q) {
      is (ptx_fsm_idle) {
        ptx_fsm_en  := fwd_vld || mem_vld
        ptx_fsm_nxt := fwd_vld ?? ptx_fsm_fwd :: ptx_fsm_mem
      }
      // save one cycle
      is (ptx_fsm_fwd) {
        ptx_fsm_en  := rdy
        ptx_fsm_nxt := mem_vld ?? ptx_fsm_mem :: ptx_fsm_idle
      }
      // go to the pending state once a new forwarding is made while the mem
      // response is still outstanding
      is (ptx_fsm_mem) {
        ptx_fsm_en  := fwd_vld || rdy
        ptx_fsm_nxt := fwd_rdy ?? ptx_fsm_fwd  ::
                       fwd_vld ?? ptx_fsm_pend ::
                                  ptx_fsm_idle
      }
      is (ptx_fsm_pend) {
        ptx_fsm_en  := rdy
        ptx_fsm_nxt := ptx_fsm_fwd
      }
    }

    ptx_fsm_q := RegEnable(ptx_fsm_nxt, ptx_fsm_idle, ptx_fsm_en)

    val ptx_fsm_is_busy = ptx_fsm_q =/= ptx_fsm_idle
    val ptx_fsm_is_fwd  = ptx_fsm_q === ptx_fsm_fwd
    val ptx_fsm_is_mem  = ptx_fsm_q === ptx_fsm_mem
    val ptx_fsm_is_pend = ptx_fsm_q === ptx_fsm_pend

    // keep the forwarding slot intact
    ptx_fwd_vld    (i) := ptx_fsm_is_fwd || ptx_fsm_is_pend

    // propagate upstream ready to mem
    ptx_mem_rdy    (i) := rdy && (ptx_fsm_is_mem || ptx_fsm_is_pend)

    // combine ready's from two channels
    ptx_mem_rdy_q  (i) := RegEnable(ptx_mem_rdy(i) && !mem_resp_rdy,
                                    false.B,
                                    ptx_mem_rdy(i) ||  mem_resp_rdy)

    ptx_mem_rdy_byp(i) := ptx_mem_rdy    (i) ||
                          ptx_mem_rdy_q  (i) ||
                         !mrq_clr_mux.src(i)

    ptx(i).valid := ptx_fsm_is_busy
    ptx(i).bits  := ptx_fsm_is_fwd ?? fwd_resp :: mem_resp_pld
  }

  // consensus
  mem_resp_rdy := All(ptx_mem_rdy_byp)

  // forward progress
  ptw_req_i.ready  := arb_rdy && arb_sel(0)
  deq_req_i.ready  := arb_rdy && arb_sel(1)
  ptc_req_i.ready  := arb_rdy && arb_sel(2) && mrq_set_mul

  ptw_resp_o       <> ptx(0)
  deq_resp_o       <> ptx(1)
  ptc_resp_o       <> ptx(2)

  mlb_req_o.valid  := Any(raw_mlb_req)
  mlb_req_o.bits   := MLBReq(P,
                             mrq_mlb_mux.rnw,
                             mrq_mlb_mux.mcn(P.mcnBits := 12 - P.clWid))

  mem_req_o.valid  := mem_req_vld && !mrq_mem_mux.err
  mem_req_o.bits   := MemReq(P,
                             Enc(mrq_mem_req),
                             mrq_mem_mux.rnw,
                             mrq_mem_mux.mcn,
                             mrq_mem_mux.pcn,
                             mrq_mem_mux.data)

  mem_resp_i.ready := mem_resp_rdy && !mem_err_resp_vld_q

  // override
  if (P.bsSkip) {
    val mem_dec_req  = Dec(mem_req_o.bits.idx)
    val mem_dec_resp = Dec(mem_resp_i.bits.idx)

    val mem_resp_sel_deq = Seq.tabulate(P.deqWays) { i =>
      Src(mem_req_o.fire  && mem_dec_req (i),
          mem_resp_i.fire && mem_dec_resp(i),
          deq_req_i.valid, 3)
    }

    // priority: deq > llc
    deq_req_i.ready  := mem_req_o.ready
    ptc_req_i.ready  := deq_req_i.ready && !deq_req_i.valid

    mem_req_o.valid  := deq_req_i.valid ||  ptc_req_i.valid
    mem_req_o.bits   := deq_req_i.valid ??
                            MemReq(P,
                                   deq_req_i.bits.idx,
                                   false.B,
                                   0.U,
                                   deq_req_i.bits.pcn,
                                   deq_req_i.bits.data,
                                   P.mrqIdx) ::
                            MemReq(P,
                                   ptc_req_i.bits.idx,
                                   ptc_req_i.bits.rnw,
                                   0.U,
                                   ptc_req_i.bits.mcn,
                                   ptc_req_i.bits.data,
                                   P.mrqIdx)

    val sel_deq = Any(mem_dec_resp(P.deqWays.W) & mem_resp_sel_deq.U)

    deq_resp_o.valid := mem_resp_i.valid &&  sel_deq
    deq_resp_o.bits  := mem_resp_i.bits

    ptc_resp_o.valid := mem_resp_i.valid && !sel_deq
    ptc_resp_o.bits  := mem_resp_i.bits

    mem_resp_i.ready := sel_deq && deq_resp_o.ready ||
                       !sel_deq && ptc_resp_o.ready

    ptw_req_i .tie
    ptw_resp_o.tie

    mlb_req_o .tie
  }
}
