package midgard.backside

import  chisel3._
import  chisel3.util._
import  midgard._
import  midgard.util._


class MRQEntry(val P: Param) extends Bundle {
  val src  = UInt(2.W)
  val rnw  = Bool()
  val err  = Bool()
  val mcn  = UInt(P.mcnBits.W)
  val pcn  = UInt(P.pcnBits.W)
  val data = UInt(P.clBits.W)

  def hit(m: UInt): Bool = {
    mcn === m
  }
}


class MRQ(val P: Param) extends Module {

  // --------------------------
  // io

  val ptw_ack_o  = IO(           Output(UInt(P.mrqIdx.W)))
  val ptc_ack_o  = IO(           Output(UInt(P.mrqIdx.W)))

  val ptw_req_i  = IO(Flipped(Decoupled(new MemReq(P))))
  val ptw_resp_o = IO(        Decoupled(new MemResp(P)))

  val ptc_req_i  = IO(Flipped(Decoupled(new MemReq(P))))
  val ptc_resp_o = IO(        Decoupled(new MemResp(P)))

  val mlb_req_o  = IO(        Decoupled(new MLBReq(P)))
  val mlb_resp_i = IO(Flipped(    Valid(new MLBResp(P))))

  val mem_req_o  = IO(        Decoupled(new MemReq(P)))
  val mem_resp_i = IO(Flipped(Decoupled(new MemResp(P))))


  // --------------------------
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

  val ptw_req      = ptw_req_i.fire()
  val ptc_req      = ptc_req_i.fire()

  val mlb_req      = mlb_req_o.fire()
  val mlb_resp     = mlb_resp_i.fire()

  val mem_req      = mem_req_o.fire()
  val mem_resp     = mem_resp_i.fire()
  val mem_resp_wnr = Non(mem_resp_i.bits.rnw)


  //
  // arb

  // round robin
  val arb_rdy      = dontTouch(Wire(Bool()))
  val arb_q        = dontTouch(Wire(Bool()))

  val req_vld      = ptw_req || ptc_req
  val req_pld      = ptw_req ?? ptw_req_i.bits :: ptc_req_i.bits
  val req_src      = ptc_req ## ptw_req
  val req_wnr      = Non(req_pld.rnw)

  val req_dep      = dontTouch(Wire(Vec(P.mrqWays, Bool())))
  val req_fwd      = dontTouch(Wire(Vec(P.mrqWays, Bool())))
  val req_fwd_sel  = dontTouch(Wire(Vec(P.mrqWays, Bool())))
  val req_fwd_any  = dontTouch(Wire(Bool()))

  arb_q := RegEnable(!arb_q, false.B, req_vld)


  //
  // slots

  val raw_mlb_req  = dontTouch(Wire(Vec(P.mrqWays, Bool())))
  val raw_mem_req  = dontTouch(Wire(Vec(P.mrqWays, Bool())))

  val mrq_vld      = dontTouch(Wire(Vec(P.mrqWays, Bool())))
  val mrq_inv      = dontTouch(Wire(Vec(P.mrqWays, Bool())))
  val mrq_mlb_req  = dontTouch(Wire(Vec(P.mrqWays, Bool())))
  val mrq_mlb_resp = dontTouch(Wire(Vec(P.mrqWays, Bool())))
  val mrq_mem_req  = dontTouch(Wire(Vec(P.mrqWays, Bool())))
  val mrq_mem_sel  = dontTouch(Wire(Vec(P.mrqWays, Bool())))
  val mrq_mem_resp = dontTouch(Wire(Vec(P.mrqWays, Bool())))
  val mrq_fwd      = dontTouch(Wire(Vec(P.mrqWays, Bool())))

  val mrq_set_raw  = PrR(mrq_inv)
  val mrq_clr_raw  = Dec(mem_resp_i.bits.idx)
  val mrq_set      = EnQ(req_vld,   mrq_set_raw)
  val mrq_clr      = EnQ(mem_resp,  mrq_clr_raw)
  val mrq_set_mul  = Any(mrq_inv & ~mrq_set_raw)

  // starting state
  val mrq_set_nxt  = req_fwd_any ?? mrq_fsm_idle    ::
                     ptc_req     ?? mrq_fsm_mlb_req ::
                                    mrq_fsm_mem_req

  // body
  val mrq_q = dontTouch(Wire(Vec(P.mrqWays, new MRQEntry(P))))

  val age_q = dontTouch(Wire(Vec(P.mrqWays, Vec(P.mrqWays, Bool()))))
  val dep_q = dontTouch(Wire(Vec(P.mrqWays, Vec(P.mrqWays, Bool()))))

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
    val yts = mrq_vld & age_q(i)
    val ots = mrq_vld & age_q.map(_(i))

    mrq_q(i).rnw  := RegEnable(req_pld.rnw, mrq_set(i))
    mrq_q(i).mcn  := RegEnable(req_pld.mcn, mrq_set(i))

    // variable fields
    mrq_q(i).src  := RegEnable(mrq_set(i) ?? req_src ::
                                            (req_src | mrq_q(i).src),
                               mrq_set(i) ||
                               req_fwd(i) && mrq_vld(i) && req_pld.rnw)

    mrq_q(i).err  := RegEnable(mrq_clr(i) && mem_resp_i.bits.err,
                               mrq_set(i) ||
                               mrq_clr(i))

    mrq_q(i).pcn  := RegEnable(mrq_set(i) ?? req_pld.pcn ::
                                            (mlb_resp_i.bits.ppn ## mrq_q(i).mcn(12 - P.clWid := 0)),
                               mrq_set(i) && ptw_req ||
                               mlb_resp   && mrq_mlb_resp(i))

    mrq_q(i).data := RegEnable(mrq_set(i) ?? req_pld.data :: mem_resp_i.bits.data,
                               mrq_set(i) && req_wnr ||
                               mrq_clr(i) && mem_resp_wnr)

    // same-line in ma space
    val hit = req_vld && mrq_q(i).hit(req_pld.mcn)

    // same-line slots which are busy
    req_dep(i) := hit && mrq_vld(i)

    // same-line slots which are
    // 1. busy without dependency (youngest)
    // 3. waiting for forwarding
    dep_q(i) := RegEnable(req_dep, mrq_set(i))

    req_fwd(i) := req_dep(i) && Non(ots & dep_q.map(_(i))) ||
                  mrq_fwd(i) && hit

    // fsm
    val mrq_fsm_en  = dontTouch(Wire(Bool()))
    val mrq_fsm_q   = dontTouch(Wire(UInt(3.W)))
    val mrq_fsm_nxt = dontTouch(Wire(UInt(3.W)))

    mrq_fsm_en  := false.B
    mrq_fsm_nxt := mrq_fsm_q

    switch (mrq_fsm_q) {
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
        mrq_fsm_en  := mem_req_o.ready && mrq_mem_sel(i)
        mrq_fsm_nxt := mrq_fsm_mem_resp
      }
      is (mrq_fsm_mem_resp) {
        mrq_fsm_en  := mrq_clr(i)
        mrq_fsm_nxt := mrq_fsm_fwd
      }
      // another idle state, wait-for-forwarding (and -allocation)
      // slot stops forwarding when hit by a new same-line write. same-line read is just served by it
      is (mrq_fsm_fwd) {
        mrq_fsm_en  := mrq_set(i) || hit && req_wnr
        mrq_fsm_nxt := mrq_set(i) ?? mrq_set_nxt :: mrq_fsm_idle
      }
    }

    mrq_fsm_q := RegEnable(mrq_fsm_nxt, mrq_fsm_idle, mrq_fsm_en)

    val mrq_fsm_is_idle     = mrq_fsm_q === mrq_fsm_idle
    val mrq_fsm_is_mlb_req  = mrq_fsm_q === mrq_fsm_mlb_req
    val mrq_fsm_is_mlb_resp = mrq_fsm_q === mrq_fsm_mlb_resp
    val mrq_fsm_is_mem_req  = mrq_fsm_q === mrq_fsm_mem_req
    val mrq_fsm_is_mem_resp = mrq_fsm_q === mrq_fsm_mem_resp
    val mrq_fsm_is_fwd      = mrq_fsm_q === mrq_fsm_fwd

    raw_mlb_req (i) := mrq_fsm_is_mlb_req
    raw_mem_req (i) := mrq_fsm_is_mem_req

    mrq_vld     (i) := mrq_fsm_is_mlb_req  ||
                       mrq_fsm_is_mlb_resp ||
                       mrq_fsm_is_mem_req  ||
                       mrq_fsm_is_mem_resp
    mrq_inv     (i) := mrq_fsm_is_idle && !req_fwd_sel(i) ||
                       mrq_fsm_is_fwd  && !req_fwd_sel(i)

    // also arbitrarized
    mrq_mlb_req (i) := mrq_fsm_is_mlb_req  && Non(yts & raw_mlb_req)
    mrq_mlb_resp(i) := mrq_fsm_is_mlb_resp
    mrq_mem_req (i) := mrq_fsm_is_mem_req  && Non(yts & raw_mem_req)
    mrq_mem_resp(i) := mrq_fsm_is_mem_resp
    mrq_fwd     (i) := mrq_fsm_is_fwd
  }

  // mem req may have stability issue due to mlb
  val mrq_mem_vld_q = RegEnable(mem_req_o.valid && !mem_req_o.ready,
                                false.B,
                                mem_req_o.valid)

  val mrq_mem_req_q = RegEnable(mrq_mem_req,
                                mem_req_o.valid && !mrq_mem_vld_q)

  mrq_mem_sel := mrq_mem_vld_q ?? mrq_mem_req_q ::
                                  mrq_mem_req

  // big partial muxes
  val mrq_fwd_mux = OrM(req_fwd_sel, mrq_q)
  val mrq_mlb_mux = OrM(mrq_mlb_req, mrq_q)
  val mrq_mem_mux = OrM(mrq_mem_sel, mrq_q)
  val mrq_clr_mux = OrM(mrq_clr_raw, mrq_q)

  arb_rdy := Any(mrq_inv)

  // at most one forwarder
  // if one slot is in fwd state, then all other same-line slots must be idle
  assert(OHp(req_fwd, true.B))


  //
  // output

  val ptx         = dontTouch(Wire(Vec(2, Decoupled(new MemResp(P)))))
  val ptx_fwd_vld = dontTouch(Wire(Vec(2, Bool())))
  val ptx_mem_rdy = dontTouch(Wire(Vec(2, Bool())))

  val ptx_fwd_any = Any(ptx_fwd_vld)

  // forwarding occurs only after the slot finishes or is finishing, even for write
  req_fwd_any := Any(req_fwd & (mrq_fwd | mrq_clr)) && req_pld.rnw && !ptx_fwd_any

  // slot that is currently forwarding
  req_fwd_sel := EnQ(ptx_fwd_any, RegEnable(req_fwd, req_fwd_any))

  // TODO: implicit function not called correctly
  val fwd_idx  = Enc(req_fwd_any ?? (req_fwd | req_fwd) :: mrq_set_raw)
  val fwd_resp = MemResp(P,
                         Enc(req_fwd_sel),
                         mrq_fwd_mux.err,
                         mrq_fwd_mux.rnw,
                         mrq_fwd_mux.data)

  // 0: ptw
  // 1: llc
  for (i <- 0 until 2) {
    val rdy     = ptx(i).ready

    val fwd_vld = req_fwd_any      && req_src(i)
    val mem_vld = mem_resp_i.valid && mrq_clr_mux.src(i)

    // collision
    val fwd_rdy = fwd_vld && rdy

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
    ptx_fwd_vld(i) := ptx_fsm_is_fwd && !rdy || ptx_fsm_is_pend

    // propagate upstream ready to mem
    ptx_mem_rdy(i) := rdy && (ptx_fsm_is_mem || ptx_fsm_is_pend)

    ptx(i).valid := ptx_fsm_is_busy
    ptx(i).bits  := ptx_fsm_is_fwd ?? fwd_resp :: mem_resp_i.bits
  }

  // combine ready's from two channels
  val ptx_mem_rdy_q   = dontTouch(Wire(Vec(2, Bool())))
  val ptx_mem_rdy_byp = dontTouch(Wire(Vec(2, Bool())))

  for (i <- 0 until 2) {
    ptx_mem_rdy_q  (i) := RegEnable(ptx_mem_rdy(i) && !mem_resp_i.ready,
                                    false.B,
                                    ptx_mem_rdy(i) ||  mem_resp_i.ready)

    ptx_mem_rdy_byp(i) := ptx_mem_rdy    (i) ||
                          ptx_mem_rdy_q  (i) ||
                         !mrq_clr_mux.src(i)
  }

  // forward progress
  ptw_req_i.ready  := arb_rdy && !(ptc_req_i.valid && mrq_set_mul && arb_q)
  ptc_req_i.ready  := arb_rdy && !(ptw_req_i.valid &&               !arb_q) && mrq_set_mul

  ptw_ack_o        := fwd_idx
  ptc_ack_o        := fwd_idx

  ptw_resp_o       <> ptx(0)
  ptc_resp_o       <> ptx(1)

  mlb_req_o.valid  := Any(mrq_mlb_req)
  mlb_req_o.bits   := MLBReq(P,
                             mrq_mlb_mux.rnw,
                             mrq_mlb_mux.mcn(P.mcnBits := 12 - P.clWid))

  mem_req_o.valid  := Any(mrq_mem_req)
  mem_req_o.bits   := MemReq(P,
                             Enc(mrq_mem_sel),
                             mrq_mem_mux.rnw,
                             mrq_mem_mux.mcn,
                             mrq_mem_mux.pcn,
                             mrq_mem_mux.data)

  mem_resp_i.ready := All(ptx_mem_rdy_byp)
}