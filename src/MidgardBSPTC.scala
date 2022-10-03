package midgard.backside

import  chisel3._
import  chisel3.util._
import  midgard._
import  midgard.util._


class PTCRdReq(val P: Param) extends Bundle {
  val mcn  = UInt(P.mcnBits.W)
}

class PTCWrReq(val P: Param) extends Bundle {
  val lvl  = UInt(P.ptwLvl.W)
  val mcn  = UInt(P.mcnBits.W)
  val data = UInt(P.clBits.W)
}

class PTCEntry(val P: Param) extends Bundle {
  val vld  = Bool()
  val mcn  = UInt(P.mcnBits.W)
  val data = UInt(P.clBits.W)

  def hit(m: UInt): Bool = {
    vld && (mcn === m)
  }
}


object PTCRdReq {
  def apply(P: Param, m: UInt): PTCRdReq = {
    val ret = Wire(new PTCRdReq(P))

    ret.mcn  := m
    ret
  }
}

object PTCWrReq {
  def apply(P: Param, l: UInt, m: UInt, d: UInt): PTCWrReq = {
    val ret = Wire(new PTCWrReq(P))

    ret.lvl  := l
    ret.mcn  := m
    ret.data := d
    ret
  }
}


class PTC(P: Param) extends Module {

  // ---------------------------
  // io

  val llc_req_i  = IO(Flipped(Decoupled(new MemReq (P, P.llcIdx))))
  val llc_resp_o = IO(        Decoupled(new MemResp(P, P.llcIdx)))

  val ptw_req_i  = IO(Flipped(    Valid(new PTCRdReq(P))))
  val ptw_resp_o = IO(            Valid(new PTCEntry(P)))

  val upd_req_i  = IO(Flipped(    Valid(new PTCWrReq(P))))

  val mrq_req_o  = IO(        Decoupled(new MemReq (P, P.llcIdx)))
  val mrq_resp_i = IO(Flipped(Decoupled(new MemResp(P, P.llcIdx))))


  // ---------------------------
  // logic

  val
     (fsm_idle ::
      fsm_fwd  ::
      fsm_busy ::
      fsm_pend ::
      fsm_null) = Enum(4)

  val llc_req = llc_req_i.fire
  val ptw_req = ptw_req_i.valid && !P.bsSkip.B

  val mrq_req = mrq_req_o.fire


  //
  // arb

  // ptw always has higher priority
  val req_vld = ptw_req || llc_req_i.valid
  val req_mcn = ptw_req ?? ptw_req_i.bits.mcn :: llc_req_i.bits.mcn


  //
  // ptc

  val ptc_hit = dontTouch(Wire(Bool()))
  val ptc_mux = dontTouch(Wire(new PTCEntry(P)))

  if (P.ptcEn) {
    val lvl_hit = dontTouch(Wire(Vec(P.ptwLvl, Bool())))
    val lvl_mux = dontTouch(Wire(Vec(P.ptwLvl, new PTCEntry(P))))

    for (i <- 0 until P.ptwLvl) {
      // body
      val ptc_q = dontTouch(Wire(Vec(P.ptcWays(i), new PTCEntry(P))))

      // same-line in ma space
      val hit_way = ptc_q.map(_.hit(req_mcn)).U
      val hit_any = Any(hit_way)

      // llc req may collide with upd req. leave it to mrq
      val hit = req_vld &&  hit_any
      val mis = req_vld && !hit_any

      assert(req_vld -> OHp(hit_way, true.B))

      // simple pseudo-random replacement
      val upd_vld = upd_req_i.valid && upd_req_i.bits.lvl(i)

      val inv_way = ptc_q.map(!_.vld).U
      val rpl_way = Any(inv_way) ?? PrL(inv_way) ::
                                    PRA(P.ptcWays(i), upd_vld)

      for (j <- 0 until P.ptcWays(i)) {
        val upd = upd_vld && rpl_way(j)
        val wnr = llc_req && hit_way(j) && !llc_req_i.bits.rnw

        // respect mlb flush
        ptc_q(j).vld  := RegEnable(upd, false.B,       upd)
        ptc_q(j).mcn  := RegEnable(upd_req_i.bits.mcn, upd)

        // write through
        ptc_q(j).data := RegEnable(upd ?? upd_req_i.bits.data ::
                                          llc_req_i.bits.data,
                                   upd || wnr)
      }

      lvl_hit(i) := hit
      lvl_mux(i) := OrM(hit_way, ptc_q)
    }

    assert(req_vld -> OHp(lvl_hit.U, true.B))

    ptc_hit := Any(lvl_hit)
    ptc_mux := OrM(lvl_hit, lvl_mux)

  } else {
    ptc_hit := false.B
    ptc_mux := 0.U.asTypeOf(ptc_mux)
  }

  val llc_ptc_hit = llc_req && llc_req_i.bits.rnw && ptc_hit

  // single-entry buf
  val buf_set   = dontTouch(Wire(Bool()))
  val buf_clr   = dontTouch(Wire(Bool()))

  val buf_vld_q = RegEnable(buf_set, false.B, buf_set || buf_clr)
  val buf_fwd_q = RegEnable(llc_ptc_hit,      buf_set)
  val buf_q     = RegEnable(MemReq(P,
                                   llc_req_i.bits.idx,
                                   llc_req_i.bits.rnw,
                                   llc_req_i.bits.mcn,
                                   llc_req_i.bits.pcn,
                                   llc_ptc_hit ?? ptc_mux.data :: llc_req_i.bits.data,
                                   P.llcIdx),
                            buf_set)

  val buf_fwd   = buf_vld_q && buf_fwd_q
  val buf_resp  = MemResp(P,
                          buf_q.idx,
                          false.B,
                          buf_q.rnw,
                          buf_q.data,
                          P.llcIdx)


  //
  // fsm

  // combine llc fast/slow response paths
  val llc_fsm_en  = dontTouch(Wire(Bool()))
  val llc_fsm_q   = dontTouch(Wire(UInt(2.W)))
  val llc_fsm_nxt = dontTouch(Wire(UInt(2.W)))

  llc_fsm_en  := false.B
  llc_fsm_nxt := llc_fsm_q

  val llc_resp_sel_fwd = llc_resp_o.ready && buf_fwd

  switch (llc_fsm_q) {
    is (fsm_idle) {
      llc_fsm_en  := buf_fwd || mrq_resp_i.valid
      llc_fsm_nxt := buf_fwd ?? fsm_fwd :: fsm_busy
    }
    // save one cycle
    is (fsm_fwd) {
      llc_fsm_en  := llc_resp_o.ready
      llc_fsm_nxt := mrq_resp_i.valid ?? fsm_busy :: fsm_idle
    }
    // decouple llc req/mem resp paths
    is (fsm_busy) {
      llc_fsm_en  := llc_resp_o.ready || buf_fwd
      llc_fsm_nxt := llc_resp_sel_fwd ?? fsm_fwd  ::
                     llc_resp_o.ready ?? fsm_idle ::
                                         fsm_pend
    }
    is (fsm_pend) {
      llc_fsm_en  := llc_resp_o.ready
      llc_fsm_nxt := fsm_fwd
    }
  }

  llc_fsm_q := RegEnable(llc_fsm_nxt, fsm_idle, llc_fsm_en)

  val llc_fsm_is_busy = llc_fsm_q =/= fsm_idle
  val llc_fsm_is_fwd  = llc_fsm_q === fsm_fwd
  val llc_fsm_is_mem  = llc_fsm_q(1)

  buf_set := llc_req
  buf_clr := buf_vld_q &&
                (buf_fwd_q && llc_fsm_is_fwd && llc_resp_o.ready ||
                !buf_fwd_q && mrq_req_o.ready)


  //
  // output

  ptw_resp_o.valid := ptc_hit && ptw_req
  ptw_resp_o.bits  := ptc_mux

  // b2b should be rare
  llc_req_i.ready  := Non(buf_vld_q) && !ptw_req

  llc_resp_o.valid := llc_fsm_is_busy
  llc_resp_o.bits  := llc_fsm_is_mem ?? mrq_resp_i.bits :: buf_resp

  mrq_req_o.valid  := buf_vld_q && !buf_fwd_q
  mrq_req_o.bits   := MemReq(P,
                             buf_q.idx,
                             buf_q.rnw,
                             buf_q.mcn,
                             buf_q.mcn,
                             buf_q.data,
                             P.llcIdx)

  mrq_resp_i.ready := llc_fsm_is_mem && llc_resp_o.ready

  // override
  if (P.bsSkip) {
    llc_req_i.ready  := mrq_req_o.ready

    llc_resp_o.valid := mrq_resp_i.valid
    llc_resp_o.bits  := MemResp(P,
                                mrq_resp_i.bits.idx,
                                mrq_resp_i.bits.err,
                                mrq_resp_i.bits.rnw,
                                mrq_resp_i.bits.data,
                                P.llcIdx)

    mrq_req_o.valid  := llc_req_i.valid
    mrq_req_o.bits   := MemReq (P,
                                llc_req_i.bits.idx,
                                llc_req_i.bits.rnw,
                                llc_req_i.bits.mcn,
                                llc_req_i.bits.mcn,
                                llc_req_i.bits.data,
                                P.mrqIdx)

    mrq_resp_i.ready := llc_resp_o.ready

    ptw_resp_o.tie
  }
}