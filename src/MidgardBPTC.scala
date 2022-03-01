package midgard.backside

import  chisel3._
import  chisel3.util._
import  chisel3.util.random._
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

  // --------------------------
  // io

  val llc_req_i  = IO(Flipped(Decoupled(new MemReq (P, P.llcIdx))))
  val llc_resp_o = IO(        Decoupled(new MemResp(P, P.llcIdx)))

  val ptw_req_i  = IO(Flipped(    Valid(new PTCRdReq(P))))
  val ptw_resp_o = IO(            Valid(new PTCEntry(P)))

  val upd_req_i  = IO(Flipped(    Valid(new PTCWrReq(P))))

  val mrq_req_o  = IO(        Decoupled(new MemReq (P, P.llcIdx)))
  val mrq_resp_i = IO(Flipped(Decoupled(new MemResp(P, P.llcIdx))))

  val clr_i      = IO(            Input(Bool()))


  // --------------------------
  // logic

  val
     (fsm_idle ::
      fsm_fwd  ::
      fsm_busy ::
      fsm_pend ::
      fsm_null) = Enum(4)

  val llc_req     = llc_req_i.fire()
  val llc_req_wnr = llc_req && !llc_req_i.bits.rnw


  //
  // arb

  // ptw always has higher priority
  val req_vld = ptw_req_i.valid || llc_req
  val req_mcn = ptw_req_i.valid ?? ptw_req_i.bits.mcn :: llc_req_i.bits.mcn


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

      // same-line either in ma/pa space
      val hit_way = ptc_q.map(_.hit(req_mcn)).U
      val hit_any = Any(hit_way)

      // llc req may collide with upd req. leave it to mrq
      val hit = req_vld &&  hit_any
      val mis = req_vld && !hit_any

      assert(req_vld -> OHp(hit_way, true.B))

      // simple pseudo-random replacement
      val rpl_vld = upd_req_i.valid && upd_req_i.bits.lvl(i)
      val rpl_q   = LFSR(log2Ceil(P.ptcWays(i)).max(2), rpl_vld)

      for (j <- 0 until P.ptcWays(i)) {
        val rpl = dontTouch(Wire(Bool()))
        val upd = llc_req_wnr &&  hit_way(j)

        if (P.ptcWays(i) <= 1)
          rpl := rpl_vld
        else
          rpl := rpl_vld && (rpl_q(log2Ceil(P.ptcWays(i)).W) === j.U)

        // respect tlb flush
        ptc_q(j).vld  := RegEnable(rpl && !clr_i,
                                   false.B,
                                   rpl ||  clr_i)
        ptc_q(j).mcn  := RegEnable(upd_req_i.bits.mcn, rpl)

        // llc write through
        ptc_q(j).data := RegEnable(rpl ?? upd_req_i.bits.data ::
                                          llc_req_i.bits.data,
                                   rpl || upd)
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

  // force the llc wr to go through
  val llc_req_hit = llc_req &&  ptc_hit && !llc_req_wnr
  val llc_req_mis = llc_req && !ptc_hit ||  llc_req_wnr


  //
  // fsm

  // combine llc fast/slow response paths
  val llc_fsm_en  = dontTouch(Wire(Bool()))
  val llc_fsm_q   = dontTouch(Wire(UInt(2.W)))
  val llc_fsm_nxt = dontTouch(Wire(UInt(2.W)))

  val llc_fwd_col = llc_resp_o.ready && llc_req_hit

  llc_fsm_en  := false.B
  llc_fsm_nxt := llc_fsm_q

  switch (llc_fsm_q) {
    is (fsm_idle) {
      llc_fsm_en  := llc_req_hit || mrq_resp_i.valid
      llc_fsm_nxt := llc_req_hit ?? fsm_fwd :: fsm_busy
    }
    // save one cycle
    is (fsm_fwd) {
      llc_fsm_en  := llc_resp_o.ready
      llc_fsm_nxt := llc_req_hit      ?? fsm_fwd  ::
                     mrq_resp_i.valid ?? fsm_busy ::
                                         fsm_idle
    }
    // decouple llc req/mem resp paths
    is (fsm_busy) {
      llc_fsm_en  := llc_resp_o.ready || llc_req_hit
      llc_fsm_nxt := llc_fwd_col      ?? fsm_fwd  ::
                     llc_resp_o.ready ?? fsm_idle ::
                                         fsm_pend
    }
    is (fsm_pend) {
      llc_fsm_en  := llc_resp_o.ready
      llc_fsm_nxt := fsm_fwd
    }
  }

  llc_fsm_q := RegEnable(llc_fsm_nxt, fsm_idle, llc_fsm_en)

  val llc_fsm_is_fwd = llc_fsm_q === fsm_fwd
  val llc_fsm_is_mem = llc_fsm_q(1)


  //
  // output

  val llc_hit_resp_q = RegEnable(MemResp(P,
                                         llc_req_i.bits.idx,
                                         false.B,
                                         llc_req_i.bits.rnw,
                                         ptc_mux.data,
                                         P.llcIdx),
                                 llc_req_hit)

  ptw_resp_o.valid := ptc_hit && ptw_req_i.valid
  ptw_resp_o.bits  := ptc_mux

  // TODO: timing. for now just pass through
  llc_req_i.ready  := mrq_req_o.ready &&
                     !ptw_req_i.valid &&
                    !(llc_fsm_is_fwd  && !llc_resp_o.ready)

  llc_resp_o.valid := Any(llc_fsm_q)
  llc_resp_o.bits  := llc_fsm_is_mem ?? mrq_resp_i.bits :: llc_hit_resp_q

  mrq_req_o.valid  := llc_req_mis
  mrq_req_o.bits   := MemReq(P,
                             llc_req_i.bits.idx,
                             llc_req_i.bits.rnw,
                             llc_req_i.bits.mcn,
                             0.U,
                             llc_req_i.bits.data,
                             P.llcIdx)

  mrq_resp_i.ready := llc_fsm_is_mem && llc_resp_o.ready
}