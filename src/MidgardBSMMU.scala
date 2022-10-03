package midgard.backside

import  chisel3._
import  chisel3.util._
import  midgard._
import  midgard.util._


class MMU(val P: Param) extends Module {

  // ---------------------------
  // io

  val llc_req_i  = IO(Flipped(Decoupled(new MemReq (P, P.llcIdx))))
  val llc_resp_o = IO(        Decoupled(new MemResp(P, P.llcIdx)))

  val stb_req_i  = IO(Flipped(Decoupled(new MemReq (P, P.deqIdx))))
  val stb_resp_o = IO(        Decoupled(new MemResp(P, P.deqIdx)))

  val llc_req_o  = IO(        Decoupled(new LLCReq (P)))
  val llc_resp_i = IO(Flipped(Decoupled(new LLCResp(P))))

  val mem_req_o  = IO(        Decoupled(new MemReq (P)))
  val mem_resp_i = IO(Flipped(Decoupled(new MemResp(P))))

  val deq_busy_o = IO(           Output(UInt(P.deqWays.W)))
  val deq_head_o = IO(           Output(UInt(32.W)))

  val ctl_i      = IO(            Input(Vec (16, UInt(64.W))))
  val rst_i      = IO(            Input(UInt(2.W)))


  // ---------------------------
  // logic

  // consistency check
  // the midgard page tables are organized in the following manner (considering
  // four levels of translation):
  //   l1: ----
  //   l2: --------
  //   l3: ----------------
  //   l4: --------------------------------
  // where the base addresses of each table are specified in the `ctl_q` array.
  // within each table, ptes corresponding to contiguous pages, or contiguous
  // page table pages, etc., are contiguous placed.
  // nevertheless, since the l4 table contains ptes that cover the entire ma
  // space, there must be a set of ptes covering the l4 table itself, and a set
  // of ptes covering the set of ptes that cover the l4 table, etc., as shown
  // below (c.f. an in-cache address translation mechanism):
  //   l1:                      ----
  //   l2:                   --------
  //   l3:             ----------------
  //   l4: --------------------------------
  // where l3 is the table of ptes covering l4, and l2 the table covering l3,
  // etc.
  // indeed, these two forms of pte hierarchy should be identical, since they
  // specify the same set of backside translations. nevertheless, arbitrarily
  // setting `ctl_q` would make these two forms occupying different ma regions
  // for the same values, breaking data consistency.
  // the following checks simply ensure that these two sets of tables are
  // exactly the same.

  // only check when turning on mmu
  val mmu_on = ctl_i(0)(0)

  when (P.dbg.B && !P.bsSkip.B && mmu_on && !RegNext(mmu_on)) {
    val top = ctl_i(P.ptwLvl)(P.maBits :- 9)

    for (l <- 0 until P.ptwLvl) {
      val wid = (P.ptwLvl - l) * 9

      assert(ctl_i(l + 1)(P.maBits :- wid) ===
             Rep(top, P.ptwLvl - l)(wid := 9 - (if (l > 0) 9 else P.ptwTop)))
    }
  }

  // an extra benefit of this scheme is that once a ma can be translated, the ma
  // of each pte that is accessed during the translation can also be translated.
  // this property frees us from handling all sorts of complexity of maintaining
  // the coherence and synchronization between llc and ptc


  //
  // inst

  val u_ptc = Module(new PTC(P))
  val u_mrq = Module(new MRQ(P))
  val u_mlb = Module(new MLB(P))
  val u_ptw = Module(new PTW(P))
  val u_deq = Module(new DEQ(P))

  u_ptc.llc_req_i  <> llc_req_i
  u_ptc.llc_resp_o <> llc_resp_o
  u_ptc.ptw_req_i  <> u_ptw.ptc_req_o
  u_ptc.ptw_resp_o <> u_ptw.ptc_resp_i
  u_ptc.upd_req_i  <> u_ptw.upd_req_o
  u_ptc.mrq_req_o  <> u_mrq.ptc_req_i
  u_ptc.mrq_resp_i <> u_mrq.ptc_resp_o

  u_mrq.ptw_req_i  <> u_ptw.mrq_req_o
  u_mrq.ptw_resp_o <> u_ptw.mrq_resp_i
  u_mrq.deq_req_i  <> u_deq.mrq_req_o
  u_mrq.deq_resp_o <> u_deq.mrq_resp_i
  u_mrq.mlb_req_o  <> u_mlb.mrq_req_i
  u_mrq.mlb_resp_i <> u_mlb.mrq_resp_o
  u_mrq.mem_req_o  <> mem_req_o
  u_mrq.mem_resp_i <> mem_resp_i
  u_mrq.ctl_i      := ctl_i.slice(0, P.ptwLvl + 1)

  u_mlb.ptw_req_o  <> u_ptw.mlb_req_i
  u_mlb.ptw_resp_i <> u_ptw.mlb_resp_o
  u_mlb.rst_i      := rst_i(0)

  u_ptw.llc_req_o  <> llc_req_o
  u_ptw.llc_resp_i <> llc_resp_i
  u_ptw.ctl_i      := ctl_i.slice(0, P.ptwLvl + 1)

  u_deq.stb_req_i  <> stb_req_i
  u_deq.stb_resp_o <> stb_resp_o
  u_deq.ctl_i      := ctl_i.slice(8, 12)
  u_deq.rst_i      := rst_i(1)

  deq_busy_o       := u_deq.deq_busy_o
  deq_head_o       := u_deq.deq_head_o
}