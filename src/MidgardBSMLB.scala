package midgard.backside

import  chisel3._
import  chisel3.util._
import  midgard._
import  midgard.util._


class MLBReq  (val P: Param) extends Bundle {
  val rnw  = Bool()
  val mpn  = UInt(P.mpnBits.W)
}

class MLBResp (val P: Param) extends Bundle {
  val err  = Bool()
  val ppn  = UInt(P.ppnBits.W)
  val attr = UInt(3.W)
}

class MLBEntry(val P: Param) extends Bundle {
  val vld  = Bool()
  val err  = Bool()
  val lvl  = UInt(3.W)
  val mpn  = UInt(P.mlbTagBits.W)
  val ppn  = UInt(P.ppnBits.W)
  val attr = UInt(3.W)

  def hit(m: UInt): Bool = {
    vld && (m === mpn)
  }
}


object MLBReq {
  def apply(P: Param, r: Bool, m: UInt): MLBReq = {
    val ret = Wire(new MLBReq(P))

    ret.rnw  := r
    ret.mpn  := m
    ret
  }
}

object MLBResp {
  def apply(P: Param, e: Bool, p: UInt, a: UInt): MLBResp = {
    val ret = Wire(new MLBResp(P))

    ret.err  := e
    ret.ppn  := p
    ret.attr := a
    ret
  }
}

object MLBEntry {
  def apply(P: Param): MLBEntry = {
    val ret = Wire(new MLBEntry(P))

    ret.vld  := false.B
    ret.err  := DontCare
    ret.lvl  := DontCare
    ret.mpn  := DontCare
    ret.ppn  := DontCare
    ret.attr := DontCare
    ret
  }
  def apply(P: Param, e: Bool, l: UInt, m: UInt, p: UInt, a: UInt): MLBEntry = {
    val ret = Wire(new MLBEntry(P))

    ret.vld  := true.B
    ret.err  := e
    ret.lvl  := l
    ret.mpn  := m
    ret.ppn  := p
    ret.attr := a
    ret
  }
}


class MLB(P: Param) extends Module {

  // --------------------------
  // io

  val mrq_req_i  = IO(Flipped(Decoupled(new MLBReq  (P))))
  val mrq_resp_o = IO(            Valid(new MLBResp (P)))

  val ptw_req_o  = IO(        Decoupled(new MLBReq  (P)))
  val ptw_resp_i = IO(Flipped(    Valid(new MLBEntry(P))))

  val ctl_i      = IO(            Input(Vec (P.ptwLvl + 1, UInt(P.maBits.W))))
  val rst_i      = IO(            Input(Bool()))


  // --------------------------
  // logic

  val
     (fsm_idle ::
      fsm_req  ::
      fsm_resp ::
      fsm_null) = Enum(3)

  val mmu_on  = ctl_i(0)(0)

  val mrq_req = mrq_req_i.fire()


  //
  // common

  val rst_done = dontTouch(Wire(Bool()))
  val rst_pend = dontTouch(Wire(Bool()))

  val mlb_idle = dontTouch(Wire(Bool()))
  val mlb_busy = dontTouch(Wire(Bool()))
  val mlb_data = dontTouch(Wire(new MLBEntry(P)))

  val s2_hit   = dontTouch(Wire(Bool()))
  val s2_mis   = dontTouch(Wire(Bool()))
  val s2_mpn   = dontTouch(Wire(UInt(P.mpnBits.W)))
  val s3_mpn_q = RegEnable(s2_mpn, s2_mis)


  //
  // mlb

  if (P.mlbEn) {
    val rst_q      = dontTouch(Wire(UInt((P.mlbIdx + 1).W)))
    val rst_pend_q = dontTouch(Wire(Bool()))

    // rst_i can assert at any time, even when mlb or ptw is busy
    val rst_req_nq = rst_pend_q || rst_done && rst_i
    val rst_req    = rst_req_nq && mlb_idle

    rst_pend_q := RegEnable(rst_req_nq && !mlb_idle,
                            false.B,
                            rst_req_nq)

    rst_q := RegEnable(NeQ(rst_req, rst_q + 1.U),
                       0.U,
                       rst_req || !rst_done)

    val s0_ren     = mrq_req && mmu_on
    val s1_ren_q   = RegNext(s0_ren,   false.B)
    val s2_ren_q   = RegNext(s1_ren_q, false.B)

    val s0_mpn     = mrq_req_i.bits.mpn
    val s1_mpn_q   = RegEnable(s0_mpn,   s0_ren)
    val s2_mpn_q   = RegEnable(s1_mpn_q, s1_ren_q)

    val s1_raddr   = s1_mpn_q(P.mlbIdx.W)
    val s2_rdata   = dontTouch(Wire(Vec(P.mlbWays, new MLBEntry(P))))

    val s2_set     = s2_mpn_q(P.mlbIdx.W)
    val s2_tag     = s2_mpn_q(P.mpnBits := P.mlbIdx)

    val s2_vld_way = s2_rdata.map(_.vld).U
    val s2_hit_way = s2_rdata.map(_.hit(s2_tag)).U
    val s2_hit_any = Any(s2_hit_way)

    assert(s2_ren_q -> OHp(s2_hit_way, true.B))

    // simple pseudo-random replacement
    val s2_rpl_way = PRA(P.mlbWays, s2_mis)

    rst_done := rst_q(P.mlbIdx)
    rst_pend := rst_pend_q

    mlb_busy := s1_ren_q || s2_ren_q
    mlb_data := OrM(s2_hit_way, s2_rdata)

    s2_hit   := s2_ren_q &&  s2_hit_any
    s2_mis   := s2_ren_q && !s2_hit_any
    s2_mpn   := s2_mpn_q

    val wen   = ptw_resp_i.valid && !ptw_resp_i.bits.err  || !rst_done
    val wsel  = rst_done ?? RegEnable(s2_rpl_way, s2_mis) :: ~0.U(P.mlbWays.W)
    val waddr = rst_done ?? RegEnable(s2_set,     s2_mis) ::  rst_q(P.mlbIdx.W)
    val wdata = rst_done ?? ptw_resp_i.bits               ::  MLBEntry(P)

    // single port mem model
    for (i <- 0 until P.mlbWays) {
      val mem = SyncReadMem(P.mlbSets, new MLBEntry(P))
      val upd = wen && wsel(i)

      s2_rdata(i) := DontCare

      when (upd || s1_ren_q) {
        val port = mem(upd ?? waddr :: s1_raddr)

        when (upd) {
          port        := wdata
        } .otherwise {
          s2_rdata(i) := port
        }
      }
    }

  } else {

    rst_done := true.B
    rst_pend := false.B

    mlb_busy := false.B
    mlb_data := MLBEntry(P)

    s2_hit   := false.B
    s2_mis   := mrq_req && mmu_on
    s2_mpn   := mrq_req_i.bits.mpn
  }


  //
  // fsm

  val ptw_fsm_en  = dontTouch(Wire(Bool()))
  val ptw_fsm_q   = dontTouch(Wire(UInt(2.W)))
  val ptw_fsm_nxt = dontTouch(Wire(UInt(2.W)))

  ptw_fsm_en  := false.B
  ptw_fsm_nxt := ptw_fsm_q

  switch (ptw_fsm_q) {
    is (fsm_idle) {
      ptw_fsm_en  := s2_mis
      ptw_fsm_nxt := fsm_req
    }
    is (fsm_req) {
      ptw_fsm_en  := ptw_req_o.ready
      ptw_fsm_nxt := fsm_resp
    }
    is (fsm_resp) {
      ptw_fsm_en  := ptw_resp_i.valid
      ptw_fsm_nxt := fsm_idle
    }
  }

  ptw_fsm_q := RegEnable(ptw_fsm_nxt, fsm_idle, ptw_fsm_en)

  val ptw_fsm_is_idle = ptw_fsm_q === fsm_idle
  val ptw_fsm_is_req  = ptw_fsm_q === fsm_req
  val ptw_fsm_is_resp = ptw_fsm_q === fsm_resp

  mlb_idle := rst_done && !mlb_busy && !ptw_fsm_is_req && !ptw_fsm_is_resp


  //
  // huge page & bypass

  val mlb_resp_vld = s2_hit || ptw_resp_i.valid
  val mlb_resp_mux = ptw_fsm_is_resp ?? ptw_resp_i.bits :: mlb_data

  // reconstruct the request mpn
  val mlb_resp_mpn = mlb_resp_mux.mpn ## (ptw_fsm_is_resp ?? s3_mpn_q(P.mlbIdx.W) ::
                                                             s2_mpn  (P.mlbIdx.W))

  def gen_ppn(l: Int): UInt = {
    // total number of bits to be linearly mapped
    val t = P.maBits - P.ptwTop - l * 9
    val p = t - 12

    if (t >= P.paBits)
      0.U(P.ppnBits.W)
    else if (t <= 12)
      mlb_resp_mux.ppn
    else
      // leading part from ppn plus linear part from mpn
      mlb_resp_mux.ppn(P.ppnBits := p) ## mlb_resp_mpn(p.W)
  }

  val mlb_resp_ppn = OrM(Dec(mlb_resp_mux.lvl)(P.ptwLvl.W),
                         Seq.tabulate(P.ptwLvl)(gen_ppn))

  val mlb_resp     = MLBResp(P,
                             mlb_resp_mux.err,
                             mlb_resp_ppn,
                             mlb_resp_mux.attr)

  // mmu not enabled
  val byp_resp_vld = RegNext(mrq_req && !mmu_on)
  val byp_resp     = MLBResp(P,
                             false.B,
                             RegNext(mrq_req_i.bits.mpn),
                             7.U)


  //
  // output

  mrq_req_i.ready  := mlb_idle && !rst_pend && !rst_i

  mrq_resp_o.valid := mmu_on ?? mlb_resp_vld :: byp_resp_vld
  mrq_resp_o.bits  := mmu_on ?? mlb_resp     :: byp_resp

  ptw_req_o.valid  := ptw_fsm_is_req
  ptw_req_o.bits   := MLBReq(P,
                             true.B,
                             s3_mpn_q)
}