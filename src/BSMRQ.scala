package midgard.backside

import  chisel3._
import  chisel3.util._
import  midgard._
import  midgard.util._


class MemReq(val P: Param, val W: Int = 0) extends Bundle {
  val w    = if (W <= 0) P.mrqIdx else W

  val idx  = UInt(w.W)
  val wnr  = Bool()
  val mcn  = UInt(P.mcnBits.W)
  val pcn  = UInt(P.pcnBits.W)
  val data = UInt(P.clBits.W)
}

class MemRes(val P: Param, val W: Int = 0) extends Bundle {
  val w    = if (W <= 0) P.mrqIdx else W

  val idx  = UInt(w.W)
  val err  = Bool()
  val wnr  = Bool()
  val data = UInt(P.clBits.W)
}

class MRQEntry(val P: Param) extends Bundle {
  val idx  = UInt(P.llcIdx.W)
  val fsm  = UInt(3.W)
  val wnr  = Bool()
  val err  = Bool()
  val mpn  = UInt(P.mpnBits.W)
  val ppn  = UInt(P.ppnBits.W)
  val xcn  = UInt(6.W)
}


object MemReq {
  def apply(P: Param, i: UInt, w: Bool, m: UInt, p: UInt, d: UInt, n: Int = 0): MemReq = {
    val ret = Pin(new MemReq(P, n))

    ret.idx  := i
    ret.wnr  := w
    ret.mcn  := m
    ret.pcn  := p
    ret.data := d
    ret
  }
}

object MemRes {
  def apply(P: Param, i: UInt, e: Bool, w: Bool, d: UInt, n: Int = 0): MemRes = {
    val ret = Pin(new MemRes(P, n))

    ret.idx  := i
    ret.err  := e
    ret.wnr  := w
    ret.data := d
    ret
  }
}


class MRQ(val P: Param) extends Module {

  // ---------------------------
  // io

  val llc_req_i = IO(Flipped(Decoupled(new MemReq(P, P.llcIdx))))
  val llc_res_o = IO(        Decoupled(new MemRes(P, P.llcIdx)))

  val ptw_req_i = IO(Flipped(Decoupled(new MemReq(P))))
  val ptw_res_o = IO(        Decoupled(new MemRes(P)))

  val mlb_req_o = IO(            Valid(new MLBReq(P)))
  val mlb_res_i = IO(Flipped(    Valid(new MLBRes(P))))
  val ptw_res_i = IO(Flipped(    Valid(new MLBRes(P))))

  val ptc_req_o = IO(        Decoupled(new PTCReq(P)))
  val ptc_res_i = IO(            Input(new PTCRes(P)))

  val mem_req_o = IO(        Decoupled(new MemReq(P)))
  val mem_res_i = IO(Flipped(Decoupled(new MemRes(P))))

  val ctl_i     = IO(            Input(Vec (P.ptwLvl + 1, UInt(P.maBits.W))))


  // ---------------------------
  // logic

  // the last idx is reserved to ptw
  val M = P.mrqWays - 1

  val
     (mrq_fsm_idle    ::
      mrq_fsm_mlb_req ::
      mrq_fsm_mlb_res ::
      mrq_fsm_ptw_res ::
      mrq_fsm_ptw_dly ::
      mrq_fsm_pre_req ::
      mrq_fsm_mem_req ::
      mrq_fsm_mem_res ::
      mrq_fsm_null) = Enum(8)

  val
     (mem_fsm_idle    ::
      mem_fsm_req     ::
      mem_fsm_res     ::
      mem_fsm_null) = Enum(4)

  val mmu_on      = ctl_i(0)(0)

  val llc_req     = llc_req_i.fire
  val llc_req_pld = llc_req_i.bits
  val llc_req_w   = llc_req &&  llc_req_pld.wnr

  val ptw_req     = ptw_req_i.fire
  val ptw_req_pld = ptw_req_i.bits

  val mlb_res     = mlb_res_i.valid
  val mlb_res_pld = mlb_res_i.bits
  val mlb_res_err = mlb_res &&  mlb_res_pld.err

  val ptw_res     = ptw_res_i.valid
  val ptw_res_pld = ptw_res_i.bits
  val ptw_res_err = ptw_res &&  ptw_res_pld.err

  val ptc_res     = ptc_res_i.hit && !ptc_req_o.bits.wnr
  val ptc_res_pld = ptc_res_i

  val mem_res     = mem_res_i.fire
  val mem_res_pld = mem_res_i.bits

  val mem_res_max = mem_res_pld.idx === M.U
  val mem_res_ptw = mem_res &&  mem_res_max
  val mem_res_llc = mem_res && !mem_res_max


  //
  // mrq

  val mrq_vld         = Pin(Vec (M, Bool()))
  val mrq_mlb_req     = Pin(Vec (M, Bool()))
  val mrq_pre_req     = Pin(Vec (M, Bool()))
  val mrq_mlb_req_sel = Pin(UInt(M.W))
  val mrq_pre_req_sel = Pin(UInt(M.W))
  val mrq_mem_req_sel = Pin(UInt(M.W))
  val mrq_pre_req_rdy = Pin(Bool())
  val mrq_mem_req_rdy = Pin(Bool())
  val mrq_mem_req_fwd = Pin(Bool())

  val mrq_inv         = Neg(mrq_vld)
  val mrq_set_raw     = PrR(mrq_inv)
  val mrq_clr_raw     = Dec(mem_res_pld.idx)
  val mrq_set         = EnQ(llc_req, mrq_set_raw)

  // common next state
  val mrq_fsm_nxt_mlb = mlb_res         ?? mrq_fsm_pre_req ::
                        mlb_res_pld.rdy ?? mrq_fsm_ptw_res ::
                                           mrq_fsm_ptw_dly

  // body
  val mrq_q = Pin(Vec(M, new MRQEntry(P)))

  for (i <- 0 until M) {
    val set = mrq_set(i)

    mrq_q(i).wnr := RegEnable(llc_req_pld.wnr,                       set)
    mrq_q(i).idx := RegEnable(llc_req_pld.idx,                       set)
    mrq_q(i).mpn := RegEnable(llc_req_pld.mcn(P.mcnBits := P.clWid), set)
    mrq_q(i).xcn := RegEnable(llc_req_pld.mcn(P.clWid.W),            set)

    val mlb = Pin(Bool())
    val ptw = Pin(Bool())
    val mem = Pin(Bool())

    mrq_q(i).err := RegEnable(mlb && mlb_res_err ||
                              ptw && ptw_res_err,
                              false.B,
                              set ||
                              mlb ||
                              ptw)

    mrq_q(i).ppn := RegEnable(OrM(Seq(set,
                                      mlb,
                                      ptw),
                                  Seq(llc_req_pld.pcn(P.pcnBits := P.clWid),
                                      mlb_res_pld.ppn,
                                      ptw_res_pld.ppn)),
                              set ||
                              mlb ||
                              ptw)

    // fsm
    val fsm_en  = Pin(Bool())
    val fsm_nxt = Pin(UInt(3.W))

    val mlb_rdy = mrq_mlb_req_sel(i)
    val pre_rdy = mrq_pre_req_sel(i) && mrq_pre_req_rdy
    val mem_rdy = mrq_mem_req_sel(i) && mrq_mem_req_rdy
    val mem_fwd = mrq_mem_req_fwd    || mrq_q(i).err

    fsm_en  := false.B
    fsm_nxt := mrq_q(i).fsm

    switch (mrq_q(i).fsm) {
      is (mrq_fsm_idle) {
        fsm_en  := set
        fsm_nxt := mrq_fsm_mlb_req
      }
      is (mrq_fsm_mlb_req) {
        fsm_en  := mlb_rdy
        fsm_nxt := mrq_fsm_mlb_res
      }
      is (mrq_fsm_mlb_res) {
        fsm_en  := true.B
        fsm_nxt := mrq_fsm_nxt_mlb
      }
      is (mrq_fsm_ptw_res) {
        fsm_en  := ptw
        fsm_nxt := mrq_fsm_pre_req
      }
      is (mrq_fsm_ptw_dly) {
        fsm_en  := ptw_res
        fsm_nxt := mrq_fsm_mlb_req
      }
      is (mrq_fsm_pre_req) {
        fsm_en  := pre_rdy
        fsm_nxt := mrq_fsm_mem_req
      }
      is (mrq_fsm_mem_req) {
        fsm_en  := mem_rdy
        fsm_nxt := mem_fwd ?? mrq_fsm_idle :: mrq_fsm_mem_res
      }
      is (mrq_fsm_mem_res) {
        fsm_en  := mem
        fsm_nxt := mrq_fsm_idle
      }
    }

    mrq_q(i).fsm := RegEnable(fsm_nxt, mrq_fsm_idle, fsm_en)

    val fsm_is_busy    = mrq_q(i).fsm =/= mrq_fsm_idle
    val fsm_is_mlb_req = mrq_q(i).fsm === mrq_fsm_mlb_req
    val fsm_is_mlb_res = mrq_q(i).fsm === mrq_fsm_mlb_res
    val fsm_is_ptw_res = mrq_q(i).fsm === mrq_fsm_ptw_res
    val fsm_is_pre_req = mrq_q(i).fsm === mrq_fsm_pre_req
    val fsm_is_mem_res = mrq_q(i).fsm === mrq_fsm_mem_res

    mrq_vld    (i) := fsm_is_busy
    mrq_mlb_req(i) := fsm_is_mlb_req
    mrq_pre_req(i) := fsm_is_pre_req

    mlb := fsm_is_mlb_res && mlb_res
    ptw := fsm_is_ptw_res && ptw_res
    mem := fsm_is_mem_res && mem_res
  }

  val mrq_mlb_req_any = Any(mrq_mlb_req.U)
  val mrq_pre_req_any = Any(mrq_pre_req.U)

  // big partial muxes
  val mrq_mlb_req_mux = OrM(mrq_mlb_req_sel, mrq_q)
  val mrq_pre_req_mux = OrM(mrq_pre_req_sel, mrq_q)
  val mrq_mem_req_mux = OrM(mrq_mem_req_sel, mrq_q)
  val mrq_clr_mux     = OrM(mrq_clr_raw,     mrq_q)

  val mrq_pre_req_adv = mrq_pre_req_any && mrq_pre_req_rdy

  mrq_mlb_req_sel := RRA(mrq_mlb_req.U, mrq_mlb_req_any)
  mrq_pre_req_sel := RRA(mrq_pre_req.U, mrq_pre_req_adv)

  mrq_mem_req_sel := RegEnable(mrq_pre_req_sel, mrq_pre_req_adv)
  mrq_mem_req_fwd := RegEnable(ptc_res,         mrq_pre_req_adv)

  val mrq_pre_req_err_fwd = mrq_pre_req_mux.err || ptc_res

  val mrq_ram_req  = mrq_pre_req_any && !mrq_pre_req_err_fwd
  val mrq_err_req  = mrq_pre_req_any &&  mrq_pre_req_err_fwd


  //
  // mem

  val mem_fsm_en  = Pin(Bool())
  val mem_fsm_rdy = Pin(Bool())
  val mem_fsm_q   = Pin(UInt(2.W))
  val mem_fsm_nxt = Pin(UInt(2.W))

  val mem_fsm_is_idle = mem_fsm_q === mem_fsm_idle
  val mem_fsm_is_req  = mem_fsm_q === mem_fsm_req
  val mem_fsm_is_res  = mem_fsm_q === mem_fsm_res

  mem_fsm_en  := mem_fsm_is_idle && mrq_pre_req_any ||
                 mrq_mem_req_rdy

  mem_fsm_rdy := mem_fsm_is_idle ||
                 mrq_mem_req_rdy

  mem_fsm_q   := RegEnable(mem_fsm_nxt,
                           mem_fsm_idle,
                           mem_fsm_en)

  mem_fsm_nxt := EnQ(mrq_pre_req_adv,
                     mrq_pre_req_err_fwd ?? mem_fsm_res ::
                                            mem_fsm_req)

  val ram = Module(new SPRAM(log2Ceil(P.mrqIdx), P.clBits, 1))

  val ram_ren   = mrq_ram_req && mrq_pre_req_mux.wnr
  val ram_raddr = Enc(mrq_pre_req_sel)

  val ram_wen   = llc_req_w
  val ram_waddr = Enc(mrq_set_raw)
  val ram_wdata = llc_req_pld.data

  ram.clk      := clock
  ram.en       := ram_wen || ram_ren
  ram.wnr      := ram_wen
  ram.addr     := ram_wen ?? ram_waddr :: ram_raddr
  ram.wdata    := ram_wdata
  ram.wstrb    := 1.U(1.W)


  //
  // output

  // ptw req and mem res always have higher priority except when being preempted
  val ptw_mem_req = ptw_req_i.valid && !mem_fsm_is_req
  val mem_llc_res = mem_res_i.valid && !mem_fsm_is_res && !mem_res_max

  // the ptc data can change arbitrarily
  val ptc_res_q    = RegNext(ptc_res, false.B)
  val ptc_res_err  = ptc_res_q ?? ptc_res_pld.err  :: RegEnable(ptc_res_pld.err,  ptc_res_q)
  val ptc_res_data = ptc_res_q ?? ptc_res_pld.data :: RegEnable(ptc_res_pld.data, ptc_res_q)

  val ptw_mem_req_pld = MemReq(P,
                               ptw_req_pld.idx,
                               false.B,
                               0.U,
                               ptw_req_pld.pcn,
                               0.U)
  val mrq_mem_req_pld = MemReq(P,
                               Enc(mrq_mem_req_sel),
                               mrq_mem_req_mux.wnr,
                               0.U,
                               mrq_mem_req_mux.ppn ## mrq_mem_req_mux.xcn,
                               ram.rdata)
  val mem_llc_res_pld = MemRes(P,
                               mrq_clr_mux.idx,
                               mem_res_pld.err,
                               mem_res_pld.wnr,
                               mem_res_pld.data,
                               P.llcIdx)
  val mrq_err_res_pld = MemRes(P,
                               mrq_mem_req_mux.idx,
                               mrq_mem_req_mux.err || mrq_mem_req_fwd && ptc_res_err,
                               mrq_mem_req_mux.wnr,
                               ptc_res_data,
                               P.llcIdx)

  mrq_mem_req_rdy := mem_fsm_is_req && mem_req_o.ready ||
                     mem_fsm_is_res && llc_res_o.ready

  mrq_pre_req_rdy := mem_fsm_rdy && ptc_req_o.ready && !ram_wen &&
                        (mrq_ram_req && (mem_req_o.ready || !ptw_mem_req) ||
                         mrq_err_req && (llc_res_o.ready || !mem_llc_res))

  llc_req_i.ready := Any(mrq_inv)

  llc_res_o.valid := mem_fsm_is_res || mem_llc_res
  llc_res_o.bits  := mem_fsm_is_res ?? mrq_err_res_pld :: mem_llc_res_pld

  ptw_req_i.ready := mem_req_o.ready && !mem_fsm_is_req

  ptw_res_o.valid := mem_res_i.valid &&  mem_res_max
  ptw_res_o.bits  := MemRes(P,
                            M.U,
                            mem_res_pld.err,
                            mem_res_pld.wnr,
                            mem_res_pld.data)

  mlb_req_o.valid := mrq_mlb_req_any
  mlb_req_o.bits  := MLBReq(P,
                            Enc(mrq_mlb_req_sel),
                            mrq_mlb_req_mux.wnr,
                            mrq_mlb_req_mux.mpn)

  ptc_req_o.valid := mrq_pre_req_any && !ram_wen
  ptc_req_o.bits  := PTCReq(P,
                            mrq_pre_req_mux.wnr,
                            false.B,
                            mrq_pre_req_mux.mpn ## mrq_pre_req_mux.xcn,
                            ram.rdata)

  mem_req_o.valid := mem_fsm_is_req || ptw_mem_req
  mem_req_o.bits  := mem_fsm_is_req ?? mrq_mem_req_pld :: ptw_mem_req_pld

  mem_res_i.ready := ptw_res_o.ready &&  mem_res_max ||
                     llc_res_o.ready && !mem_res_max && !mem_fsm_is_res
}
