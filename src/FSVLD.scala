package midgard.frontside

import  chisel3._
import  chisel3.util._
import  midgard._
import  midgard.util._


class VLDReq(val P: Param) extends Bundle {
  val wnr  = Bool()
  val mdn  = UInt(P.mdnBits.W)
  val vec  = UInt(P.dirBits.W)
}

class VLDEntry(val P: Param) extends Bundle {
  val vld  = Bool()
  val tag  = UInt(P.vldTagBits.W)
  val vec  = UInt(P.dirBits.W)
}


object VLDReq {
  def apply(P: Param, w: Bool, m: UInt, v: UInt): VLDReq = {
    val ret = Pin(new VLDReq(P))

    ret.wnr := w
    ret.mdn := m
    ret.vec := v
    ret
  }
}

object VLDEntry {
  def apply(P: Param, t: UInt, v: UInt): VLDEntry = {
    val ret = Pin(new VLDEntry(P))

    ret.vld := true.B
    ret.tag := t
    ret.vec := v
    ret
  }
}


class VLD(val P: Param) extends Module {

  // ---------------------------
  // io

  val vld_req_i = IO(Flipped(Decoupled(new VLDReq(P))))
  val vld_res_o = IO(            Valid(new VLDReq(P)))


  // ---------------------------
  // logic

  val sx_inv       = 0.U.asTypeOf(new VLDEntry(P))

  val s0_req       = vld_req_i.valid
  val s0_req_pld   = vld_req_i.bits

  val s0_idx       = s0_req_pld.mdn(P.vldBits.W)

  val s1_req_q     = RegNext  (s0_req,     false.B)
  val s1_req_pld_q = RegEnable(s0_req_pld, s0_req)

  val s1_rdata     = Vec(P.vldWays,  new VLDEntry(P))
  val s1_hit_way   = Vec(P.vldWays,  Bool())
  val s1_inv_way   = Vec(P.vldWays,  Bool())
  val s1_hit_mux   = OrM(s1_hit_way, s1_rdata)

  val s1_idx       = s1_req_pld_q.mdn(P.vldBits.W)
  val s1_tag       = s1_req_pld_q.mdn(P.mdnBits :- P.vldBits)

  val s1_hit_any   = Any(s1_hit_way)
  val s1_inv_any   = Any(s1_inv_way)
  val s1_hit_vec   = Any(s1_hit_mux.vec & s1_req_pld_q.vec)

  // actions
  val s1_clr       = s1_req_q &&  s1_req_pld_q.wnr &&  s1_hit_any
  val s1_new       = s1_req_q && !s1_req_pld_q.wnr && !s1_hit_any
  val s1_upd       = s1_req_q && !s1_req_pld_q.wnr &&  s1_hit_any && !s1_hit_vec

  val s1_rpl_way   = s1_hit_any ??     s1_hit_way.U  ::
                     s1_inv_any ?? PrL(s1_inv_way.U) ::
                                   PRA(P.vldWays, s1_req_q)
  val s1_rpl_mdn   = s1_clr ?? s1_req_pld_q.mdn ::
                    (s1_hit_mux.tag ## s1_idx)

  val s1_vld       = s1_clr || s1_new || s1_upd

  val s1_wdata     = s1_clr ?? sx_inv ::
                     s1_new ?? VLDEntry(P,
                                        s1_tag,
                                        s1_req_pld_q.vec) ::
                               VLDEntry(P,
                                        s1_tag,
                                        s1_req_pld_q.vec | s1_hit_mux.vec)

  val s1_res       = s1_clr ||
                     s1_new && !s1_inv_any
  val s1_res_pld   = VLDReq(P,
                            false.B,
                            s1_rpl_mdn,
                            s1_hit_mux.vec)

  val s2_res_q     = RegNext  (s1_res,     false.B)
  val s2_res_pld_q = RegEnable(s1_res_pld, s1_res)

  // reset
  val rst_q    = Pin(UInt((P.vldBits + 1).W))
  val rst_pend = Non(rst_q(P.vldBits))
  val rst_done = Any(rst_q(P.vldBits))

  rst_q := RegEnable(rst_q + 1.U,
                     0.U,
                     rst_pend)

  for (i <- 0 until P.vldWays) {
    val wid = 8 * (new VLDEntry(P).getWidth + 7) / 8

    val ram = Module(new SPRAM(P.vldSets, wid, wid / 8))

    val ram_ren    = s0_req
    val ram_raddr  = s0_idx

    val ram_wen    = rst_pend || s1_vld
    val ram_waddr  = rst_pend ?? rst_q(P.vldBits.W) :: s1_rpl_way
    val ram_wdata  = rst_pend ?? sx_inv             :: s1_wdata

    ram.clk       := clock
    ram.en        := ram_wen || ram_ren
    ram.wnr       := ram_wen
    ram.addr      := ram_wen ?? ram_waddr :: ram_raddr
    ram.wdata     := ram_wdata.asUInt
    ram.wstrb     := Rep(true.B, wid / 8)

    s1_rdata  (i) := ram.rdata

    s1_hit_way(i) := s1_req_q &&  s1_rdata(i).vld && (s1_rdata(i).tag === s1_tag)
    s1_inv_way(i) := s1_req_q && !s1_rdata(i).vld
  }


  //
  // output

  vld_req_i.ready := rst_done && Non(s1_clr ## s1_new ## s1_upd)

  vld_res_o.valid := s2_res_q
  vld_res_o.bits  := s2_res_pld_q
}