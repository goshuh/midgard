package midgard.frontside

import  chisel3._
import  chisel3.util._
import  midgard._
import  midgard.util._


class VTDReq(val P: Param) extends Bundle {
  val wnr  = Bool()
  val mqn  = UInt(P.mqnBits.W)
  val vec  = UInt(P.dirBits.W)
}

class VTDEntry(val P: Param) extends Bundle {
  val vld  = Bool()
  val tag  = UInt(P.vtdTagBits.W)
  val vec  = UInt(P.dirBits.W)
}


object VTDReq {
  def apply(P: Param, w: Bool, m: UInt, v: UInt): VTDReq = {
    val ret = Pin(new VTDReq(P))

    ret.wnr := w
    ret.mqn := m
    ret.vec := v
    ret
  }
}

object VTDEntry {
  def apply(P: Param, t: UInt, v: UInt): VTDEntry = {
    val ret = Pin(new VTDEntry(P))

    ret.vld := true.B
    ret.tag := t
    ret.vec := v
    ret
  }
}


class VTD(val P: Param) extends Module {

  // ---------------------------
  // io

  val vtd_req_i = IO(Flipped(Decoupled(new VTDReq(P))))
  val vtd_res_o = IO(           Output(new VTDReq(P)))


  // ---------------------------
  // logic

  val sx_inv       = 0.U.asTypeOf(new VTDEntry(P))

  val s0_req       = vtd_req_i.valid
  val s0_req_pld   = vtd_req_i.bits

  val s0_idx       = s0_req_pld.mqn(P.vtdBits.W)

  val s1_req_q     = RegNext  (s0_req,     false.B)
  val s1_req_pld_q = RegEnable(s0_req_pld, s0_req)

  val s1_rdata     = Pin(Vec(P.vtdWays,  new VTDEntry(P)))
  val s1_hit_way   = Pin(Vec(P.vtdWays,  Bool()))
  val s1_inv_way   = Pin(Vec(P.vtdWays,  Bool()))
  val s1_hit_mux   = OrM(s1_hit_way, s1_rdata)

  val s1_idx       = s1_req_pld_q.mqn(P.vtdBits.W)
  val s1_tag       = s1_req_pld_q.mqn(P.mqnBits :- P.vtdBits)

  val s1_hit_any   = Any(s1_hit_way)
  val s1_inv_any   = Any(s1_inv_way)
  val s1_hit_vec   = Any(s1_hit_mux.vec & s1_req_pld_q.vec)

  // actions
  val s1_clr       = s1_req_q &&  s1_req_pld_q.wnr &&  s1_hit_any
  val s1_new       = s1_req_q && !s1_req_pld_q.wnr && !s1_hit_any
  val s1_upd       = s1_req_q && !s1_req_pld_q.wnr &&  s1_hit_any && !s1_hit_vec

  val s1_rpl_way   = s1_hit_any ??     s1_hit_way.U  ::
                     s1_inv_any ?? PrL(s1_inv_way.U) ::
                                   PRA(P.vtdWays, s1_req_q)
  val s1_rpl_mqn   = s1_clr ?? s1_req_pld_q.mqn ::
                    (s1_hit_mux.tag ## s1_idx)

  val s1_vld       = s1_clr || s1_new || s1_upd

  val s1_wdata     = s1_clr ?? sx_inv ::
                     s1_new ?? VTDEntry(P,
                                        s1_tag,
                                        s1_req_pld_q.vec) ::
                               VTDEntry(P,
                                        s1_tag,
                                        s1_req_pld_q.vec | s1_hit_mux.vec)

  val s1_res       = s1_clr ||
                     s1_new && !s1_inv_any
  val s1_res_pld   = VTDReq(P,
                            false.B,
                            s1_rpl_mqn,
                            s1_hit_mux.vec)

  val s2_res_q     = RegNext  (s1_res,     false.B)
  val s2_res_pld_q = RegEnable(s1_res_pld, s1_res)

  // reset
  val rst_q    = Pin(UInt((P.vtdBits + 1).W))
  val rst_pend = Non(rst_q(P.vtdBits))
  val rst_done = Any(rst_q(P.vtdBits))

  rst_q := RegEnable(rst_q + 1.U,
                     0.U,
                     rst_pend)

  for (i <- 0 until P.vtdWays) {
    val old = new VTDEntry(P).getWidth
    val wid = 8 * (old + 7) / 8

    val ram = Module(new SPRAM(P.vtdSets, wid, wid / 8))

    val ram_ren    = s0_req
    val ram_raddr  = s0_idx

    val ram_wen    = rst_pend || s1_vld
    val ram_waddr  = rst_pend ?? rst_q(P.vtdBits.W) :: s1_rpl_way
    val ram_wdata  = rst_pend ?? sx_inv             :: s1_wdata

    ram.clk       := clock
    ram.en        := ram_wen || ram_ren
    ram.wnr       := ram_wen
    ram.addr      := ram_wen ?? ram_waddr :: ram_raddr
    ram.wdata     := ram_wdata.asUInt
    ram.wstrb     := Rep(true.B, wid / 8)

    s1_rdata  (i) := ram.rdata(old.W).asTypeOf(new VTDEntry(P))

    s1_hit_way(i) := s1_req_q &&  s1_rdata(i).vld && (s1_rdata(i).tag === s1_tag)
    s1_inv_way(i) := s1_req_q && !s1_rdata(i).vld
  }


  //
  // output

  vtd_req_i.ready := rst_done && Non(s1_clr ## s1_new ## s1_upd)

  vtd_res_o.wnr   := s2_res_q
  vtd_res_o.mqn   := s2_res_pld_q.mqn
  vtd_res_o.vec   := s2_res_pld_q.vec
}