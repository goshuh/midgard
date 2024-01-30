package midgard.frontside

import  chisel3._
import  chisel3.util._
import  midgard._
import  midgard.util._


class VTDReq(val P: Param) extends Bundle {
  val cmd  = UInt(3.W)
  val mcn  = UInt(P.mcnBits.W)
  val vec  = UInt(P.dirBits.W)
}

class VTDEntry(val P: Param) extends Bundle {
  val vld  = Bool()
  val tag  = UInt(P.vtdTagBits.W)
  val vec  = UInt(P.dirBits.W)
}


object VTDReq {
  def apply(P: Param, c: UInt, m: UInt, v: UInt): VTDReq = {
    val ret = Pin(new VTDReq(P))

    ret.cmd := c
    ret.mcn := m
    ret.vec := v
    ret
  }
}

object VTDEntry {
  def apply(P: Param, t: UInt, v: UInt): VTDEntry = {
    val ret = Pin(new VTDEntry(P))

    ret.vld := Any(v)
    ret.tag := t
    ret.vec := v
    ret
  }
}


class VTD(val P: Param) extends Module {

  // ---------------------------
  // io

  val vtd_req_i = IO(Flipped(Decoupled(new VTDReq(P))))
  val vtd_res_o = IO(        Decoupled(new VTDReq(P)))

  val dir_fwd_i = IO(            Input(new VTDReq(P)))


  // ---------------------------
  // logic

  val vtd_res = vtd_res_o.fire


  val s0_req       = vtd_req_i.valid
  val s0_req_pld   = vtd_req_i.bits

  val s0_idx       = s0_req_pld.mcn  (P.vtdBits.W)

  val s1_req_q     = RegNext  (s0_req,     false.B)
  val s1_req_pld_q = RegEnable(s0_req_pld, s0_req)

  val s1_idx       = s1_req_pld_q.mcn(P.vtdBits.W)
  val s1_tag       = s1_req_pld_q.mcn(P.mcnBits := P.vtdBits)

  val s1_rdata     = Pin(Vec(P.vtdWays, new VTDEntry(P)))
  val s1_hit_way   = Pin(Vec(P.vtdWays, Bool()))
  val s1_inv_way   = Pin(Vec(P.vtdWays, Bool()))

  Chk(s1_req_q -> OHp(s1_hit_way.U, true.B))

  val s1_hit_any   = Any(s1_hit_way)
  val s1_inv_any   = Any(s1_inv_way)

  val s1_rpl_way   = s1_hit_any ??     s1_hit_way.U  ::
                     s1_inv_any ?? PrL(s1_inv_way.U) ::
                                   PRA(P.vtdWays, s1_req_q)

  val s1_hit_mux   = OrM(s1_hit_way, s1_rdata)
  val s1_rpl_mux   = OrM(s1_rpl_way, s1_rdata)

  // llc promotes a rd to have wr permission
  val s1_cmd_rd    = s1_req_pld_q.cmd(0)
  val s1_cmd_wr    = s1_req_pld_q.cmd(1) || dir_fwd_i.cmd(1)
  val s1_cmd_ev    = s1_req_pld_q.cmd(2)

  // llc tracks the line while vtd doesn't. this should also be considered as a hit
  val s1_hit       = s1_hit_any || Any(dir_fwd_i.vec)
  val s1_vec       = s1_hit_mux.vec |  dir_fwd_i.vec

  Chk(s1_hit_any  -> Non(dir_fwd_i.vec & ~s1_hit_mux.vec))

  //                        ram     res
  //   wr    hit        ->  0       ram-req
  //   wr   !hit   inv  ->
  //   wr   !hit  !inv  ->
  //   rd    hit   inc  ->
  //   rd    hit  !inc  ->  req+ram
  //   rd   !hit   inv  ->  req+ram
  //   rd   !hit  !inv  ->  req+ram rpl
  //   ev    hit        ->  ram-req req

  val s1_vec_add   = s1_vec |  s1_req_pld_q.vec
  val s1_vec_sub   = s1_vec & ~s1_req_pld_q.vec

  val s1_wen       = s1_req_q  && (s1_cmd_wr &&     s1_hit                           ||
                                   s1_cmd_rd && Non(s1_hit_way.U & s1_req_pld_q.vec) ||
                                   s1_cmd_ev)

  val s1_wdata     = VTDEntry(P,
                              s1_tag,
                              OrM(Seq(s1_cmd_rd,
                                      s1_cmd_ev),
                                  Seq(s1_vec_add,
                                      s1_vec_sub)))

  val s1_res       = s1_req_q  && (s1_cmd_wr &&  s1_hit && Any(s1_vec_sub) ||
                                   s1_cmd_rd && !s1_hit && Non(s1_inv_way) ||
                                   s1_cmd_ev)

  val s1_res_mcn   = s1_cmd_rd ?? (s1_rpl_mux.tag ## s1_idx) ::
                                  (s1_tag         ## s1_idx)

  val s1_res_vec   = s1_cmd_wr ??  s1_vec_sub     ::
                     s1_cmd_rd ??  s1_rpl_mux.vec ::
                                   s1_req_pld_q.vec

  // reset
  val rst_q     = Pin(UInt((P.vtdBits + 1).W))
  val rst_pend  = Non(rst_q(P.vtdBits))
  val rst_done  = Any(rst_q(P.vtdBits))

  val rst_idx   = rst_q(P.vtdBits.W)
  val rst_wdata = 0.U.asTypeOf(new VTDEntry(P))

  rst_q := RegEnable(rst_q + 1.U,
                     0.U,
                     rst_pend)

  for (i <- 0 until P.vtdWays) {
    val ram = Module(new SPRAM(P.vtdBits, new VTDEntry(P).getWidth, 1))

    val ram_ren    = s0_req
    val ram_raddr  = s0_idx

    val ram_wen    = rst_pend || s1_wen    && s1_rpl_way(i)
    val ram_waddr  = rst_pend ?? rst_idx   :: s1_idx
    val ram_wdata  = rst_pend ?? rst_wdata :: s1_wdata

    ram.clk       := clock
    ram.en        := ram_wen || ram_ren
    ram.wnr       := ram_wen
    ram.addr      := ram_wen ?? ram_waddr :: ram_raddr
    ram.wdata     := ram_wdata.asUInt
    ram.wstrb     := 1.U(1.W)

    s1_rdata  (i) := ram.rdata.asTypeOf(new VTDEntry(P))

    s1_hit_way(i) := s1_req_q &&  s1_rdata(i).vld && (s1_rdata(i).tag === s1_tag)
    s1_inv_way(i) := s1_req_q && !s1_rdata(i).vld
  }

  val s2_req_q     = RegEnable(s1_res,
                               false.B,
                               s1_res || vtd_res)

  val s2_res_mcn_q = RegEnable(s1_res_mcn, s1_res)
  val s2_res_vec_q = RegEnable(s1_res_vec, s1_res)


  //
  // output

  vtd_req_i.ready := rst_done && !s1_req_q && !(vtd_res_o.valid && !vtd_res_o.ready)

  vtd_res_o.valid := s1_req_q
  vtd_res_o.bits  := VTDReq(P,
                            2.U,
                            s2_res_mcn_q,
                            s2_res_vec_q)
}
