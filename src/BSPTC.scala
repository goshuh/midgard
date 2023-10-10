package midgard.backside

import  chisel3._
import  chisel3.util._
import  midgard._
import  midgard.util._


class PTCReq(val P: Param) extends Bundle {
  val wnr  = Bool()
  val err  = Bool()
  val mcn  = UInt(P.mcnBits.W)
  val data = UInt(P.clBits.W)
}

class PTCRes(val P: Param) extends Bundle {
  val hit  = Bool()
  val err  = Bool()
  val data = UInt(P.clBits.W)
}

class PTCEntry(val P: Param) extends Bundle {
  val vld  = Bool()
  val err  = Bool()
  val mcn  = UInt(P.mcnBits.W)

  def hit(m: UInt): Bool = {
    vld && (mcn === m)
  }
}


object PTCReq {
  def apply(P: Param, w: Bool, e: Bool, m: UInt, d: UInt): PTCReq = {
    val ret = Pin(new PTCReq(P))

    ret.wnr  := w
    ret.err  := e
    ret.mcn  := m
    ret.data := d
    ret
  }
}

object PTCRes {
  def apply(P: Param, h: Bool, e: Bool, d: UInt): PTCRes = {
    val ret = Pin(new PTCRes(P))

    ret.hit  := h
    ret.err  := e
    ret.data := d
    ret
  }
}

object PTCEntry {
  def apply(P: Param, r: PTCReq): PTCEntry = {
    val ret = Pin(new PTCEntry(P))

    ret.vld  := true.B
    ret.err  := r.err
    ret.mcn  := r.mcn
    ret
  }
}


class PTC(val P: Param) extends Module {

  // ---------------------------
  // io

  val mrq_req_i = IO(Flipped(Decoupled(new PTCReq(P))))
  val mrq_res_o = IO(           Output(new PTCRes(P)))

  val ptw_req_i = IO(Flipped(Decoupled(new PTCReq(P))))
  val ptw_res_o = IO(           Output(new PTCRes(P)))

  val rst_i     = IO(            Input(Bool()))


  // ---------------------------
  // logic

  val ptw_req      = ptw_req_i.fire
  val ptw_req_pld  = ptw_req_i.bits

  val mrq_req      = mrq_req_i.fire
  val mrq_req_pld  = mrq_req_i.bits


  if (P.ptcEn) {
    //
    // stage 0

    val s0_req_raw   = ptw_req ## mrq_req

    val s0_req       = Any(s0_req_raw)
    val s0_req_pld   = OrM(s0_req_raw,
                           Seq(mrq_req_pld,
                               ptw_req_pld))
    val s0_req_wdata = ptw_req_pld.data

    val s0_req_r     = ptw_req && !ptw_req_pld.wnr ||
                       mrq_req
    val s0_req_w     = ptw_req &&  ptw_req_pld.wnr

    val s0_hit_way   = Pin(Vec(P.ptcWays, Bool()))
    val s0_err_way   = Pin(Vec(P.ptcWays, Bool()))
    val s0_hit_idx   = Enc(s0_hit_way)
    val s0_rpl_idx   = PRA(P.ptcWays, s0_req_w)

    val s0_hit       = s0_req && Any(s0_hit_way)
    val s0_hit_err   = s0_req && Any(s0_hit_way.U & s0_err_way.U)
    val s0_hit_mrq   = s0_hit && mrq_req && mrq_req_pld.wnr

    Chk(s0_req -> OHp(s0_hit_way.U, true.B))


    //
    // stage 1

    val s1_req_q     = RegNext  (s0_hit_mrq, false.B)
    val s1_req_wdata = mrq_req_pld.data

    val s1_hit_err_q = RegEnable(s0_hit_err, s0_req_r)
    val s1_req_pld_q = RegEnable(s0_req_pld, s0_hit_mrq)
    val s1_req_idx_q = RegEnable(s0_hit_idx, s0_hit_mrq)

    // tag
    val ptc_q = Pin(Vec(P.ptcWays, new PTCEntry(P)))

    for (i <- 0 until P.ptcWays) {
      val s0_set = s0_req_w && (s0_rpl_idx   === i.U)
      val s1_set = s1_req_q && (s1_req_idx_q === i.U)

      s0_hit_way(i) := ptc_q(i).hit(s0_req_pld.mcn) && !rst_i
      s0_err_way(i) := ptc_q(i).err

      val set = s0_set || s1_set
      val pld = PTCEntry(P,
                         OrM(Seq(s0_set,
                                 s1_set),
                             Seq(s0_req_pld,
                                 s1_req_pld_q)))

      ptc_q(i)      := RegEnable(pld,          set)
      ptc_q(i).vld  := RegEnable(set, false.B, set || rst_i)
    }

    // data
    val ram = Module(new SPRAM(log2Ceil(P.ptcWays), P.clBits, P.clBytes))

    val ram_ren   = s0_req_r
    val ram_raddr = s0_hit_idx

    val ram_wen   = s1_req_q || s0_req_w
    val ram_waddr = s1_req_q ?? s1_req_idx_q :: s0_rpl_idx
    val ram_wdata = s1_req_q ?? s1_req_wdata :: s0_req_wdata

    ram.clk      := clock
    ram.en       := ram_wen || ram_ren
    ram.wnr      := ram_wen
    ram.addr     := ram_wen ?? ram_waddr :: ram_raddr
    ram.wdata    := ram_wdata
    ram.wstrb    := Rep(true.B, P.clBytes)


    //
    // output

    val s0_res = PTCRes(P,
                        s0_hit,
                        s1_hit_err_q,
                        ram.rdata)

    ptw_req_i.ready := Non(s1_req_q)
    mrq_req_i.ready := Non(s1_req_q ## ptw_req_i.valid)

    ptw_res_o       := s0_res
    mrq_res_o       := s0_res

  } else {

    val s0_res = PTCRes(P,
                        false.B,
                        false.B,
                        0.U)

    ptw_req_i.ready := true.B
    mrq_req_i.ready := true.B

    ptw_res_o       := s0_res
    mrq_res_o       := s0_res
  }
}
