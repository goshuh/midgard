package midgard

import chisel3._
import chisel3.util._

import midgard.misc._


case class MidgardParam(
  maBits:    Int,
  paBits:    Int,
  tlbEn:     Int,
  tlbSetNum: Int,
  tlbWayNum: Int,
  ptcEn:     Int,
  ptcNum:    Int,
  cfgBase:   BigInt,
  cfgSize:   BigInt
)


class MidgardMMUResp(val p: MidgardParam) extends Bundle {
  val err = UInt(1.W)
  val ppn = UInt((p.paBits - 12).W)
}

object MidgardMMUResp {
  def apply(p: MidgardParam, e: UInt, n: UInt): MidgardMMUResp = {
    val res = Wire(new MidgardMMUResp(p))

    res.err := e
    res.ppn := n

    res
  }
}

class MidgardLLCResp extends Bundle {
  val hit = UInt( 1.W)
  val pte = UInt(64.W)
}

class MidgardMEMResp extends Bundle {
  val err = UInt( 1.W)
  val pte = UInt(64.W)
}

class MidgardCFGReq  extends Bundle {
  val ren  = UInt( 1.W)
  val addr = UInt( 4.W)
  val data = UInt(64.W)
}

class MidgardCFGResp extends Bundle {
  val inv  = UInt( 1.W)
  val ren  = UInt( 1.W)
  val data = UInt(64.W)
}


class MidgardMMU(p: MidgardParam) extends MultiIOModule {

  // --------------------------
  // param

  val ptwLvl = (p.maBits - 12 + (9 - 1)) / 9


  // --------------------------
  // io

  val mmu_req_i  = IO(Flipped(Decoupled(UInt((p.maBits - 12).W))))
  val mmu_resp_o = IO(        Decoupled(new MidgardMMUResp(p)))

  val llc_req_o  = IO(        Decoupled(UInt(p.maBits.W)))
  val llc_resp_i = IO(Flipped(Decoupled(new MidgardLLCResp())))

  val mem_req_o  = IO(        Decoupled(UInt(p.paBits.W)))
  val mem_resp_i = IO(Flipped(Decoupled(new MidgardMEMResp())))

  val cfg_req_i  = IO(Flipped(Decoupled(new MidgardCFGReq())))
  val cfg_resp_o = IO(        Decoupled(new MidgardCFGResp()))


  // --------------------------
  // cfg

  val cfg_q   = dontTouch(Wire(Vec(ptwLvl + 1, UInt(p.maBits.W))))
  val cfg_sel = dontTouch(Wire(Vec(ptwLvl + 1, UInt(1.W))))

  val cfg_req_fire  = cfg_req_i.fire()
  val cfg_resp_fire = cfg_resp_o.fire()
  val cfg_addr_sel  = Dec(cfg_req_i.bits.addr)

  val cfg_vld_q = dontTouch(RegEnable(cfg_req_fire, 0.U(1.W), cfg_req_fire | cfg_resp_fire))
  val cfg_ren_q = dontTouch(RegEnable(cfg_req_i.bits.ren,     cfg_req_fire))

  for (i <- 0 to ptwLvl) {
    cfg_sel(i) := cfg_req_fire & cfg_addr_sel(i)
    cfg_q  (i) := RegEnable(cfg_req_i.bits.data(p.maBits - 1, 0), cfg_sel(i) & ~cfg_req_i.bits.ren)
  }

  // output
  cfg_req_i.ready := ~cfg_vld_q

  cfg_resp_o.valid     := cfg_vld_q
  cfg_resp_o.bits.inv  := RegEnable(   ~cfg_sel.orR,     cfg_req_fire)
  cfg_resp_o.bits.ren  := cfg_ren_q
  cfg_resp_o.bits.data := RegEnable(OrM(cfg_sel, cfg_q), cfg_req_fire)


  // --------------------------
  // inst

  val u_tlb = Module(new MidgardTLB(p))
  val u_ptw = Module(new MidgardPTW(p))

  u_tlb.tlb_req_i  <> mmu_req_i
  u_tlb.ptw_resp_i <> u_ptw.ptw_resp_o

  u_ptw.ptw_req_i  <> u_tlb.ptw_req_o
  u_ptw.llc_resp_i <> llc_resp_i
  u_ptw.mem_resp_i <> mem_resp_i
  u_ptw.cfg_i      := cfg_q

  // output
  mmu_resp_o <> u_tlb.tlb_resp_o
  llc_req_o  <> u_ptw.llc_req_o
  mem_req_o  <> u_ptw.mem_req_o
}
