package midgard

import chisel3._
import chisel3.util._

import chipsalliance.rocketchip.config._

import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

import midgard.misc._


case object MidgardKey extends Field[MidgardParam]

class WithMidgard extends Config((site, here, up) => {
  case MidgardKey => MidgardParam(
    maBits    = 32,
    paBits    = 32,
    tlbEn     = 1,
    tlbSetNum = 1024,
    tlbWayNum = 4,
    ptcEn     = 0,
    ptcNum    = 32,
    cfgBase   = 0x11000000,
    cfgSize   = 0x40
  )
})


class MidgardTLWrapper(implicit p: Parameters) extends LazyModule()(p) {

  // --------------------------
  // diplomacy

  val q = p(MidgardKey)

  val cfg_node = TLManagerNode(
                   Seq(TLSlavePortParameters.v1(
                         Seq(TLSlaveParameters.v1(
                               address         = Seq(AddressSet(q.cfgBase, q.cfgSize - 1)),
                               regionType      = RegionType.UNCACHED,
                               supportsGet     = TransferSizes(1, 8),
                               supportsPutFull = TransferSizes(1, 8),
                               fifoId          = Some(0))),
                         beatBytes = 8)))

  val mem_node = TLClientNode(
                   Seq(TLMasterPortParameters.v1(
                         Seq(TLMasterParameters.v1(
                               name = "mmu")))))


  lazy val module = new LazyModuleImp(this) {

    // --------------------------
    // io

    // for simplicity
    val mmu_req_i  = IO(Flipped(Decoupled(UInt((q.maBits - 12).W))))
    val mmu_resp_o = IO(        Decoupled(new MidgardMMUResp(q)))

    val llc_req_o  = IO(        Decoupled(UInt(q.maBits.W)))
    val llc_resp_i = IO(Flipped(Decoupled(new MidgardLLCResp())))


    // --------------------------
    // inst

    val u_mmu = Module(new MidgardMMU(q))

    u_mmu.mmu_req_i  <> mmu_req_i
    mmu_resp_o       <> u_mmu.mmu_resp_o
    llc_req_o        <> u_mmu.llc_req_o
    u_mmu.llc_resp_i <> llc_resp_i


    // --------------------------
    // cfg

    val cfg_in = cfg_node.in(0)._1

    // a
    cfg_in.a.ready            := u_mmu.cfg_req_i.ready

    u_mmu.cfg_req_i.valid     := cfg_in.a.valid
    u_mmu.cfg_req_i.bits.rnw  := cfg_in.a.bits.opcode === TLMessages.Get
    u_mmu.cfg_req_i.bits.addr := cfg_in.a.bits.address
    u_mmu.cfg_req_i.bits.data := cfg_in.a.bits.data

    // b
    cfg_in.b.valid            := 0.U

    // c
    cfg_in.c.ready            := 0.U

    // d
    cfg_in.d.valid            := u_mmu.cfg_resp_o.valid
    cfg_in.d.bits.opcode      := Mux(u_mmu.cfg_resp_o.bits.rnw,
                                     TLMessages.AccessAckData,
                                     TLMessages.AccessAck)
    cfg_in.d.bits.param       := 0.U
    cfg_in.d.bits.size        := 3.U
    cfg_in.d.bits.source      := 0.U
    cfg_in.d.bits.sink        := 0.U
    cfg_in.d.bits.denied      := u_mmu.cfg_resp_o.bits.inv
    cfg_in.d.bits.data        := u_mmu.cfg_resp_o.bits.data
    cfg_in.d.bits.corrupt     := 0.U

    u_mmu.cfg_resp_o.ready    := cfg_in.d.ready

    // e
    cfg_in.e.ready            := 0.U


    // --------------------------
    // mem

    val mem_in = mem_node.out(0)._1

    // a
    mem_in.a.valid            := u_mmu.mem_req_o.valid
    mem_in.a.bits.opcode      := TLMessages.Get
    mem_in.a.bits.size        := 3.U
    mem_in.a.bits.source      := 0.U
    mem_in.a.bits.address     := u_mmu.mem_req_o.bits
    mem_in.a.bits.mask        := MaskGen(u_mmu.mem_req_o.bits, 3.U, 8)
    mem_in.a.bits.data        := 0.U
    mem_in.a.bits.corrupt     := 0.U

    u_mmu.mem_req_o.ready     := mem_in.a.ready

    // b
    mem_in.b.ready            := 0.U

    // c
    mem_in.c.valid            := 0.U

    // d
    mem_in.d.ready            := u_mmu.mem_resp_i.ready

    u_mmu.mem_resp_i.valid    := mem_in.d.valid
    u_mmu.mem_resp_i.bits.err := mem_in.d.bits.denied |
                                 mem_in.d.bits.corrupt
    u_mmu.mem_resp_i.bits.pte := mem_in.d.bits.data
  }
}


trait HasMidgard { this: BaseSubsystem =>

  val u_mmu = LazyModule(new MidgardTLWrapper())

  // width modification
  u_mmu.cfg_node := cbus.coupleTo("u_mmu") {
    TLFragmenter(8, 64) := _
  }

  val u_mmu_node = u_mmu.mem_node
}
