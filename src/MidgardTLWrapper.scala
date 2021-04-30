package midgard

import chipsalliance.rocketchip.config._
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.diplomaticobjectmodel.DiplomaticObjectModelAddressing
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.{LogicalModuleTree, LogicalTreeNode}
import freechips.rocketchip.diplomaticobjectmodel.model.{OMComponent, OMDevice, OMInterrupt, OMMemoryRegion}
import freechips.rocketchip.regmapper.{RegField, RegFieldDesc, RegisterRouter, RegisterRouterParams}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import midgard.misc._


case object MidgardKey extends Field[MidgardParam]

class WithMidgard extends Config((site, here, up) => {
  case MidgardKey => MidgardParam(
    maBits    = 48,
    paBits    = 32,
    tlbEn     = 1,
    tlbSetNum = 1024,
    tlbWayNum = 4,
    ptcEn     = 0,
    ptcNum    = 32,
    prbEn     = 1,
    cfgBase   = 0x11000000,
    cfgSize   = 0x40)
})


case class MidgardOM(
  memoryRegions: Seq[OMMemoryRegion],
  interrupts:    Seq[OMInterrupt],
) extends OMDevice


class MidgardPRBEntry(val p: MidgardParam, val s: Int) extends Bundle {
  val vld  = UInt(1.W)
  val mem  = UInt(1.W)
  val llc  = UInt(1.W)
  val addr = UInt((p.maBits - 6).W)
  val data = UInt(512.W)
  val src  = UInt(s.W)
}

class MidgardTLWrapper(implicit p: Parameters)
  extends RegisterRouter(
    RegisterRouterParams(
      name      = "midgard-mmu",
      compat    =  Seq(),
      base      =  p(MidgardKey).cfgBase,
      beatBytes =  8))
  with HasTLControlRegMap {

  // --------------------------
  // diplomacy

  val q = p(MidgardKey)

  val adp_node = TLAdapterNode(
                   managerFn = { mp =>
                     mp.v1copy(
                       managers = mp.managers.map { m =>
                         m.v1copy(
                           supportsAcquireB = m.supportsGet,
                           supportsAcquireT = m.supportsPutFull,
                           alwaysGrantsT    = true)
                       },
                       // 1 data + 6 walk
                       endSinkId = 7)
                   },
                   clientFn = { cp =>
                     cp.v1copy(
                       clients = cp.clients.map { c =>
                         c.v1copy(
                           supportsProbe = TransferSizes.none)
                       })
                   })

  val logicalTreeNode = new LogicalTreeNode(() => Some(device)) {
    def getOMComponents(resourceBindings: ResourceBindings, children: Seq[OMComponent]) = {
      Seq(MidgardOM(
            memoryRegions = DiplomaticObjectModelAddressing.getOMMemoryRegions("Midgard", resourceBindings, None),
            interrupts    = DiplomaticObjectModelAddressing.describeGlobalInterrupts(device.describe(resourceBindings).name, resourceBindings)))
    }
  }


  lazy val module = new LazyModuleImp(this) {

    // ------------------------
    // mmio

    val root = Seq(
      0x00 ->
        Seq(RegField(64, RegFieldDesc( "ROOT",      "ROOT register in the PA space")))
    )
    val base = Seq.tabulate(q.ptwLvl)(n => {
      0x08 * (n + 1) ->
        Seq(RegField(64, RegFieldDesc(s"BASE${n}", s"BASE${n} register in the MA space")))
    })

    regmap(root ++ base: _*)


    // --------------------------
    // inst

    val u_mmu = Module(new MidgardMMU(q))


    // --------------------------
    // cfg

    val cfg = controlNode.in.head._1

    // a
    cfg.a.ready               :=  u_mmu.cfg_req_i.ready

    u_mmu.cfg_req_i.valid     :=  cfg.a.valid
    u_mmu.cfg_req_i.bits.rnw  := (cfg.a.bits.opcode === TLMessages.Get)
    u_mmu.cfg_req_i.bits.addr :=  cfg.a.bits.address(6, 3)
    u_mmu.cfg_req_i.bits.data :=  cfg.a.bits.data

    // b
    cfg.b.valid               :=  0.U

    // c
    cfg.c.ready               :=  0.U

    // d
    cfg.d.valid               :=  u_mmu.cfg_resp_o.valid
    cfg.d.bits.opcode         :=  Mux(u_mmu.cfg_resp_o.bits.rnw,
                                      TLMessages.AccessAckData,
                                      TLMessages.AccessAck)
    cfg.d.bits.param          :=  0.U
    cfg.d.bits.size           :=  3.U
    cfg.d.bits.source         :=  RegEnable(cfg.a.bits.source, cfg.a.fire())
    cfg.d.bits.sink           :=  0.U
    cfg.d.bits.denied         :=  u_mmu.cfg_resp_o.bits.inv
    cfg.d.bits.data           :=  u_mmu.cfg_resp_o.bits.data
    cfg.d.bits.corrupt        :=  0.U

    u_mmu.cfg_resp_o.ready    :=  cfg.d.ready

    // e
    cfg.e.ready               :=  0.U


    // --------------------------
    // adapter

    val llc = adp_node.in .head._1
    val mem = adp_node.out.head._1

    val llc_a_sel_q = dontTouch(Wire(UInt(1.W)))
    val mmu_a_sel_q = dontTouch(Wire(UInt(1.W)))
    val mem_a_sel_q = dontTouch(Wire(UInt(1.W)))

    val mmu_req_fire  = u_mmu.mmu_req_i.fire()
    val mmu_resp_fire = u_mmu.mmu_resp_o.fire()

    val prb_busy      = dontTouch(Wire(UInt(1.W)))
    val mmu_busy      = prb_busy |
                        RegEnable(mmu_req_fire, 0.U(1.W), mmu_req_fire | mmu_resp_fire)

    // llc a
    //   1. to llc d: acquireBlock.BtoT/acquirePerm
    //   2. to mmu n: acquireBlock.NtoB/NtoT
    //   3. to prb:   acquireBlock.NtoB
    val llc_a_dataless         = (llc.a.bits.opcode === TLMessages.AcquirePerm)  |
                                 (llc.a.bits.opcode === TLMessages.AcquireBlock) &
                                 (llc.a.bits.param  === TLPermissions.BtoT)

    val llc_a_llc_d            =  dontTouch(Wire    (llc.d.cloneType))
    val llc_a_mmu_a            =  dontTouch(WireInit(llc.a))

    llc_a_llc_d.valid         :=  llc.a.valid &  llc_a_dataless

    llc_a_llc_d.bits.opcode   :=  TLMessages.Grant
    llc_a_llc_d.bits.param    :=  TLPermissions.toT
    llc_a_llc_d.bits.size     :=  llc.a.bits.size
    llc_a_llc_d.bits.source   :=  llc.a.bits.source
    llc_a_llc_d.bits.sink     :=  0.U
    llc_a_llc_d.bits.denied   :=  0.U
    llc_a_llc_d.bits.corrupt  :=  0.U
    llc_a_llc_d.bits.data     :=  0.U

    llc_a_mmu_a.valid         :=  llc.a.valid & ~llc_a_dataless & ~mmu_busy

    llc.a.ready               :=  Mux(llc_a_dataless,
                                      llc_a_llc_d.ready,
                                      llc_a_mmu_a.ready | mmu_busy)

    // llc b
    //   1. from mmu o
    llc.b.valid               :=  u_mmu.llc_req_o.valid

    llc.b.bits.opcode         :=  TLMessages.Probe
    llc.b.bits.param          :=  TLPermissions.toB
    llc.b.bits.size           :=  6.U
    llc.b.bits.source         :=  0.U
    llc.b.bits.address        :=  Cat(u_mmu.llc_req_o.bits(q.maBits - 1, 6), 0.U(6.W))
    llc.b.bits.mask           :=  MaskGen(llc.b.bits.address, llc.b.bits.size, 64)
    llc.b.bits.data           :=  0.U
    llc.b.bits.corrupt        :=  0.U

    u_mmu.llc_req_o.ready     :=  llc.b.ready

    // llc c
    //   1. to llc d: release
    //   2. to mmu o: probeAck[Data]
    //   3. to mmu n: releaseData
    val llc_c_dataless         = (llc.c.bits.opcode === TLMessages.Release)
    val llc_c_probe_resp       = (llc.c.bits.opcode === TLMessages.ProbeAck) |
                                 (llc.c.bits.opcode === TLMessages.ProbeAckData)

    val llc_c_llc_d            =  dontTouch(Wire(llc.d.cloneType))
    val llc_c_mmu_a            =  dontTouch(Wire(llc.a.cloneType))

    llc_c_llc_d.valid         :=  llc.c.valid &  llc_c_dataless

    llc_c_llc_d.bits.opcode   :=  TLMessages.ReleaseAck
    llc_c_llc_d.bits.param    :=  0.U
    llc_c_llc_d.bits.size     :=  llc.c.bits.size
    llc_c_llc_d.bits.source   :=  llc.c.bits.source
    llc_c_llc_d.bits.sink     :=  0.U
    llc_c_llc_d.bits.denied   :=  0.U
    llc_c_llc_d.bits.corrupt  :=  0.U
    llc_c_llc_d.bits.data     :=  0.U

    llc_c_mmu_a.valid         :=  llc.c.valid & ~llc_c_dataless & ~llc_c_probe_resp

    llc_c_mmu_a.bits.opcode   :=  llc.c.bits.opcode
    llc_c_mmu_a.bits.param    :=  llc.c.bits.param
    llc_c_mmu_a.bits.size     :=  llc.c.bits.size
    llc_c_mmu_a.bits.source   :=  llc.c.bits.source
    llc_c_mmu_a.bits.address  :=  llc.c.bits.address
    llc_c_mmu_a.bits.mask     :=  MaskGen(llc_c_mmu_a.bits.address, llc_c_mmu_a.bits.size, 64)
    llc_c_mmu_a.bits.corrupt  :=  llc.c.bits.corrupt
    llc_c_mmu_a.bits.data     :=  llc.c.bits.data

    u_mmu.llc_resp_i.valid    :=  llc.c.valid &  llc_c_probe_resp

    u_mmu.llc_resp_i.bits.hit := (llc.c.bits.param =/= TLPermissions.NtoN) &
                                 (llc.c.bits.param =/= TLPermissions.BtoN)
    u_mmu.llc_resp_i.bits.pte :=  OrM(Dec(RegEnable(u_mmu.llc_req_o.bits(5, 3), llc.b.fire())).asBools,
                                      Spl(llc.c.bits.data, 64))

    llc.c.ready               :=  Mux(llc_c_probe_resp,
                                      u_mmu.llc_resp_i.ready,
                                      Mux(llc_c_dataless,
                                          llc_c_llc_d.ready,
                                          llc_c_mmu_a.ready))

    // llc d
    //   1. from mem d: grantData: acquireBlock.NtoB/NtoT / releaseAck: releaseData
    //   2. from prb:   grantData: acquireBlock.NtoB
    //   3. from llc c: releaseAck: release
    //   4. from llc a: grant: acquireBlock.BtoT/acquirePerm
    val mem_d_llc_d            =  dontTouch(Wire(llc.d.cloneType))
    val prb_d_llc_d            =  dontTouch(Wire(llc.d.cloneType))

    val mem_d_llc_d_q          =  Queue(mem_d_llc_d, 2).suggestName("u_mem_q")
    val prb_d_llc_d_q          =  Queue(prb_d_llc_d, 2).suggestName("u_prb_q")
    val llc_a_llc_d_q          =  Queue(llc_a_llc_d, 2).suggestName("u_llc_a_q")
    val llc_c_llc_d_q          =  Queue(llc_c_llc_d, 2).suggestName("u_llc_c_q")

    llc.d.valid               :=  mem_d_llc_d_q.valid |
                                  prb_d_llc_d_q.valid |
                                  llc_c_llc_d_q.valid |
                                  llc_a_llc_d_q.valid

    val sink_q = dontTouch(Reg(UInt(2.W)))

    llc.d.bits                :=  Mux(mem_d_llc_d_q.valid, mem_d_llc_d_q.bits,
                                  Mux(prb_d_llc_d_q.valid, prb_d_llc_d_q.bits,
                                  Mux(llc_c_llc_d_q.valid, llc_c_llc_d_q.bits,
                                                           llc_a_llc_d_q.bits)))
    llc.d.bits.sink           :=  sink_q

    mem_d_llc_d_q.ready       :=  llc.d.ready
    prb_d_llc_d_q.ready       :=  mem_d_llc_d_q.ready & ~mem_d_llc_d_q.valid
    llc_c_llc_d_q.ready       :=  prb_d_llc_d_q.ready & ~prb_d_llc_d_q.valid
    llc_a_llc_d_q.ready       :=  llc_c_llc_d_q.ready & ~llc_c_llc_d_q.valid

    // llc e
    llc.e.ready               :=  1.U

    val sink_inc = llc.d.fire() & ((llc.d.bits.opcode === TLMessages.Grant) |
                                   (llc.d.bits.opcode === TLMessages.GrantData))
    val sink_dec = llc.e.fire()

    sink_q := RegEnable(sink_q + Mux(sink_inc & ~sink_dec,
                                     1.U(2.W),
                                     3.U(2.W)),
                        sink_inc |
                        sink_dec)

    // mmu
    //   1. from llc c: releaseData
    //   2. from llc a: acquireBlock.NtoB/NtoT
    u_mmu.mmu_req_i.valid     :=  llc_c_mmu_a.valid |
                                  llc_a_mmu_a.valid

    u_mmu.mmu_req_i.bits      :=  Mux(llc_c_mmu_a.valid,
                                      llc_c_mmu_a.bits.address(q.maBits - 1, 12),
                                      llc_a_mmu_a.bits.address(q.maBits - 1, 12))

    llc_a_sel_q               :=  RegEnable(llc_a_mmu_a.valid                                    &
                                           ~llc_c_mmu_a.valid                                    &
                                           (llc_a_mmu_a.bits.opcode === TLMessages.AcquireBlock) &
                                           (llc_a_mmu_a.bits.param  === TLPermissions.NtoB), mmu_req_fire)
    mmu_a_sel_q               :=  RegEnable(llc_c_mmu_a.valid, mmu_req_fire)

    llc_c_mmu_a.ready         :=  u_mmu.mmu_req_i.ready
    llc_a_mmu_a.ready         :=  llc_c_mmu_a.ready & ~llc_c_mmu_a.valid

    // mem a
    //   1. from mmu o (ptw)
    //   2. from mmu o (done, i.e. llc)
    val mmu_a_mem_a            =  dontTouch(Wire(mem.a.cloneType))
    val llc_x_mem_a            =  dontTouch(Wire(mem.a.cloneType))

    val llc_x_mem_a_bits       =  Mux(llc_c_mmu_a.valid,
                                      llc_c_mmu_a.bits,
                                      llc_a_mmu_a.bits)
    val llc_x_mem_a_bits_q     =  RegEnable(llc_x_mem_a_bits, mmu_req_fire)

    mmu_a_mem_a.valid         :=  u_mmu.mem_req_o.valid

    mmu_a_mem_a.bits.opcode   :=  TLMessages.Get
    mmu_a_mem_a.bits.param    :=  0.U
    mmu_a_mem_a.bits.size     :=  6.U
    mmu_a_mem_a.bits.source   :=  0.U
    mmu_a_mem_a.bits.address  :=  Cat(u_mmu.mem_req_o.bits.ppa(q.paBits - 1, 6), 0.U(6.W))
    mmu_a_mem_a.bits.mask     :=  MaskGen(mmu_a_mem_a.bits.address, mmu_a_mem_a.bits.size, 64)
    mmu_a_mem_a.bits.corrupt  :=  0.U
    mmu_a_mem_a.bits.data     :=  0.U

    llc_x_mem_a.valid         :=  u_mmu.mmu_resp_o.valid

    llc_x_mem_a.bits.opcode   :=  Mux(mmu_a_sel_q,
                                      TLMessages.PutFullData,
                                      TLMessages.Get)
    llc_x_mem_a.bits.param    :=  0.U
    llc_x_mem_a.bits.size     :=  llc_x_mem_a_bits_q.size
    llc_x_mem_a.bits.source   :=  llc_x_mem_a_bits_q.source
    llc_x_mem_a.bits.address  :=  Cat(Pad(u_mmu.mmu_resp_o.bits.ppn, q.maBits - 12), llc_x_mem_a_bits_q.address(11, 0))
    llc_x_mem_a.bits.mask     :=  llc_x_mem_a_bits_q.mask
    llc_x_mem_a.bits.corrupt  :=  u_mmu.mmu_resp_o.bits.err
    llc_x_mem_a.bits.data     :=  llc_x_mem_a_bits_q.data

    mem_a_sel_q               :=  RegEnable(mmu_a_mem_a.valid, mem.a.fire())

    mem.a.valid               :=  mmu_a_mem_a.valid |
                                  llc_x_mem_a.valid
    mem.a.bits                :=  Mux(mmu_a_mem_a.valid,
                                      mmu_a_mem_a.bits,
                                      llc_x_mem_a.bits)

    mmu_a_mem_a.ready         :=  mem.a.ready
    llc_x_mem_a.ready         :=  mmu_a_mem_a.ready & ~mmu_a_mem_a.valid

    u_mmu.mem_req_o.ready     :=  mmu_a_mem_a.ready
    u_mmu.mmu_resp_o.ready    :=  llc_x_mem_a.ready

    // mem b
    mem.b.ready               :=  0.U

    // mem c
    mem.c.valid               :=  0.U

    // mem d
    //   1. to mmu o & prb
    //   2. to llc d: grantData: acquireBlock.NtoB[probe]/NtoT / releaseAck: releaseData
    u_mmu.mem_resp_i.valid    :=  mem.d.valid &  mem_a_sel_q

    u_mmu.mem_resp_i.bits.pte :=  OrM(Dec(RegEnable(u_mmu.mem_req_o.bits.ppa(5, 3), u_mmu.mem_req_o.fire())).asBools,
                                      Spl(mem.d.bits.data, 64))
    u_mmu.mem_resp_i.bits.err :=  mem.d.bits.denied |
                                  mem.d.bits.corrupt

    mem_d_llc_d.valid         :=  mem.d.valid & ~mem_a_sel_q

    mem_d_llc_d.bits.opcode   :=  Mux(mmu_a_sel_q,
                                      TLMessages.ReleaseAck,
                                      TLMessages.GrantData)
    mem_d_llc_d.bits.param    :=  Cat(0.U(1.W),  llc_a_sel_q)
    mem_d_llc_d.bits.size     :=  mem.d.bits.size
    mem_d_llc_d.bits.source   :=  mem.d.bits.source
    mem_d_llc_d.bits.sink     :=  mem.d.bits.sink
    mem_d_llc_d.bits.denied   :=  mem.d.bits.denied
    mem_d_llc_d.bits.corrupt  :=  mem.d.bits.corrupt
    mem_d_llc_d.bits.data     :=  mem.d.bits.data

    mem.d.ready               :=  Mux(mem_a_sel_q,
                                      u_mmu.mem_resp_i.ready,
                                      llc.d.ready)

    // mem e
    mem.e.valid               :=  0.U


    // ------------------------
    // prb

    if (q.prbEn >= 0) {

      val prb_q = Wire(Vec(q.ptwLvl, new MidgardPRBEntry(q, adp_node.in.head._2.bundle.sourceBits)))

      val prb_req = u_mmu.llc_req_o .fire()
      val prb_ack = u_mmu.llc_resp_i.fire()
      val prb_mem = u_mmu.mem_resp_i.fire()
      val prb_llc = llc.a.fire() & mmu_busy
      val prb_clr = prb_d_llc_d.fire()

      // indication that llc doesn't inject the acquire
      val prb_skp = prb_ack & (llc.c.bits.param =/= TLPermissions.NtoN)

      val prb_req_addr = u_mmu.llc_req_o.bits    (q.maBits - 1, 6)
      val prb_mem_addr = u_mmu.mem_req_o.bits.pma(q.maBits - 1, 6)
      val prb_llc_addr =       llc.a.bits.address(q.maBits - 1, 6)

      val prb_vld      = prb_q.map(e => e.vld)
      val prb_rdy      = prb_q.map(e => e.vld &  e.mem   &  e.llc)
      val prb_mem_sel  = prb_q.map(e => e.vld & (e.addr === prb_mem_addr))
      val prb_llc_sel  = prb_q.map(e => e.vld & (e.addr === prb_llc_addr))
      val prb_clr_sel  = PrR(prb_rdy)

      val prb_idx_q    = dontTouch(Wire(UInt(q.ptwLvl.W)))

      prb_idx_q := RegEnable(OrM(Seq(mmu_req_fire,
                                     prb_ack),
                                 Seq(1.U(q.ptwLvl.W),
                                     ShL(prb_idx_q, 1))),
                             mmu_req_fire |
                             prb_ack)

      for (i <- 0 until q.ptwLvl) {
        val set_vld = prb_req & prb_idx_q  (i)
        val set_mem = prb_mem & prb_mem_sel(i)
        val set_llc = prb_llc & prb_llc_sel(i)
        val clr     = prb_skp & prb_idx_q  (i) |
                      prb_clr & prb_clr_sel(i)

        prb_q(i).vld  := RegEnable(set_vld,        0.U(1.W), set_vld | clr)
        prb_q(i).mem  := RegEnable(set_mem & ~clr, 0.U(1.W), set_mem | clr)
        prb_q(i).llc  := RegEnable(set_llc & ~clr, 0.U(1.W), set_llc | clr)

        prb_q(i).addr := RegEnable(prb_req_addr,      set_vld)
        prb_q(i).data := RegEnable(mem.d.bits.data,   set_mem)
        prb_q(i).src  := RegEnable(llc.a.bits.source, set_llc)
      }

      prb_busy                 := prb_vld.orR
      prb_d_llc_d.valid        := prb_rdy.orR

      prb_d_llc_d.bits.opcode  := TLMessages.GrantData
      prb_d_llc_d.bits.param   := TLPermissions.toB
      prb_d_llc_d.bits.size    := 6.U
      prb_d_llc_d.bits.source  := OrM(prb_clr_sel.asBools, prb_q.map(_.src))
      prb_d_llc_d.bits.sink    := 0.U
      prb_d_llc_d.bits.denied  := 0.U
      prb_d_llc_d.bits.corrupt := 0.U
      prb_d_llc_d.bits.data    := OrM(prb_clr_sel.asBools, prb_q.map(_.data))

    } else {
      prb_busy                 := 0.U

      prb_d_llc_d.valid        := 0.U
      prb_d_llc_d.bits         := DontCare
    }
  }
}


trait HasMidgard { this: BaseSubsystem =>

  val u_mmu = LazyModule(new MidgardTLWrapper())

  // width modification
  u_mmu.node := cbus.coupleTo("u_mmu") {
    TLFragmenter(8, 64) := _
  }

  // export
  val u_mmu_node = u_mmu.adp_node

  LogicalModuleTree.add(logicalTreeNode, u_mmu.logicalTreeNode)
}
