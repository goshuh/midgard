package midgard

import  chisel3._
import  chisel3.util._
import  chisel3.stage._

import  midgard.util._

import  firrtl._
import  firrtl.options._
import  firrtl.stage._


package frontside {

  // simple frontside wrapper
  class DUT(val P: Param) extends Module {

    // --------------------------
    // io

    val Q = P.copy(tlbEn = true)

    val ilb_req_i  = IO(Vec(2, Flipped(    Valid(new VLBReq(Q)))))
    val ilb_res_o  = IO(Vec(2,             Valid(new VLBRes(Q))))
    val ilb_ttw_o  = IO(                   Valid(new VLBRes(Q)))
    val ilb_kill_i = IO(                   Input(UInt(3.W)))
    val ilb_busy_o = IO(                  Output(Bool()))

    val dlb_req_i  = IO(Vec(2, Flipped(    Valid(new VLBReq(P)))))
    val dlb_res_o  = IO(Vec(2,             Valid(new VLBRes(P))))
    val dlb_ttw_o  = IO(                   Valid(new VLBRes(P)))
    val dlb_kill_i = IO(                   Input(UInt(3.W)))
    val dlb_busy_o = IO(                  Output(Bool()))

    val vtd_req_i  = IO(                   Input(new VTDReq(P)))

    val mem_req_o  = IO(               Decoupled(new MemReq(P)))
    val mem_res_i  = IO(       Flipped(Decoupled(new MemRes(P))))

    val satp_i     = IO(                   Input(UInt(64.W)))
    val uatp_i     = IO(                   Input(UInt(64.W)))
    val uatc_i     = IO(                   Input(new VSCCfg()))


    // --------------------------
    // inst

    val u_ilb = Module(new VLB(Q, 2))
    val u_dlb = Module(new VLB(P, 2))
    val u_ttw = Module(new VSC(P))

    // the tb doesn't ever need the ready
    u_ilb.vlb_req_i(0).valid := ilb_req_i(0).valid
    u_ilb.vlb_req_i(0).bits  := ilb_req_i(0).bits
    u_ilb.vlb_req_i(1).valid := ilb_req_i(1).valid
    u_ilb.vlb_req_i(1).bits  := ilb_req_i(1).bits
    u_ilb.vlb_ttw_o          <> ilb_ttw_o
    u_ilb.ttw_req_o          <> u_ttw.vlb_req_i(0)
    u_ilb.ttw_res_i          <> u_ttw.vlb_res_o(0)
    u_ilb.ttw_ext_i          := u_ttw.vlb_ext_o(0)
    u_ilb.vtd_req_i          := vtd_req_i
    u_ilb.satp_i             := satp_i
    u_ilb.uatp_i             := uatp_i
    u_ilb.uatc_i             := uatc_i
    u_ilb.kill_i             := ilb_kill_i
    u_ilb.kill_asid_i        := satp_i(44 :+ P.asidBits)
    u_ilb.kill_sdid_i        := uatp_i(48 :+ P.sdidBits)

    ilb_res_o(0).valid       := u_ilb.vlb_res_o(0).valid
    ilb_res_o(0).bits        := u_ilb.vlb_res_o(0).bits
    ilb_res_o(1).valid       := u_ilb.vlb_res_o(1).valid
    ilb_res_o(1).bits        := u_ilb.vlb_res_o(1).bits

    u_dlb.vlb_req_i(0).valid := dlb_req_i(0).valid
    u_dlb.vlb_req_i(0).bits  := dlb_req_i(0).bits
    u_dlb.vlb_req_i(1).valid := dlb_req_i(1).valid
    u_dlb.vlb_req_i(1).bits  := dlb_req_i(1).bits
    u_dlb.vlb_ttw_o          <> dlb_ttw_o
    u_dlb.ttw_req_o          <> u_ttw.vlb_req_i(1)
    u_dlb.ttw_res_i          <> u_ttw.vlb_res_o(1)
    u_dlb.ttw_ext_i          := u_ttw.vlb_ext_o(1)
    u_dlb.vtd_req_i          := vtd_req_i
    u_dlb.satp_i             := satp_i
    u_dlb.uatp_i             := uatp_i
    u_dlb.uatc_i             := uatc_i
    u_dlb.kill_i             := dlb_kill_i
    u_dlb.kill_asid_i        := satp_i(44 :+ P.asidBits)
    u_dlb.kill_sdid_i        := uatp_i(48 :+ P.sdidBits)

    dlb_res_o(0).valid       := u_dlb.vlb_res_o(0).valid
    dlb_res_o(0).bits        := u_dlb.vlb_res_o(0).bits
    dlb_res_o(1).valid       := u_dlb.vlb_res_o(1).valid
    dlb_res_o(1).bits        := u_dlb.vlb_res_o(1).bits

    u_ttw.vtd_req_i          := vtd_req_i
    u_ttw.mem_req_o          <> mem_req_o
    u_ttw.mem_res_i          <> mem_res_i
    u_ttw.satp_i             := satp_i
    u_ttw.uatp_i             := uatp_i
    u_ttw.uatc_i             := uatc_i

    // for tb
    ilb_busy_o               := Non(u_ttw.idle_o(0))
    dlb_busy_o               := Non(u_ttw.idle_o(1))
  }
}


object Main extends App {

  def emit(gen: () => Module, param: Array[String]): Unit = {
    new ChiselStage().execute(param, Seq(
      ChiselGeneratorAnnotation(gen),
      CustomDefaultRegisterEmission(
        useInitAsPreset      = false,
        disableRandomization = true
      ),
      RunFirrtlTransformAnnotation(Dependency[RenameExtModule])
    ))
  }

  val p = Param(
    en      = true,

    vaBits  = 64,
    maBits  = 64,
    paBits  = 48,
    clBits  = 512,

    pmtBits = 16,

    tlbEn   = false,
    tlbWays = 32,

    vlbIdx  = 6,
    vlbWays = 16,

    ttwNum  = 2,

    vscEn   = true,
    vscSets = 32,
    vscWays = 4,

    vtdSets = 1,
    vtdWays = 1,
    dirBits = 1,

    llcIdx  = 3,

    mlbEn   = true,
    mlbWays = 32,

    ptcEn   = true,
    ptcWays = 32,

    mrqWays = 4,

    ctlBase = 0x11000000,
    ctlSize = 0x40,

    dbg     = false
  )

  new java.io.File("gen").mkdir()

  val param = Array(
    "-X",           "sverilog",
    "--target-dir", "gen"
  )

  emit(() => new frontside.DUT(p), param)
  emit(() => new  backside.MMU(p), param)

  println("done.")
}
