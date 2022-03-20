package midgard

import  java.io.File

import  chisel3._
import  chisel3.util._
import  chisel3.stage._

import  midgard.util._


package frontside {

  // simple frontside wrapper
  class FST(val P: Param) extends Module {

    // --------------------------
    // io

    val ilb_req_i  = IO(Flipped(    Valid(new VLBReq (P))))
    val ilb_resp_o = IO(            Valid(new VLBResp(P)))
    val ilb_fill_o = IO(            Valid(new VLBResp(P)))
    val ilb_kill_i = IO(            Input(UInt(3.W)))
    val ilb_busy_o = IO(           Output(Bool()))

    val dlb_req_i  = IO(Flipped(    Valid(new VLBReq (P))))
    val dlb_resp_o = IO(            Valid(new VLBResp(P)))
    val dlb_fill_o = IO(            Valid(new VLBResp(P)))
    val dlb_kill_i = IO(            Input(UInt(3.W)))
    val dlb_busy_o = IO(           Output(Bool()))

    val mem_req_o  = IO(        Decoupled(new MemReq (P)))
    val mem_resp_i = IO(Flipped(Decoupled(new MemResp(P))))

    val satp_i     = IO(            Input(UInt(64.W)))


    // --------------------------
    // inst

    val u_ilb = Module(new VLB(P))
    val u_dlb = Module(new VLB(P))
    val u_ptw = Module(new PTW(P, 2))

    val ilb_ptw_req   = u_ilb.ptw_req_o.fire()
    val ilb_ptw_resp  = u_ilb.ptw_req_o.bits.kill || u_ilb.ptw_resp_i.fire()
    val dlb_ptw_req   = u_dlb.ptw_req_o.fire()
    val dlb_ptw_resp  = u_dlb.ptw_req_o.bits.kill || u_dlb.ptw_resp_i.fire()

    u_ilb.vlb_req_i  <> ilb_req_i
    u_ilb.vlb_resp_o <> ilb_resp_o
    u_ilb.vlb_fill_o <> ilb_fill_o
    u_ilb.ptw_req_o  <> u_ptw.vlb_req_i (0)
    u_ilb.ptw_resp_i <> u_ptw.vlb_resp_o(0)
    u_ilb.asid_i     := satp_i(60 :- P.asidBits)
    u_ilb.kill_i     := ilb_kill_i

    u_dlb.vlb_req_i  <> dlb_req_i
    u_dlb.vlb_resp_o <> dlb_resp_o
    u_dlb.vlb_fill_o <> dlb_fill_o
    u_dlb.ptw_req_o  <> u_ptw.vlb_req_i (1)
    u_dlb.ptw_resp_i <> u_ptw.vlb_resp_o(1)
    u_dlb.asid_i     := satp_i(60 :- P.asidBits)
    u_dlb.kill_i     := dlb_kill_i

    u_ptw.mem_req_o  <> mem_req_o
    u_ptw.mem_resp_i <> mem_resp_i
    u_ptw.satp_i     := Ext(satp_i(44.W), P.mpnBits) ## 0.U(6.W)

    ilb_busy_o       := RegEnable(ilb_ptw_req && !ilb_ptw_resp, false.B, ilb_ptw_req || ilb_ptw_resp)
    dlb_busy_o       := RegEnable(dlb_ptw_req && !dlb_ptw_resp, false.B, dlb_ptw_req || dlb_ptw_resp)
  }
}


object Main extends App {

  val p = Param(
    vaBits    = 64,
    maBits    = 64,
    paBits    = 48,
    clBits    = 512,

    llcIdx    = 3,

    vlbIdx    = 6,
    vlbWays   = 16,

    mlbEn     = true,
    mlbSets   = 1024,
    mlbWays   = 4,

    ptcEn     = true,
    ptcWays   = Seq(1, 2, 4, 16, 16, 16),

    mrqWays   = 4,

    prbEn     = true,

    ctlBase   = 0x11000000,
    ctlSize   = 0x40,

    dbg       = false
  )

  new File("gen").mkdir()

  val stage = new ChiselStage()
  val param = Array("--target-dir", "gen")

  stage.emitVerilog(new frontside.FST(p), param)
  stage.emitVerilog(new  backside.MMU(p), param)
}