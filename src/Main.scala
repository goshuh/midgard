package midgard

import  java.io.File
import  chisel3.stage._


object Main extends App {

  val p = Param(
    vaBits    = 64,
    maBits    = 64,
    paBits    = 48,
    clBits    = 512,

    llcIdx    = 3,

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

  new ChiselStage().emitVerilog(new backside.MMU(p), Array("--target-dir", "gen"))
}