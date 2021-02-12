package midgard


import java.io.File

import chisel3.stage._


object Main extends App {

  val p = MidgardParam(
    maBits    = 32,
    paBits    = 32,
    tlbEn     = 1,
    tlbSetNum = 1024,
    tlbWayNum = 4,
    ptcEn     = 0, // ?
    ptcNum    = 32,
    prbEn     = 1,
    cfgBase   = 0x11000000,
    cfgSize   = 0x40
  )

  new File("gen").mkdir()

  new ChiselStage().emitVerilog(new MidgardMMU(p), Array("--target-dir", "gen"))
}
