package midgard


import java.io.File

import chisel3.stage._


object Main extends App {

  val p = MidgardParam(
    maBits    = 64,
    paBits    = 48,
    tlbEn     = 1,
    tlbSetNum = 1024,
    tlbWayNum = 4,
    ptcEn     = 1,
    ptcNum    = 32
  )

  new File("gen").mkdir()

  new ChiselStage().emitVerilog(new MidgardMMU(p), Array("--target-dir", "gen"))
}
