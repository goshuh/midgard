package midgard

import chisel3._
import chisel3.util._

import midgard.misc._


class MidgardTLBEntry(val p: MidgardParam) extends Bundle {
  val vld = UInt(1.W)
  val err = UInt(1.W)
  val lvl = UInt(3.W)
  val mpn = UInt((p.maBits - 12 - log2(p.tlbSetNum)).W)
  val ppn = UInt((p.paBits - 12                    ).W)
}

object MidgardTLBEntry {
  def apply(p: MidgardParam) = {
    val tlb = Wire(new MidgardTLBEntry(p))

    tlb.vld := 0.U(1.W)
    tlb.err := DontCare
    tlb.lvl := DontCare
    tlb.mpn := DontCare
    tlb.ppn := DontCare

    tlb
  }
  def apply(p: MidgardParam, e: UInt, l: UInt, m: UInt, r: UInt): MidgardTLBEntry = {
    val tlb = Wire(new MidgardTLBEntry(p))

    tlb.vld := 1.U(1.W)
    tlb.err := e
    tlb.lvl := l
    tlb.mpn := m
    tlb.ppn := r(p.paBits - 1, 12)

    tlb
  }
}


class MidgardTLB(p: MidgardParam) extends MultiIOModule {

  // --------------------------
  // params

  val tlbTagHi =  p.maBits - 13
  val tlbTagLo =  log2(p.tlbSetNum)

  val tlbSetHi =  tlbTagLo - 1
  val tlbSetLo =  0

  val ptwLvl   = (p.maBits - 12 + (9      - 1)) / 9
  val ptwTop   =  p.maBits - 12 - (ptwLvl - 1)  * 9


  // --------------------------
  // io

  val tlb_req_i  = IO(Flipped(Decoupled(UInt((p.maBits - 12).W))))
  val tlb_resp_o = IO(        Decoupled(new MidgardMMUResp(p)))

  val ptw_req_o  = IO(        Decoupled(UInt((p.maBits - 12).W)))
  val ptw_resp_i = IO(Flipped(Decoupled(new MidgardTLBEntry(p))))


  // --------------------------
  // main

  val tlb_req_fire  = tlb_req_i.fire()
  val tlb_resp_fire = tlb_resp_o.fire()

  val ptw_req_fire  = ptw_req_o.fire()
  val ptw_resp_fire = ptw_resp_i.fire()

  // common
  val rst_done = dontTouch(Wire(UInt(1.W)))

  val tlb_busy = dontTouch(Wire(UInt(1.W)))
  val tlb_data =           Wire(new MidgardTLBEntry(p))

  val hit_s2   = dontTouch(Wire(UInt(1.W)))
  val mis_s2   = dontTouch(Wire(UInt(1.W)))
  val mpn_s2   = dontTouch(Wire(UInt((p.maBits - 12).W)))

  if (p.tlbEn != 0) {

    // init reset
    val rst_q = dontTouch(RegInit(UInt((tlbTagLo + 1).W), 0.U((tlbTagLo + 1).W)))

    when (~rst_done) {
      rst_q := rst_q + 1.U((tlbTagLo + 1).W)
    }

    val ren_s0   = tlb_req_fire
    val ren_s1_q = dontTouch(RegNext(ren_s0,   0.U))
    val ren_s2_q = dontTouch(RegNext(ren_s1_q, 0.U))

    val mpn_s0   = tlb_req_i.bits
    val mpn_s1_q = dontTouch(RegEnable(mpn_s0,   ren_s0))
    val mpn_s2_q = dontTouch(RegEnable(mpn_s1_q, ren_s1_q))

    val raddr_s1 = mpn_s1_q(tlbSetHi, tlbSetLo)
    val rdata_s2 = dontTouch(Wire(Vec(p.tlbWayNum, new MidgardTLBEntry(p))))

    val wen      = dontTouch(Wire(UInt(1.W)))
    val wsel     = dontTouch(Wire(UInt(p.tlbWayNum.W)))
    val waddr    = dontTouch(Wire(UInt(tlbTagLo.W)))
    val wdata    =           Wire(new MidgardTLBEntry(p))

    // mem model
    for (i <- 0 until p.tlbWayNum) {
      val mem = SyncReadMem(p.tlbSetNum, new MidgardTLBEntry(p))

      rdata_s2(i) := DontCare

      // single port
      when (ren_s1_q | wen & wsel(i)) {
        val port = mem(Mux(ren_s1_q,
                           raddr_s1,
                           waddr))

        when (ren_s1_q) {
          rdata_s2(i) := port
        } .otherwise {
          port := wdata
        }
      }

      when (wen & wsel(i) & rst_done) {
        printf(p"TLB w ${Hexadecimal(Cat(wdata.mpn, waddr))}: s${Hexadecimal(waddr)} w${i}: ${Hexadecimal(wdata.err)} ${Hexadecimal(wdata.ppn)}\n")
      }
    }

    // hit check
    val mpn_set_s2 = mpn_s2_q(tlbSetHi, tlbSetLo)
    val mpn_tag_s2 = mpn_s2_q(tlbTagHi, tlbTagLo)

    val vld_way_s2 = rdata_s2.map(_.vld)
    val hit_way_s2 = rdata_s2.map(e => {
      e.vld & e.mpn === mpn_tag_s2
    })

    val hit_raw_s2 = hit_way_s2.orR

    // TODO: random replacement
    val rnd_q = dontTouch(RegInit(UInt(16.W), 1.U))

    when (mis_s2) {
      rnd_q := Ran(rnd_q)
    }

    val rpl_way_s2 = Mux(vld_way_s2.andR,
                         Dec( rnd_q(log2(p.tlbWayNum) - 1, 0)),
                         Pri(~vld_way_s2))

    // write
    wen   := ptw_resp_fire | ~rst_done

    wsel  := Mux(rst_done, RegEnable(rpl_way_s2, mis_s2), ~0.U(p.tlbWayNum.W))
    waddr := Mux(rst_done, RegEnable(mpn_set_s2, mis_s2),  rst_q(tlbSetHi, tlbSetLo))
    wdata := Mux(rst_done, ptw_resp_i.bits,                MidgardTLBEntry(p))

    // output
    rst_done := rst_q(tlbTagLo)

    tlb_busy := ren_s1_q |  ren_s2_q
    tlb_data := OrM(hit_way_s2,
                   rdata_s2.map(_.asUInt)).asTypeOf(new MidgardTLBEntry(p))

    hit_s2   := ren_s2_q &  hit_raw_s2
    mis_s2   := ren_s2_q & ~hit_raw_s2
    mpn_s2   := mpn_s2_q

    when (ren_s2_q) {
      when (hit_raw_s2) {
        assert(Pri(hit_way_s2) === hit_way_s2)
        printf(p"TLB h ${Hexadecimal(mpn_s2_q)}: s${Hexadecimal(mpn_set_s2)} w${Hexadecimal(Enc(hit_way_s2))}: ${Hexadecimal(tlb_data.err)} ${Hexadecimal(tlb_data.ppn)}\n")
      } .otherwise {
        printf(p"TLB m ${Hexadecimal(mpn_s2_q)}\n")
      }
    }

  } else {

    rst_done := 1.U

    tlb_busy := 0.U
    tlb_data := 0.U.asTypeOf(new MidgardTLBEntry(p))

    hit_s2   := 0.U
    mis_s2   := tlb_req_fire
    mpn_s2   := tlb_req_i.bits
  }

  val ptw_pend_q = dontTouch(RegEnable(mis_s2,       0.U(1.W), ptw_req_fire | mis_s2))
  val ptw_busy_q = dontTouch(RegEnable(ptw_req_fire, 0.U(1.W), ptw_req_fire | ptw_resp_fire))

  // huge page
  val tlb_mux = Mux(ptw_busy_q,
                    ptw_resp_i.bits,
                    tlb_data)

  val tlb_mpn = Cat(tlb_mux.mpn,
                    Mux(ptw_busy_q,
                        ptw_req_o.bits(tlbSetHi, tlbSetLo),
                        mpn_s2        (tlbSetHi, tlbSetLo)))

  val tlb_ppn = OrM(Dec(tlb_mux.lvl).asBools.slice(0, ptwLvl),
                    Seq.tabulate(ptwLvl)(n => {
                      val h = p.maBits - ptwTop - n * 9

                      if (h >= p.paBits)
                        0.U((p.paBits - 12).W)
                      else if (h <= 12)
                        tlb_mux.ppn
                      else
                        Cat(tlb_mux.ppn(p.paBits - 13, h - 12),
                            tlb_mpn    (h        - 13, 0))
                    }))

  val tlb_resp_vld = hit_s2 |
                     ptw_resp_fire
  val tlb_resp_nxt = MidgardMMUResp(p, tlb_mux.err, tlb_ppn)


  // --------------------------
  // output

  // TODO: pipeline
  tlb_req_i.ready  := rst_done   &
                     ~tlb_busy   &
                     ~ptw_pend_q &
                     ~ptw_busy_q &
                     ~tlb_resp_o.valid

  tlb_resp_o.valid := RegEnable(tlb_resp_vld, 0.U(1.W), tlb_resp_vld | tlb_resp_fire)
  tlb_resp_o.bits  := RegEnable(tlb_resp_nxt,           tlb_resp_vld)

  ptw_req_o.valid  := ptw_pend_q
  ptw_req_o.bits   := RegEnable(mpn_s2, mis_s2)

  ptw_resp_i.ready := ptw_busy_q
}