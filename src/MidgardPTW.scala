package midgard

import chisel3._
import chisel3.util._

import midgard.misc._


class MidgardPTCEntry(val p: MidgardParam) extends Bundle {
  val vld = UInt( 1.W)
  val pma = UInt((p.maBits - 12).W)
  val pte = UInt(64.W)
}


class MidgardPTW(p: MidgardParam) extends MultiIOModule {

  // --------------------------
  // io

  val ptw_req_i  = IO(Flipped(Decoupled(UInt((p.maBits - 12).W))))
  val ptw_resp_o = IO(        Decoupled(new MidgardTLBEntry(p)))

  val llc_req_o  = IO(        Decoupled(UInt(p.maBits.W)))
  val llc_resp_i = IO(Flipped(Decoupled(new MidgardLLCResp())))

  val mem_req_o  = IO(        Decoupled(new MidgardMEMReq(p)))
  val mem_resp_i = IO(Flipped(Decoupled(new MidgardMEMResp())))

  val cfg_i      = IO(            Input(Vec(p.ptwLvl + 1, UInt((p.maBits).W))))


  // --------------------------
  // logic

  val fsm_idle :: fsm_req :: fsm_resp :: fsm_null = Enum(4)

  val ptw_req_fire  = ptw_req_i.fire()
  val ptw_resp_fire = ptw_resp_o.fire()

  val llc_req_fire  = llc_req_o.fire()
  val llc_resp_fire = llc_resp_i.fire()
  val llc_resp_hit  = llc_resp_i.bits.hit

  val mem_req_fire  = mem_req_o.fire()
  val mem_resp_fire = mem_resp_i.fire()
  val mem_resp_err  = mem_resp_i.bits.err


  // --------------------------
  // schedule

  // states
  val vld_q = dontTouch(Wire(UInt(1.W)))
  val dir_q = dontTouch(Wire(UInt(1.W)))
  val mpn_q = dontTouch(Wire(UInt((p.maBits - 12).W)))

  val lvl_q = dontTouch(Reg (UInt(p.ptwLvl.W)))
  val pma_q = dontTouch(Reg (UInt((p.maBits - 3).W)))
  val ppa_q = dontTouch(Reg (UInt((p.paBits - 3).W)))

  val lvl_top = lvl_q(0)
  val lvl_bot = lvl_q(p.ptwLvl - 1)
  val lvl_inv = OrM(lvl_q.asBools,
                    Seq.tabulate(p.ptwLvl)(n => {
                      if ((p.maBits - p.ptwTop - n * 9) >= p.paBits)
                        1.U(1.W)
                      else
                        0.U(1.W)
                    }))

  val llc_vld = vld_q & ~dir_q
  val mem_vld = vld_q &  dir_q

  //   |    | d |    lvl |  pma                            |  msk/mpa                     |  if hit/blk
  //
  //   | l6 | 0 | 100000 | {cfg6[63:55], mpn[tt:l6], 3'b0) | {52'b0            }          |  -> idle
  // l | l5 | 0 | 010000 | {cfg5[63:46], mpn[tt:l5], 3'b0} | {43'b0, { 9{1'b1}}}          |  -> l6
  // l | l4 | 0 | 001000 | {cfg4[63:37], mpn[tt:l4], 3'b0} | {34'b0, {18{1'b1}}}          |  -> l5
  // c | l3 | 0 | 000100 | {cfg3[63:28], mpn[tt:l3], 3'b0} | {25'b0, {27{1'b1}}}          |  -> l4
  //   | l2 | 0 | 000010 | {cfg2[63:19], mpn[tt:l2], 3'b0} | {16'b0, {36{1'b1}}}          |  -> l3
  //   | l1 | 0 | 000001 | {cfg1[63:10], mpn[tt   ], 3'b0} | { 7'b0, {45{1'b1}}}          |  -> l2
  //
  //   | l1 | 1 | 000001 | {cfg1[63:10], mpn[tt   ], 3'b0} | {cfg0[47:10], mpn[tt], 3'b0} |  NA
  // m | l2 | 1 | 000010 | {cfg2[63:19], mpn[tt:l2], 3'b0} | {pte1[47:12], mpn[l2], 3'b0} |  NA
  // e | l3 | 1 | 000100 | {cfg3[63:28], mpn[tt:l3], 3'b0} | {pte2[47:12], mpn[l3], 3'b0} | {pte3[47:39], mpn[l4:l6]}
  // m | l4 | 1 | 001000 | {cfg4[63:37], mpn[tt:l4], 3'b0} | {pte3[47:12], mpn[l4], 3'b0} | {pte4[47:30], mpn[l5:l6]}
  //   | l5 | 1 | 010000 | {cfg5[63:46], mpn[tt:l5], 3'b0} | {pte4[47:12], mpn[l5], 3'b0} | {pte5[47:21], mpn[   l6]}
  //   | l6 | 1 | 100000 | {cfg6[63:55], mpn[tt:l6], 3'b0) | {pte5[47:12], mpn[l6], 3'b0} |  pte6[47:12]

  // forward decl
  val ptw_resp_vld = dontTouch(Wire(UInt(1.W)))

  val llc_start    = dontTouch(Wire(UInt(1.W)))
  val llc_step     = dontTouch(Wire(UInt(1.W)))
  val llc_stop     = dontTouch(Wire(UInt(1.W)))
  val llc_mem      = dontTouch(Wire(UInt(1.W)))
  val llc_end      = dontTouch(Wire(UInt(1.W)))
  val llc_ppn      = dontTouch(Wire(UInt((p.paBits - 12).W)))

  val mem_start    = dontTouch(Wire(UInt(1.W)))
  val mem_step     = dontTouch(Wire(UInt(1.W)))
  val mem_stop     = dontTouch(Wire(UInt(1.W)))
  val mem_ppn      = dontTouch(Wire(UInt((p.paBits - 12).W)))

  vld_q := RegEnable(ptw_req_fire, 0.U(1.W), ptw_req_fire | ptw_resp_vld)
  dir_q := RegEnable(mem_start,              llc_start    | mem_start)
  mpn_q := RegEnable(ptw_req_i.bits,         llc_start)

  val llc_ror = llc_step & ~llc_stop & ~lvl_top
  val llc_rol = mem_step & ~mem_stop & ~lvl_bot |
                llc_mem

  val lvl_nxt = OrM(Seq(llc_ror,
                        llc_rol,
                       ~llc_ror & ~llc_rol), // avoid toggling (llc_end)
                    Seq(RoR(lvl_q, 1),
                        RoL(lvl_q, 1),
                            lvl_q))

  when (llc_start | llc_step | mem_step) {
    lvl_q := Mux(llc_start,
                 Cat(1.U(1.W), 0.U((p.ptwLvl - 1).W)),
                 lvl_nxt)
  }

  // construct pte's ma 1 cycle earlier: for ptc timing
  val pma_1st = Cat(cfg_i(p.ptwLvl)(p.maBits - 1, p.maBits - 9),
                    ptw_req_i.bits)

  val pma_nxt = OrM(lvl_nxt(p.ptwLvl - 1, 0).asBools,
                    Seq.tabulate(p.ptwLvl)(n => {
                      val s = (p.ptwLvl - n) * 9

                      Cat(cfg_i(n + 1)(p.maBits - 1, p.maBits - s),
                          mpn_q(p.maBits - 13, s - 9))
                    }))

  when (llc_start | llc_ror | llc_rol) {
    pma_q := Mux(llc_start,
                 pma_1st,
                 pma_nxt)
  }

  // construct pte's pa 1 cycle earlier
  val ppa_mux = OrM(lvl_nxt(p.ptwLvl - 1, 1).asBools,
                    Seq.tabulate(p.ptwLvl - 1)(n => {
                      val s = (p.ptwLvl - n) * 9

                      mpn_q(s - 10, s - 18)
                    }))

  val ppa_1st = Mux(llc_end,
                    Cat(cfg_i(0)(p.paBits - 1, p.ptwTop + 3),
                        mpn_q(p.maBits - 13, p.maBits - p.ptwTop - 12)),
                    Cat(llc_ppn,
                        ppa_mux))

  val ppa_nxt = Cat(mem_ppn,
                    ppa_mux)

  when (mem_start | mem_step) {
    ppa_q := Mux(mem_start,
                 ppa_1st,
                 ppa_nxt)
  }


  // --------------------------
  // page table cache

  val ptc_hit_raw = dontTouch(Wire(UInt( 1.W)))
  val ptc_pte_raw = dontTouch(Wire(UInt(64.W)))
  // only for debug
  val ptc_ren = Wire(UInt(1.W))

  if (p.ptcEn != 0) {
    val ptc_q = dontTouch(Wire(Vec(p.ptcNum, new MidgardPTCEntry(p))))

    val ptc_rpl_q   = dontTouch(RegInit(UInt(16.W), 1.U(16.W)))
    val ptc_rpl_sel = Dec(ptc_rpl_q(log2(p.ptcNum) - 1, 0))

    val ptc_rpl_vld = mem_resp_fire & ~mem_resp_err & lvl_inv

    when (ptc_rpl_vld) {
      ptc_rpl_q := Ran(ptc_rpl_q)
    }

    for (i <- 0 until p.ptcNum) {
      val set = ptc_rpl_vld & ptc_rpl_sel(i)

      // flop array
      ptc_q(i).vld := RegEnable(1.U, 0.U,            set)
      ptc_q(i).pma := RegEnable(pma_q,               set)
      ptc_q(i).pte := RegEnable(mem_resp_i.bits.pte, set)

      when (set) {
        printf(p"PTC w ${Hexadecimal(pma_q)}: w${i} ${Hexadecimal(mem_resp_i.bits.pte)}\n")
      }
    }

    val ptc_hit_way = ptc_q.map(e => {
      e.vld & (e.pma === pma_q)
    })

    ptc_hit_raw := ptc_hit_way.orR
    ptc_pte_raw := OrM(ptc_hit_way,
                       ptc_q.map(_.pte))

    when (ptc_ren) {
      when (ptc_hit_raw) {
        assert(PrR(ptc_hit_way) === ptc_hit_way)
        printf(p"PTC h ${Hexadecimal(pma_q)}: w${Hexadecimal(Enc(ptc_hit_way))} ${Hexadecimal(ptc_pte_raw)}\n")
      } .otherwise {
        printf(p"PTC m ${Hexadecimal(pma_q)}\n")
      }
    }

  } else {
    ptc_hit_raw := 0.U
    ptc_pte_raw := 0.U
  }

  val ptc_hit = llc_vld & ptc_hit_raw


  // --------------------------
  // llc

  val llc_pte = Mux(ptc_hit,
                    ptc_pte_raw,
                    llc_resp_i.bits.pte)

  val llc_pte_v   = llc_pte(0)
  val llc_pte_b   = llc_pte(3, 1).orR()

  val llc_pte_mem = llc_pte_v & ~llc_pte_b & ~lvl_bot
  val llc_pte_vld = llc_pte_v &  llc_pte_b & ~lvl_inv
  val llc_pte_inv = llc_pte_v & ~llc_pte_b &  lvl_bot |
                    llc_pte_v &  llc_pte_b &  lvl_inv |
                   ~llc_pte_v

  val llc_fsm_en  = dontTouch(Wire(UInt(1.W)))
  val llc_fsm_nxt = dontTouch(Wire(UInt(2.W)))
  val llc_fsm_q   = dontTouch(RegEnable(llc_fsm_nxt, fsm_idle, llc_fsm_en))

  // only for debug
  ptc_ren := (llc_fsm_q === fsm_idle) & llc_vld

  llc_fsm_en  := 0.U
  llc_fsm_nxt := llc_fsm_q

  // the whole fsm may repeat at most `ptwLvl` times if ptc is enabled
  switch (llc_fsm_q) {
    is (fsm_idle) {
      llc_fsm_en  := llc_vld
      llc_fsm_nxt := Mux(ptc_hit_raw,
                         fsm_idle,
                         fsm_req)
    }
    is (fsm_req)  {
      llc_fsm_en  := llc_req_fire
      llc_fsm_nxt := fsm_resp
    }
    is (fsm_resp) {
      llc_fsm_en  := llc_resp_fire
      llc_fsm_nxt := Mux(p.ptcEn.U(1.W),
                         fsm_idle,
                         Mux(llc_resp_hit | lvl_top,
                             fsm_idle,
                             fsm_req))
    }
  }

  val llc_hit   = ptc_hit |
                  llc_resp_fire & llc_resp_hit

  val llc_succ  = llc_hit & llc_pte_vld
  val llc_fail  = llc_hit & llc_pte_inv

  // output
  llc_start := ptw_req_fire
  llc_step  := ptc_hit  |
               llc_resp_fire
  llc_stop  := llc_succ |
               llc_fail |
               llc_mem  |
               llc_end

  llc_mem   := llc_hit & llc_pte_mem
  llc_end   := llc_resp_fire & ~llc_resp_hit & lvl_top

  llc_ppn   := llc_pte(p.paBits - 1, 12)


  // --------------------------
  // mem

  val mem_pte_v   = mem_resp_i.bits.pte(0)
  val mem_pte_b   = mem_resp_i.bits.pte(3, 1).orR()

  val mem_pte_vld = mem_pte_v &  mem_pte_b & ~lvl_inv
  val mem_pte_inv = mem_pte_v & ~mem_pte_b &  lvl_bot |
                    mem_pte_v &  mem_pte_b &  lvl_inv |
                   ~mem_pte_v

  val mem_fsm_en  = dontTouch(Wire(UInt(1.W)))
  val mem_fsm_nxt = dontTouch(Wire(UInt(2.W)))
  val mem_fsm_q   = dontTouch(RegEnable(mem_fsm_nxt, fsm_idle, mem_fsm_en))

  mem_fsm_en  := 0.U
  mem_fsm_nxt := mem_fsm_q

  // the req/resp loop may repeat at most `ptwLvl` times
  switch (mem_fsm_q) {
    is (fsm_idle) {
      mem_fsm_en  := mem_start
      mem_fsm_nxt := fsm_req
    }
    is (fsm_req)  {
      mem_fsm_en  := mem_req_fire
      mem_fsm_nxt := fsm_resp
    }
    is (fsm_resp) {
      mem_fsm_en  := mem_resp_fire
      mem_fsm_nxt := Mux(lvl_bot | mem_resp_err | mem_pte_vld | mem_pte_inv,
                         fsm_idle,
                         fsm_req)
    }
  }

  val mem_err  = mem_resp_fire &  mem_resp_err
  val mem_succ = mem_resp_fire & ~mem_resp_err & mem_pte_vld
  val mem_fail = mem_resp_fire & ~mem_resp_err & mem_pte_inv

  // output
  mem_start := llc_mem  |
               llc_end
  mem_step  := mem_resp_fire
  mem_stop  := mem_err  |
               mem_succ |
               mem_fail

  mem_ppn   := mem_resp_i.bits.pte(p.paBits - 1, 12)


  // --------------------------
  // resp

  ptw_resp_vld := llc_succ |
                  llc_fail |
                  mem_succ |
                  mem_fail |
                  mem_err

  val ptw_resp  = MidgardTLBEntry(p,
                    llc_fail |
                    mem_fail |
                    mem_err,
                    Enc(lvl_q),
                    mpn_q(p.maBits - 13, log2(p.tlbSetNum)),
                    OrM(Seq(llc_succ | llc_fail,
                            mem_succ | mem_fail,
                            mem_err),
                        Seq(llc_pte,
                            mem_resp_i.bits.pte,
                            0.U)))


  // --------------------------
  // output

  llc_req_o.valid  :=  llc_fsm_q(0)
  llc_req_o.bits   :=  Cat(pma_q, 0.U(3.W))
  llc_resp_i.ready :=  llc_fsm_q(1)

  mem_req_o.valid  :=  mem_fsm_q(0)
  mem_req_o.bits   :=  MidgardMEMReq(p, Cat(pma_q, 0.U(3.W)), Cat(ppa_q, 0.U(3.W)))
  mem_resp_i.ready :=  mem_fsm_q(1)

  ptw_req_i.ready  := ~vld_q & ~ptw_resp_o.valid

  ptw_resp_o.valid :=  RegEnable(ptw_resp_vld, 0.U(1.W), ptw_resp_vld | ptw_resp_fire)
  ptw_resp_o.bits  :=  RegEnable(ptw_resp,               ptw_resp_vld)
}