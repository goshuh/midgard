package midgard.backside

import  chisel3._
import  chisel3.util._
import  midgard._
import  midgard.util._


class LLCReq(val P: Param) extends Bundle {
  val mcn  = UInt(P.mcnBits.W)
}

class LLCRes(val P: Param) extends Bundle {
  val hit  = Bool()
  val data = UInt(P.clBits.W)
}

class PTE(val P: Param) extends Bundle {
  val res  = Opt (54 - P.ppnBits)
  val ppn  = UInt(P.ppnBits.W)
  val rsw  = UInt(2.W)
  val d    = Bool()
  val a    = Bool()
  val g    = Bool()
  val u    = Bool()
  val x    = Bool()
  val w    = Bool()
  val r    = Bool()
  val v    = Bool()

  def b: Bool = {
    r || w || x
  }
}


object LLCReq {
  def apply(P: Param, m: UInt): LLCReq = {
    val ret = Pin(new LLCReq(P))

    ret.mcn  := m
    ret
  }
}

object LLCRes {
  def apply(P: Param, h: Bool, d: UInt): LLCRes = {
    val ret = Pin(new LLCRes(P))

    ret.hit  := h
    ret.data := d
    ret
  }
}


class PTW(val P: Param) extends Module {

  // ---------------------------
  // io

  val mlb_req_i = IO(Flipped(    Valid(new MLBReq(P))))
  val mlb_res_o = IO(            Valid(new MLBEntry(P)))

  val ptc_req_o = IO(        Decoupled(new PTCReq(P)))
  val ptc_res_i = IO(            Input(new PTCRes(P)))

  val llc_req_o = IO(        Decoupled(new LLCReq(P)))
  val llc_res_i = IO(Flipped(Decoupled(new LLCRes(P))))

  val mem_req_o = IO(        Decoupled(new MemReq(P)))
  val mem_res_i = IO(Flipped(Decoupled(new MemRes(P))))

  val ctl_i     = IO(            Input(Vec (P.ptwLvl + 1, UInt(P.maBits.W))))


  // ---------------------------
  // logic

  val
     (ptw_fsm_idle ::
      ptw_fsm_ptc  ::
      ptw_fsm_req  ::
      ptw_fsm_res  ::
      ptw_fsm_null) = Enum(4)

  val mlb_req     = mlb_req_i.valid
  val mlb_req_pld = mlb_req_i.bits
  val mlb_res     = mlb_res_o.valid

  val ptc_res     = ptc_req_o.fire && ptc_res_i.hit
  val ptc_res_q   = RegNext(ptc_res, false.B)
  val ptc_res_pld = ptc_res_i

  val llc_res     = llc_res_i.fire
  val llc_res_pld = llc_res_i.bits

  val mem_res     = mem_res_i.fire
  val mem_res_pld = mem_res_i.bits


  //
  // state

  // number of bits translated in each level
  val lvl_rem     = Seq.tabulate(P.ptwLvl + 1) { l =>
                     (P.ptwLvl - l) * 9
                    }

  // higher levels may not be reachable due to huge page constraints
  val lvl_inv_idx = Seq.range(0, P.ptwLvl).filter { l =>
                     (P.maBits - P.ptwTop - l * 9) >= P.paBits
                    }

  // ptw may reach the root even before reaching the highest level
  val lvl_top     = Seq.tabulate(P.ptwLvl) { l =>
                      if (l == 0)
                        mlb_req_pld.mpn(P.ptwTop.W           ) === ctl_i(1)(P.maBits :- P.ptwTop)
                      else
                        mlb_req_pld.mpn(P.ptwTop + l * 9 :- 9) === ctl_i(1)(P.maBits :- 9)
                    }.U

  val ptw_req_q   = Pin(Bool())
  val ptw_dir_q   = Pin(Bool())
  val ptw_mpn_q   = Pin(UInt(P.mpnBits.W))

  val ptw_lvl_q   = Pin(UInt(P.ptwLvl.W))
  val ptw_top_q   = Pin(UInt(P.ptwLvl.W))
  val ptw_mdn_q   = Pin(UInt(P.mdnBits.W))
  val ptw_pdn_q   = Pin(UInt(P.pdnBits.W))

  // at least one level of translation is required
  val ptw_lvl_top = ptw_lvl_q(0) || Any(ptw_lvl_q & Rev(ptw_top_q))
  val ptw_lvl_bot = ptw_lvl_q(P.ptwLvl - 1)
  val ptw_lvl_blk = ptw_top_q(P.ptwLvl - 1) && ptw_lvl_bot

  // invalid huge page level range
  val ptw_lvl_inv = Any(lvl_inv_idx.map(ptw_lvl_q(_)).U)

  val ptw_sel_llc = Non(ptw_dir_q)
  val ptw_sel_mem =     ptw_dir_q


  //
  // llc

  val ptc_hit     = ptc_res_q && !ptc_res_pld.err
  val ptc_err     = ptc_res_q &&  ptc_res_pld.err

  val llc_hit     = llc_res   &&  llc_res_pld.hit || ptc_hit
  val llc_mis     = llc_res   && !llc_res_pld.hit

  val llc_pte     = OrM(Dec(ptw_mdn_q((P.clWid - 3).W)),
                        Div(ptc_res_q ?? ptc_res_pld.data ::
                                         llc_res_pld.data, 64)).asTypeOf(new PTE(P))

  val llc_pte_vld = llc_pte.v
  val llc_pte_blk = llc_pte.b
  val llc_pte_ppn = llc_pte.ppn

  // with the consistency requirement of midgard page tables, the lowest level
  // pte may not necessarily point to a normal data block/page. instead, it may
  // point to a page table page
  val llc_lvl_blk = llc_pte_blk ||  ptw_lvl_blk

  val llc_hit_bot = llc_pte_vld &&  llc_lvl_blk &&  ptw_lvl_bot
  val llc_hit_mem = llc_pte_vld && !llc_pte_blk && !ptw_lvl_bot
  val llc_hit_blk = llc_pte_vld &&  llc_pte_blk && !ptw_lvl_inv

  // error cases
  // 1. invalid pte
  // 2. non-block leaf pte which is also not for a pte
  // 3. huge page encountered at an invalid level
  val llc_hit_err = llc_pte_vld &&  llc_pte_blk &&  ptw_lvl_inv ||
                    llc_pte_vld && !llc_lvl_blk &&  ptw_lvl_bot ||
                   !llc_pte_vld

  val llc_hit_end = llc_hit_bot ||  llc_hit_blk ||  llc_hit_err

  val llc_init    = mlb_req
  val llc_step    = llc_res || ptc_res_q
  val llc_done    = llc_hit && llc_hit_end || ptc_err

  // switch to mem
  val llc_mem     = llc_hit && llc_hit_mem
  val llc_top     = llc_mis && ptw_lvl_top


  //
  // mem

  // same pte errors plus bus error
  val mem_hit     = mem_res && !mem_res_pld.err
  val mem_err     = mem_res &&  mem_res_pld.err

  val mem_pte     = OrM(Dec(ptw_pdn_q((P.clWid - 3).W)),
                        Div(mem_res_pld.data, 64)).asTypeOf(new PTE(P))

  val mem_pte_vld = mem_pte.v
  val mem_pte_blk = mem_pte.b
  val mem_pte_ppn = mem_pte.ppn

  // same as the above llc case
  val mem_lvl_blk = mem_pte_blk ||  ptw_lvl_blk

  val mem_hit_bot = mem_pte_vld &&  mem_lvl_blk &&  ptw_lvl_bot
  val mem_hit_blk = mem_pte_vld &&  mem_pte_blk && !ptw_lvl_inv
  val mem_hit_err = mem_pte_vld &&  mem_pte_blk &&  ptw_lvl_inv ||
                    mem_pte_vld && !mem_lvl_blk &&  ptw_lvl_bot ||
                   !mem_pte_vld

  val mem_hit_end = mem_hit_bot ||  mem_hit_blk ||  mem_hit_err

  val mem_init    = llc_mem || llc_top
  val mem_step    = mem_res
  val mem_done    = mem_hit && mem_hit_end || mem_err


  //
  // ptw

  ptw_req_q := RegEnable(mlb_req, false.B, mlb_req || mlb_res)
  ptw_dir_q := RegEnable(mem_init,         mlb_req || mem_init)
  ptw_mpn_q := RegEnable(mlb_req_pld.mpn,  mlb_req)

  val ptw_mpn = mlb_req     ?? mlb_req_pld.mpn :: ptw_mpn_q
  val ptw_pte = ptw_sel_llc ?? llc_pte         :: mem_pte

  // level can either increase (llc) or decrease (mem)
  val ptw_lvl_ror = llc_step && !llc_done && !ptw_lvl_top && !llc_mem
  val ptw_lvl_rol = mem_step && !mem_done && !ptw_lvl_bot ||  llc_mem

  val ptw_lvl_en  = llc_init    ||
                    ptw_lvl_ror ||
                    ptw_lvl_rol

  val ptw_lvl_rot = OrM(Seq(ptw_lvl_ror,
                            ptw_lvl_rol),
                        Seq(RoR(ptw_lvl_q, 1),
                            RoL(ptw_lvl_q, 1)))

  val ptw_lvl_nxt = llc_init ?? RoR(1.U(P.ptwLvl.W), 1) :: ptw_lvl_rot

  // always onehot during ptw
  Chk(ptw_req_q -> OHp(ptw_lvl_q, false.B))

  // calculate the ma of the pte one cycle earlier
  def gen_pma(l: Int): UInt = {
    ctl_i(l + 1)(P.maBits :- lvl_rem(l)) ##
    ptw_mpn(P.mpnBits := lvl_rem(l + 1))
  }

  val ptw_mdn_nxt = OrM(ptw_lvl_nxt,
                        Seq.tabulate(P.ptwLvl)(gen_pma))

  ptw_lvl_q := RegEnable(ptw_lvl_nxt,  ptw_lvl_en)
  ptw_mdn_q := RegEnable(ptw_mdn_nxt,  ptw_lvl_en)
  ptw_top_q := RegEnable(ArR(lvl_top), mlb_req)

  // index bits in lower levels
  def gen_ppa(l: Int): UInt = {
    ptw_mpn_q(lvl_rem(l + 1) :- 9)
  }

  // top level consumes ptwTop bits instead of 9 bits for indexing
  val ptw_pdn_top = ctl_i(0)(P.paBits := P.ptwTop + 3) ##
                    ptw_mdn_q(P.ptwTop.W)

  val ptw_pdn_nxt = ptw_pte.ppn ##
                    OrM(ptw_lvl_nxt(P.ptwLvl := 1),
                        Seq.tabulate(P.ptwLvl - 1)(gen_ppa))

  // calculate the pa of the pte one cycle earlier
  ptw_pdn_q := RegEnable(llc_top  ?? ptw_pdn_top :: ptw_pdn_nxt,
                         mem_init || mem_step)


  //
  // fsm

  val ptw_fsm_en  = Pin(Bool())
  val ptw_fsm_nxt = Pin(UInt(2.W))
  val ptw_fsm_q   = Pin(UInt(2.W))

  // the fsm serves both llc and mem
  val ptw_init    = ptw_req_q   && ptc_req_o.ready
  val ptw_done    = ptw_sel_llc || mem_done
  val ptw_req_rdy = ptw_sel_llc ?? llc_req_o.ready :: mem_req_o.ready
  val ptw_res_vld = ptw_sel_llc ?? llc_res_i.valid :: mem_res_i.valid

  ptw_fsm_en  := false.B
  ptw_fsm_nxt := ptw_fsm_q

  switch (ptw_fsm_q) {
    is (ptw_fsm_idle) {
      ptw_fsm_en  := ptw_init
      ptw_fsm_nxt := ptc_res  ?? ptw_fsm_ptc  ::
                                 ptw_fsm_req
    }
    // wait one cycle for the ram in the ptc
    is (ptw_fsm_ptc) {
      ptw_fsm_en  := true.B
      ptw_fsm_nxt := llc_done ?? ptw_fsm_idle ::
                                 ptw_fsm_req
    }
    // comply with the standard protocol
    is (ptw_fsm_req)  {
      ptw_fsm_en  := ptw_req_rdy
      ptw_fsm_nxt := ptw_fsm_res
    }
    is (ptw_fsm_res) {
      ptw_fsm_en  := ptw_res_vld
      ptw_fsm_nxt := ptw_done ?? ptw_fsm_idle ::
                                 ptw_fsm_req
    }
  }

  ptw_fsm_q := RegEnable(ptw_fsm_nxt, ptw_fsm_idle, ptw_fsm_en)

  val ptw_fsm_is_idle = ptw_fsm_q === ptw_fsm_idle
  val ptw_fsm_is_req  = ptw_fsm_q === ptw_fsm_req
  val ptw_fsm_is_res  = ptw_fsm_q === ptw_fsm_res


  //
  // output

  mlb_res_o.valid := ptw_req_q && (llc_done || mem_done)
  mlb_res_o.bits  := MLBEntry(P,
                              llc_hit && llc_hit_err ||
                              mem_hit && mem_hit_err ||
                              ptc_err ||
                              mem_err ||
                             !ptw_req_q,
                              Enc(ptw_lvl_q),
                              ptw_mpn_q,
                              ptw_pte.ppn,
                              ptw_pte.d ##
                              ptw_pte.a ##
                             (ptw_pte.w || ptw_lvl_blk) ##
                             (ptw_pte.r || ptw_lvl_blk))

  ptc_req_o.valid := ptw_fsm_is_idle && ptw_req_q || mem_res_i.valid
  ptc_req_o.bits  := PTCReq(P,
                            ptw_fsm_is_res,
                            mem_res_pld.err,
                            ptw_mdn_q(P.mdnBits := P.clWid - 3),
                            mem_res_pld.data)

  llc_req_o.valid := ptw_fsm_is_req  && ptw_sel_llc
  llc_req_o.bits  := LLCReq(P,
                            ptw_mdn_q(P.mdnBits := P.clWid - 3))

  llc_res_i.ready := ptw_fsm_is_res  && ptw_sel_llc

  mem_req_o.valid := ptw_fsm_is_req  && ptw_sel_mem
  mem_req_o.bits  := MemReq(P,
                           (P.mrqWays - 1).U,
                            true.B,
                            ptw_mdn_q(P.mdnBits := P.clWid - 3),
                            ptw_pdn_q(P.pdnBits := P.clWid - 3),
                            0.U)

  mem_res_i.ready := ptw_fsm_is_res  && ptw_sel_mem && ptc_req_o.ready
}
