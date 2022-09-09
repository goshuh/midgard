package midgard.backside

import  chisel3._
import  chisel3.util._
import  midgard._
import  midgard.util._


class LLCReq (val P: Param) extends Bundle {
  val mcn  = UInt(P.mcnBits.W)
}

class LLCResp(val P: Param) extends Bundle {
  val hit  = Bool()
  val data = UInt(P.clBits.W)
}

class MemReq (val P: Param, val w: Int = 0) extends Bundle {
  val wid  = if (w <= 0) P.mrqIdx else w
  val idx  = UInt(wid.W)
  val rnw  = Bool()
  val mcn  = UInt(P.mcnBits.W)
  val pcn  = UInt(P.pcnBits.W)
  val data = UInt(P.clBits.W)
}

class MemResp(val P: Param, val w: Int = 0) extends Bundle {
  val wid  = if (w <= 0) P.mrqIdx else w
  val idx  = UInt(wid.W)
  val err  = Bool()
  val rnw  = Bool()
  val data = UInt(P.clBits.W)
}

class PTE    (val P: Param) extends Bundle {
  val res  = UInt((54 - P.ppnBits).W)
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
    val ret = Wire(new LLCReq(P))

    ret.mcn  := m
    ret
  }
}

object LLCResp {
  def apply(P: Param, h: Bool, d: UInt): LLCResp = {
    val ret = Wire(new LLCResp(P))

    ret.hit  := h
    ret.data := d
    ret
  }
}

object MemReq {
  def apply(P: Param, i: UInt, r: Bool, m: UInt, p: UInt, d: UInt, w: Int = 0): MemReq = {
    val ret = Wire(new MemReq(P, w))

    ret.idx  := i
    ret.rnw  := r
    ret.mcn  := m
    ret.pcn  := p
    ret.data := d
    ret
  }
}

object MemResp {
  def apply(P: Param, i: UInt, e: Bool, r: Bool, d: UInt, w: Int = 0): MemResp = {
    val ret = Wire(new MemResp(P, w))

    ret.idx  := i
    ret.err  := e
    ret.rnw  := r
    ret.data := d
    ret
  }
}


class PTW(P: Param) extends Module {

  // --------------------------
  // io

  val mlb_req_i  = IO(Flipped(Decoupled(new MLBReq(P))))
  val mlb_resp_o = IO(            Valid(new MLBEntry(P)))

  val ptc_req_o  = IO(            Valid(new PTCRdReq(P)))
  val ptc_resp_i = IO(Flipped(    Valid(new PTCEntry(P))))

  val upd_req_o  = IO(            Valid(new PTCWrReq(P)))

  val llc_req_o  = IO(        Decoupled(new LLCReq(P)))
  val llc_resp_i = IO(Flipped(Decoupled(new LLCResp(P))))

  val mrq_req_o  = IO(        Decoupled(new MemReq(P)))
  val mrq_resp_i = IO(Flipped(Decoupled(new MemResp(P))))

  val ctl_i      = IO(            Input(Vec (P.ptwLvl + 1, UInt(P.maBits.W))))
  val rst_i      = IO(            Input(Bool()))


  // --------------------------
  // logic

  val
     (fsm_idle ::
      fsm_req  ::
      fsm_resp ::
      fsm_null) = Enum(4)

  val mlb_req  = mlb_req_i.fire()
  val llc_resp = llc_resp_i.fire()
  val mrq_resp = mrq_resp_i.fire()


  //
  // state

  // number of bits from the base in each level of ma translation
  val lvl_rem     = Seq.tabulate(P.ptwLvl + 1) { l =>
                      (P.ptwLvl - l) * 9
                    }

  // higher levels may not be reachable due to huge page constraints
  // i.e., the huge page at certain level may linearly map more bits than
  // physically had
  val lvl_inv_idx = Seq.range(0, P.ptwLvl).filter { l =>
                      (P.maBits - P.ptwTop - l * 9) > P.paBits
                    }

  // with the consistency requirement of midgard page tables, ptw may reach
  // the root page table even before reaching the highest level. this occurs
  // especially when the ma to be translated is within the table range
  val lvl_top     = Seq.tabulate(P.ptwLvl) { l =>
                      if (l == 0)
                        mlb_req_i.bits.mpn(P.ptwTop.W           ) === ctl_i(1)(P.maBits :- P.ptwTop)
                      else
                        mlb_req_i.bits.mpn(P.ptwTop + l * 9 :- 9) === ctl_i(1)(P.maBits :- 9)
                    }.U

  // globals
  val ptw_vld_q   = dontTouch(Wire(Bool()))
  val ptw_dir_q   = dontTouch(Wire(Bool()))
  val ptw_mpn_q   = dontTouch(Wire(UInt(P.mpnBits.W)))

  val ptw_lvl_q   = dontTouch(Wire(UInt(P.ptwLvl.W)))
  val ptw_top_q   = dontTouch(Wire(UInt(P.ptwLvl.W)))
  val ptw_mdn_q   = dontTouch(Wire(UInt(P.mdnBits.W)))
  val ptw_pdn_q   = dontTouch(Wire(UInt(P.pdnBits.W)))

  // at least one level of translation is required
  val ptw_lvl_top = ptw_lvl_q(0) || Any(ptw_lvl_q & Rev(ptw_top_q))
  val ptw_lvl_bot = ptw_lvl_q(P.ptwLvl - 1)
  val ptw_lvl_blk = ptw_top_q(P.ptwLvl - 1) && ptw_lvl_bot

  // invalid huge page level range
  val ptw_lvl_inv = Any(lvl_inv_idx.map(ptw_lvl_q(_)).U)

  val llc_vld     = ptw_vld_q && !ptw_dir_q
  val mrq_vld     = ptw_vld_q &&  ptw_dir_q


  //
  // llc

  val ptc_hit     = ptc_resp_i.valid
  val llc_hit     = llc_resp &&  llc_resp_i.bits.hit || ptc_hit
  val llc_mis     = llc_resp && !llc_resp_i.bits.hit

  val llc_pte     = OrM(Dec(ptw_mdn_q((P.clWid - 3).W)),
                        Div(ptc_hit ?? ptc_resp_i.bits.data ::
                                       llc_resp_i.bits.data, 64)).asTypeOf(new PTE(P))

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
  val llc_step    = ptc_hit || llc_resp
  val llc_done    = llc_hit && llc_hit_end

  // switch to mem
  val llc_mem     = llc_hit && llc_hit_mem
  val llc_top     = llc_mis && ptw_lvl_top


  //
  // mem

  // same pte errors plus bus error
  val mrq_hit     = mrq_resp && !mrq_resp_i.bits.err
  val mrq_err     = mrq_resp &&  mrq_resp_i.bits.err

  val mrq_pte     = OrM(Dec(ptw_pdn_q((P.clWid - 3).W)),
                        Div(mrq_resp_i.bits.data, 64)).asTypeOf(new PTE(P))

  val mrq_pte_vld = mrq_pte.v
  val mrq_pte_blk = mrq_pte.b
  val mrq_pte_ppn = mrq_pte.ppn

  val mrq_lvl_blk = mrq_pte_blk ||  ptw_lvl_blk

  val mrq_hit_bot = mrq_pte_vld &&  mrq_lvl_blk &&  ptw_lvl_bot
  val mrq_hit_blk = mrq_pte_vld &&  mrq_pte_blk && !ptw_lvl_inv
  val mrq_hit_err = mrq_pte_vld &&  mrq_pte_blk &&  ptw_lvl_inv ||
                    mrq_pte_vld && !mrq_lvl_blk &&  ptw_lvl_bot ||
                   !mrq_pte_vld

  val mrq_hit_end = mrq_hit_bot ||  mrq_hit_blk ||  mrq_hit_err

  val mrq_init    = llc_mem || llc_top
  val mrq_step    = mrq_resp
  val mrq_done    = mrq_hit && mrq_hit_end || mrq_err


  //
  // ptw

  ptw_vld_q := RegEnable(mlb_req, false.B,   mlb_req || mlb_resp_o.valid)
  ptw_dir_q := RegEnable(mrq_init,           mlb_req || mrq_init)
  ptw_mpn_q := RegEnable(mlb_req_i.bits.mpn, mlb_req)

  val ptw_mpn = ptw_vld_q ?? ptw_mpn_q :: mlb_req_i.bits.mpn
  val ptw_pte = llc_vld   ?? llc_pte   :: mrq_pte

  // level can either increase (ror) or decrease (rol)
  val ptw_lvl_ror = llc_step && !llc_done && !ptw_lvl_top && !llc_mem
  val ptw_lvl_rol = mrq_step && !mrq_done && !ptw_lvl_bot ||  llc_mem

  val ptw_lvl_en  = llc_init || ptw_lvl_ror || ptw_lvl_rol

  val ptw_lvl_rot = OrM(Seq(ptw_lvl_ror,
                            ptw_lvl_rol),
                        Seq(RoR(ptw_lvl_q, 1),
                            RoL(ptw_lvl_q, 1)))

  val ptw_lvl_nxt = llc_init ?? RoR(1.U(P.ptwLvl.W), 1) ::
                                ptw_lvl_rot

  ptw_lvl_q := RegEnable(ptw_lvl_nxt,
                         ptw_lvl_en)

  ptw_top_q := RegEnable(ArR(lvl_top),
                         mlb_req)

  // always onehot during ptw
  assert(ptw_vld_q -> OHp(ptw_lvl_q, false.B))

  // calculate the ma of the pte one cycle earlier
  def gen_pma(l: Int): UInt = {
    ctl_i(l + 1)(P.maBits :- lvl_rem(l)) ## ptw_mpn(P.mpnBits := lvl_rem(l + 1))
  }

  val ptw_mdn_nxt = OrM(ptw_lvl_nxt,
                        Seq.tabulate(P.ptwLvl)(gen_pma))

  ptw_mdn_q := RegEnable(ptw_mdn_nxt,
                         ptw_lvl_en)

  // index bits in lower levels
  def gen_ppa(l: Int): UInt = {
    ptw_mpn_q(lvl_rem(l + 1) :- 9)
  }

  // top level consumes ptwTop bits instead of 9 bits for indexing
  val ptw_pdn_top = ctl_i(0)(P.paBits := P.ptwTop + 3) ## ptw_mdn_q(P.ptwTop.W)

  val ptw_pdn_nxt = ptw_pte.ppn ## OrM(ptw_lvl_nxt(P.ptwLvl := 1),
                                       Seq.tabulate(P.ptwLvl - 1)(gen_ppa))

  // calculate the pa of the pte one cycle earlier
  ptw_pdn_q := RegEnable(llc_top  ?? ptw_pdn_top :: ptw_pdn_nxt,
                         mrq_init || mrq_step)


  //
  // fsm

  val req_fsm_en  = dontTouch(Wire(Bool()))
  val req_fsm_nxt = dontTouch(Wire(UInt(2.W)))
  val req_fsm_q   = dontTouch(Wire(UInt(2.W)))

  req_fsm_en  := false.B
  req_fsm_nxt := req_fsm_q

  // the fsm serves both llc and mem
  val req_fsm_byp      = llc_vld && ptc_hit
  val req_fsm_done     = llc_vld || mrq_done
  val req_fsm_req_rdy  = llc_vld ?? llc_req_o.ready  :: mrq_req_o.ready
  val req_fsm_resp_vld = llc_vld ?? llc_resp_i.valid :: mrq_resp_i.valid

  switch (req_fsm_q) {
    is (fsm_idle) {
      req_fsm_en  := ptw_vld_q
      req_fsm_nxt := req_fsm_byp  ?? fsm_idle :: fsm_req
    }
    is (fsm_req)  {
      req_fsm_en  := req_fsm_req_rdy
      req_fsm_nxt := fsm_resp
    }
    is (fsm_resp) {
      req_fsm_en  := req_fsm_resp_vld
      req_fsm_nxt := req_fsm_done ?? fsm_idle :: fsm_req
    }
  }

  req_fsm_q := RegEnable(req_fsm_nxt, fsm_idle, req_fsm_en)

  val req_fsm_is_idle = req_fsm_q === fsm_idle
  val req_fsm_is_req  = req_fsm_q === fsm_req
  val req_fsm_is_resp = req_fsm_q === fsm_resp


  //
  // output

  mlb_req_i.ready  := ptw_vld_q === false.B

  mlb_resp_o.valid := llc_done || mrq_done
  mlb_resp_o.bits  := MLBEntry(P,
                               llc_hit && llc_hit_err ||
                               mrq_hit && mrq_hit_err ||
                               mrq_err,
                               Enc(ptw_lvl_q),
                               ptw_mpn(P.mpnBits := P.mlbIdx),
                               ptw_pte.ppn,
                               ptw_pte.d ## ptw_pte.a ## (ptw_pte.w || ptw_lvl_blk))

  ptc_req_o.valid  := llc_vld && req_fsm_is_idle
  ptc_req_o.bits   := PTCRdReq(P,
                               ptw_mdn_q(P.mdnBits := P.clWid - 3))

  upd_req_o.valid  := mrq_hit
  upd_req_o.bits   := PTCWrReq(P,
                               ptw_lvl_q,
                               ptw_mdn_q(P.mdnBits := P.clWid - 3),
                               mrq_resp_i.bits.data)

  llc_req_o.valid  := llc_vld && req_fsm_is_req
  llc_req_o.bits   := LLCReq(P,
                             ptw_mdn_q(P.mdnBits := P.clWid - 3))
  llc_resp_i.ready := true.B

  mrq_req_o.valid  := mrq_vld && req_fsm_is_req
  mrq_req_o.bits   := MemReq(P,
                             0.U,
                             true.B,
                             ptw_mdn_q(P.mdnBits := P.clWid - 3),
                             ptw_pdn_q(P.pdnBits := P.clWid - 3),
                             0.U,
                             P.llcIdx)
  mrq_resp_i.ready := true.B
}
