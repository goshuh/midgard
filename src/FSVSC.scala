package midgard.frontside

import  chisel3._
import  chisel3.util._
import  midgard._
import  midgard.util._


class MemReq(val P: Param) extends Bundle {
  val idx   = UInt(P.ttwIdx.W)
  val mcn   = UInt(P.mcnBits.W)
}

class MemRes(val P: Param) extends Bundle {
  val idx   = UInt(P.ttwIdx.W)
  val data  = UInt(P.clBits.W)
}

class VSCVMA(val P: Param) extends Bundle {
  // 16 bytes in size
  val vld   = Bool()
  val pad   = Opt (127 - P.attrBits - P.sdidBits - 2 * P.vpnBits)
  val attr  = UInt(P.attrBits.W)
  val sdid  = UInt(P.sdidBits.W)
  val offs  = UInt(P.vpnBits.W)
  val bound = UInt(P.vpnBits.W)
}

class VSCCfg extends Bundle {
  val idx   = UInt( 6.W)
  val vsc   = UInt( 6.W)
  val top   = UInt( 6.W)
  val tsl   = UInt( 6.W)

  val mmask = UInt(32.W) // max 32 offs bits for the minimum vsc (2 mb minimum)
  val imask = UInt(32.W) // max 32 idx bits
  val vmask = UInt( 5.W) // max 32 vscs
  val tmask = UInt(20.W) // max 64 mb for the table
}

class VSCEntry(val P: Param) extends Bundle {
  val bot   = UInt(P.vpnBits.W)
  val idx   = UInt(32.W)
  val top   = UInt(32.W)
  val fsm   = UInt( 2.W)
  val way   = UInt(P.vscWays.W)
  val vpn   = UInt(P.vpnBits.W)
}


object MemReq {
  def apply(P: Param, i: UInt, m: UInt): MemReq = {
    val ret = Pin(new MemReq(P))

    ret.idx := i
    ret.mcn := m
    ret
  }
}


class VSC(val P: Param) extends Module {

  // ---------------------------
  // io

  val vlb_req_i = IO(Vec(P.ttwNum, Flipped(    Valid(new VLBReq(P)))))
  val vlb_res_o = IO(Vec(P.ttwNum,             Valid(new VMA   (P))))
  val vlb_ext_o = IO(Vec(P.ttwNum,            Output(new TTWExt(P))))

  val mem_req_o = IO(                      Decoupled(new MemReq(P)))
  val mem_res_i = IO(              Flipped(Decoupled(new MemRes(P))))

  val inv_req_i = IO(              Flipped(    Valid(new InvReq(P))))

  val satp_i    = IO(                          Input(UInt(64.W)))
  val uatp_i    = IO(                          Input(UInt(64.W)))
  val uatc_i    = IO(                          Input(new VSCCfg()))

  val idle_o    = IO(                         Output(Vec (P.ttwNum, Bool())))


  // ---------------------------
  // logic

  val
     (vsc_fsm_idle ::
      vsc_fsm_req  ::
      vsc_fsm_res  ::
      vsc_fsm_null) = Enum(4)

  val inv_req        = inv_req_i.valid
  val inv_req_pld    = inv_req_i.bits.mcn ## inv_req_i.bits.idx

  val mem_req        = mem_req_o.fire
  val mem_res        = mem_res_i.fire
  val mem_res_pld    = mem_res_i.bits
  val mem_res_vma    = Pin(new VSCVMA(P))

  val asid           = satp_i(44 :+ P.asidBits)
  val sdid           = uatp_i(48 :+ P.sdidBits)


  //
  // stage 0

  val s0_req_rdy     = Pin(Vec(P.ttwNum, Bool()))
  val s0_req_raw     = s0_req_rdy.U & vlb_req_i.map(_.valid).U
  val s0_req_any     = Any(s0_req_raw)

  val s0_req         = inv_req || s0_req_any
  val s0_req_sel     = RRA(s0_req_raw, s0_req)
  val s0_req_pld     = OrM(s0_req_sel,
                           vlb_req_i.map(_.bits))

  // calculate the formula
  val s0_req_va      = s0_req_pld.vpn ## 0.U(12.W)
  val s0_req_idx_s   = BSR(s0_req_va, uatc_i.idx)(32.W) & uatc_i.imask
  val s0_req_vsc_s   = BSR(s0_req_va, uatc_i.vsc)( 5.W) & uatc_i.vmask
  val s0_req_top_s   = BSR(s0_req_va, uatc_i.top)(32.W)

  val s0_req_vmask   = OrR(Dec(s0_req_vsc_s))
  val s0_req_vsh_1   = s0_req_vmask(32 := 1)
  val s0_req_vsh_2   = s0_req_vmask(32 := 2)
  val s0_req_mmask   = Ext(BFL(uatc_i.mmask, s0_req_vsc_s), P.vaBits)

  val s0_req_min     = Non(s0_req_vsc_s)
  val s0_req_idx     = inv_req ?? inv_req_pld :: (s0_req_idx_s & ~s0_req_vsh_1 | s0_req_vsh_2)
  val s0_req_bot     = inv_req ?? 0.U         :: (s0_req_va    & ~s0_req_mmask)(P.vaBits := 12)
  val s0_req_top     = inv_req ?? 0.U         ::  s0_req_top_s


  //
  // stage 1

  val s1_req_q       = RegNext  (s0_req,       false.B)
  val s1_req_pld_q   = RegEnable(s0_req_pld,   s0_req)
  val s1_req_sel_q   = RegEnable(s0_req_sel,   s0_req)

  val s1_req_inv     = Non(s1_req_sel_q)
  val s1_req         = s1_req_q && (s1_req_inv || Non(s1_req_sel_q & vlb_req_i.map(_.bits.kill(0)).U))
  val s1_req_vlb     = s1_req   && !s1_req_inv

  val s1_req_min_q   = RegEnable(s0_req_min,   s0_req)
  val s1_req_idx_q   = RegEnable(s0_req_idx,   s0_req)
  val s1_req_bot_q   = RegEnable(s0_req_bot,   s0_req)
  val s1_req_top_q   = RegEnable(s0_req_top,   s0_req)
  val s1_req_idx_ram = s1_req_idx_q(P.vscBits.W)


  //
  // stage 2

  val s2_req_q       = RegNext  (s1_req,       false.B)
  val s2_req_pld_q   = RegEnable(s1_req_pld_q, s1_req_q)
  val s2_req_sel_q   = RegEnable(s1_req_sel_q, s1_req_q)

  val s2_req_inv     = Non(s2_req_sel_q)
  val s2_req         = s2_req_q && (s2_req_inv || Non(s2_req_sel_q & vlb_req_i.map(_.bits.kill(0)).U))
  val s2_req_vlb     = s2_req   && !s2_req_inv

  val s2_req_min_q   = RegEnable(s1_req_min_q, s1_req_q)
  val s2_req_bot_q   = RegEnable(s1_req_bot_q, s1_req_q)
  val s2_req_top_q   = RegEnable(s1_req_top_q, s1_req_q)
  val s2_req_idx_q   = RegEnable(s1_req_idx_q, s1_req_q)

  val s2_req_idx_ram = s2_req_idx_q(P.vscBits.W)
  val s2_req_idx     = ShL(s2_req_idx_q, 1) | !s2_req_min_q

  val s2_rdata       = Pin(Vec(P.vscWays, new VMA(P)))
  val s2_hit_way     = Pin(Vec(P.vscWays, Bool()))
  val s2_inv_way     = Pin(Vec(P.vscWays, Bool()))
  val s2_hit_mux     = OrM(s2_hit_way, s2_rdata)

  Chk(s2_req_q -> OHp(s2_hit_way.U, true.B))

  val s2_hit_any     = Any(s2_hit_way)
  val s2_inv_any     = Any(s2_inv_way)

  val s2_rpl_way     = s2_inv_any ?? PrL(s2_inv_way.U) ::
                                     PRA(P.vscWays, s2_req)

  // actions
  val s2_hit_ttw     = Pin(Vec(P.ttwNum, Bool()))

  val s2_hit_inv     = s2_req &&  s2_hit_any &&  s2_req_inv
  val s2_hit_vlb     = s2_req &&  s2_hit_any && !s2_req_inv
  val s2_mis_vlb     = s2_req && !s2_hit_any && !s2_req_inv && Non(s2_hit_ttw)

  // forward decl
  val s3_mem_res     = Pin(Bool())
  val s3_mem_res_way = Pin(UInt(P.vscWays.W))
  val s3_mem_res_idx = Pin(UInt(P.vscBits.W))
  val s3_mem_res_vma = Pin(new VMA(P))

  // reset
  val rst_q          = Pin(UInt((P.vscBits + 1).W))
  val rst_pend       = Non(rst_q(P.vscBits))
  val rst_done       = Any(rst_q(P.vscBits))

  val rst_idx        = rst_q(P.vscBits.W)
  val rst_vma        = 0.U.asTypeOf(new VMA(P))

  rst_q := RegEnable(rst_q + 1.U,
                     0.U,
                     rst_pend)

  for (i <- 0 until P.vscWays) {
    val wid = 8 * (new VMA(P).getWidth + 7) / 8

    val ram = Module(new SPRAM(P.vscBits, wid, wid / 8))

    val s2_inv_we  = s2_hit_inv && s2_hit_way    (i)
    val s3_res_we  = s3_mem_res && s3_mem_res_way(i)

    val ram_ren    = s1_req_vlb
    val ram_raddr  = s1_req_idx_ram

    val ram_wen    = rst_pend || s2_inv_we || s3_res_we
    val ram_waddr  = rst_pend ?? rst_idx   :: s2_inv_we ?? s2_req_idx_ram :: s3_mem_res_idx
    val ram_wdata  = rst_pend ?? rst_vma   :: s2_inv_we ?? rst_vma        :: s3_mem_res_vma

    ram.clk       := clock
    ram.en        := ram_wen || ram_ren
    ram.wnr       := ram_wen
    ram.addr      := ram_wen ?? ram_waddr :: ram_raddr
    ram.wdata     := ram_wdata.asUInt
    ram.wstrb     := Rep(true.B, wid / 8)

    s2_rdata  (i) := ram.rdata(wid.W).asTypeOf(new VMA(P))

    s2_hit_way(i) := s2_req_q &&  s2_rdata(i).vld && s2_rdata(i).hit(s2_req_pld_q.vpn, asid)
    s2_inv_way(i) := s2_req_q && !s2_rdata(i).vld
  }


  //
  // intf

  val mem_req_ext = Pin(Bool())
  val mem_req_raw = Pin(Vec(P.ttwNum, Bool()))
  val mem_req_sel = PrL(mem_req_raw.U)
  val mem_res_sel = Dec(mem_res_pld.idx)

  val vsc_q = Pin(Vec(P.ttwNum, new VSCEntry(P)))

  for (i <- 0 until P.ttwNum) {
    // required to be accurate
    val sx_req_idx_q = RegEnable(vlb_req_i(i).bits.idx, s0_req_sel(i))

    val hit = s2_req_sel_q(i) && s2_hit_vlb
    val set = s2_req_sel_q(i) && s2_mis_vlb

    val req = mem_req_sel (i) && mem_req
    val res = mem_res_sel (i) && mem_res

    vsc_q(i).idx := RegEnable(s2_req_idx,       set)
    vsc_q(i).top := RegEnable(s2_req_top_q,     set)
    vsc_q(i).bot := RegEnable(s2_req_bot_q,     set)
    vsc_q(i).way := RegEnable(s2_rpl_way,       set)
    vsc_q(i).vpn := RegEnable(s2_req_pld_q.vpn, set)

    // fsm
    val fsm_en  = Pin(Bool())
    val fsm_nxt = Pin(UInt(2.W))

    val req_ext = mem_req_sel(i) && mem_req_ext
    val kill_q  = Pin(Bool())
    val kill    = Pin(Bool())

    fsm_en  := false.B
    fsm_nxt := vsc_q(i).fsm

    switch (vsc_q(i).fsm) {
      is (vsc_fsm_idle) {
        fsm_en  := set
        fsm_nxt := vsc_fsm_req
      }
      is (vsc_fsm_req) {
        fsm_en  := mem_req_sel(i) || kill
        fsm_nxt := req_ext ?? vsc_fsm_idle ::
                   req     ?? vsc_fsm_res  ::
                   kill    ?? vsc_fsm_idle ::
                              vsc_fsm_req
      }
      is (vsc_fsm_res) {
        fsm_en  := res
        fsm_nxt := vsc_fsm_idle
      }
    }

    val fsm_is_busy = vsc_q(i).fsm =/= vsc_fsm_idle
    val fsm_is_req  = vsc_q(i).fsm === vsc_fsm_req
    val fsm_is_res  = vsc_q(i).fsm === vsc_fsm_res

    val kill_raw = vlb_req_i(i).bits.kill(0) && fsm_is_busy
    val kill_clr = req_ext || res

    kill_q := RegEnable(kill_raw && !kill_clr,
                        false.B,
                        kill_raw ||  kill_clr)
    kill   := kill_raw ||
              kill_q

    vsc_q(i).fsm   := RegEnable(fsm_nxt, vsc_fsm_idle, fsm_en)

    mem_req_raw(i) := fsm_is_req

    val rdy = Non(fsm_is_busy) &&
                 !(s1_req_q && s1_req_sel_q(i)) &&
                 !(s2_req_q && s2_req_sel_q(i))

    idle_o     (i) := rdy || kill
    s0_req_rdy (i) := rdy && rst_done && !inv_req

    s2_hit_ttw (i) := fsm_is_busy &&
                         (s2_req_bot_q === vsc_q(i).bot)

    // output
    val req_ext_vld = req_ext && !kill
    val res_vld     = res     && !kill

    vlb_res_o(i).valid := hit || req_ext_vld || res_vld
    vlb_res_o(i).bits  := OrM(Seq(hit,
                                  req_ext_vld,
                                  res_vld),
                              Seq(s2_hit_mux,
                                  VMA(P),
                                  VMA(P,
                                      mem_res_vma,
                                      asid,
                                      vsc_q(i).bot,
                                      vsc_q(i).idx(P.pmtBits.W))))

    // allowed to be valid but with error
    val err = req_ext_vld ||
              res_vld && !mem_res_vma.vld ||
            ((res_vld ??  vsc_q(i).vpn      :: s2_req_pld_q.vpn) >
             (res_vld ??  mem_res_vma.bound :: s2_hit_mux.bound))

    vlb_ext_o(i) := TTWExt(P,
                           sx_req_idx_q,
                           vsc_q(i).vpn,
                           err)
  }

  // big partial muxes
  val mem_req_mux = OrM(mem_req_sel, vsc_q)
  val mem_res_mux = OrM(mem_res_sel, vsc_q)

  mem_res_vma := OrM(Dec(mem_res_mux.idx(2.W)),
                     Div(mem_res_pld.data, 128)).asTypeOf(new VSCVMA(P))

  s3_mem_res     := mem_res && mem_res_vma.vld
  s3_mem_res_idx := mem_res_mux.idx(P.vscBits.W)
  s3_mem_res_way := mem_res_mux.way
  s3_mem_res_vma := VMA(P,
                        mem_res_vma,
                        asid,
                        mem_res_mux.bot,
                        mem_res_mux.idx(P.pmtBits.W))


  //
  // output

  val mem_req_arr  = uatp_i(48.W) ## 0.U(6.W)
  // idx must be larger than 12
  val mem_req_idx  = mem_req_mux.idx(32 := 2) | BSL(Ext(mem_req_mux.top, 40), uatc_i.tsl)

  // maximum 64 mb for the table, should be fine
  mem_req_ext     := Any(mem_req_idx(40 := 20)) ||
                     Any(mem_req_idx(20.W) & uatc_i.tmask)

  mem_req_o.valid := Any(mem_req_raw) && !mem_req_ext
  mem_req_o.bits  := MemReq(P,
                            Enc(mem_req_sel),
                            mem_req_arr | mem_req_idx)

  mem_res_i.ready := Non(inv_req)
}