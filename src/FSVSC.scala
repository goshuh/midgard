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

class VSCTab(val P: Param) extends Bundle {
  // 2 bytes in size
  val csid  = UInt(P.csidBits.W)
  val attr  = UInt(4.W)

  def v = attr(0)
}

class VSCVMA(val P: Param) extends Bundle {
  // 64 bytes in size
  val tab   = Vec (20, new VSCTab(P))
  val ptr   = UInt(64.W)
  val pad   = Opt (64 - P.vpnBits)
  val offs  = UInt(P.vpnBits.W)
  val attr  = UInt(8.W)
  val res   = Opt (56 - P.vpnBits)
  val bound = UInt(P.vpnBits.W)

  def v = attr(0)
  def u = attr(4)
  def g = attr(5)
  def p = attr(6)
  def e = attr(7)
}

class VSCCfg extends Bundle {
  val idx   = UInt( 6.W)
  val vsc   = UInt( 6.W)
  val top   = UInt( 6.W)
  val siz   = UInt( 6.W)
  val tvi   = UInt( 6.W)

  val mmask = UInt(32.W) // max 32 offs bits for the minimum vsc (2 mb minimum)
  val imask = UInt(32.W) // max 32 idx bits
  val vmask = UInt( 5.W) // max 32 vscs
  val tmask = UInt(20.W) // max 64 mb for the table
}

class VSCEntry(val P: Param) extends Bundle {
  val idx   = UInt(22.W)
  val fsm   = UInt( 2.W)
  val vpn   = UInt(P.vpnBits.W)
  val way   = UInt(P.vscWays.W)
  val bot   = UInt(P.vpnBits.W)
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

  val vlb_req_i   = IO(Vec(P.ttwNum, Flipped(    Valid(new VLBReq(P)))))
  val vlb_res_o   = IO(Vec(P.ttwNum,             Valid(new VMA   (P))))
  val vlb_ext_o   = IO(Vec(P.ttwNum,            Output(new TTWExt(P))))

  val int_req_o   = IO(                         Output(UInt(4.W)))

  val mem_req_o   = IO(                      Decoupled(new MemReq(P)))
  val mem_res_i   = IO(              Flipped(Decoupled(new MemRes(P))))

  val uat_req_i   = IO(              Flipped(Decoupled(new UATReq(P))))
  val uat_req_o   = IO(                         Output(new UATReq(P)))
  val uat_res_o   = IO(                         Output(Bool()))

  val satp_i      = IO(                          Input(UInt(64.W)))
  val uatp_i      = IO(                          Input(UInt(64.W)))
  val uatc_i      = IO(                          Input(new VSCCfg()))
  val asid_i      = IO(                          Input(UInt(P.asidBits.W)))
  val csid_i      = IO(                          Input(UInt(P.csidBits.W)))

  val idle_o      = IO(                         Output(Vec (P.ttwNum, Bool())))

  val kill_i      = IO(                          Input(UInt(3.W)))
  val kill_asid_i = IO(                          Input(UInt(P.asidBits.W)))
  val kill_csid_i = IO(                          Input(UInt(P.csidBits.W)))


  // ---------------------------
  // logic

  val
     (vsc_fsm_idle ::
      vsc_fsm_req  ::
      vsc_fsm_res  ::
      vsc_fsm_null) = Enum(4)

  val uat_req        = uat_req_i.fire && uat_req_i.bits.cmd(1)
  val uat_req_pld    = uat_req_i.bits

  val mem_req        = mem_req_o.fire
  val mem_res        = mem_res_i.fire
  val mem_res_pld    = mem_res_i.bits


  //
  // stage 0

  val s0_req_rdy     = Pin(Vec(P.ttwNum, Bool()))
  val s0_req_raw     = s0_req_rdy.U & vlb_req_i.map(_.valid).U
  val s0_req_any     = Any(s0_req_raw)

  val s0_req         = uat_req || s0_req_any
  val s0_req_sel     = RRA(s0_req_raw, s0_req)
  val s0_req_pld     = OrM(s0_req_sel,
                           vlb_req_i.map(_.bits))

  // calculate the formula
  val s0_req_va      = s0_req_pld.vpn ## 0.U(12.W)
  val s0_req_idx_s   = BSR(s0_req_va, uatc_i.idx)(32.W) & uatc_i.imask
  val s0_req_vsc_s   = BSR(s0_req_va, uatc_i.vsc)( 5.W) & uatc_i.vmask
  val s0_req_top_s   = BSR(s0_req_va, uatc_i.tvi)(32.W)

  val s0_req_vmask   = OrR(Dec(s0_req_vsc_s))
  val s0_req_vsh_1   = s0_req_vmask(32 := 1)
  val s0_req_vsh_2   = s0_req_vmask(32 := 2)
  val s0_req_mmask   = Ext(BFL(uatc_i.mmask, s0_req_vsc_s), P.vaBits)(P.vaBits := 12)

  val s0_req_min     = Non(s0_req_vsc_s)
  val s0_req_idx     = s0_req_idx_s   & ~s0_req_vsh_1 | s0_req_vsh_2
  val s0_req_bot     = s0_req_pld.vpn & ~s0_req_mmask
  val s0_req_top     = s0_req_top_s
  val s0_req_mcn     = uat_req_pld.mcn


  //
  // stage 1

  val s1_req_q       = RegNext  (s0_req,       false.B)
  val s1_req_pld_q   = RegEnable(s0_req_pld,   s0_req)
  val s1_req_sel_q   = RegEnable(s0_req_sel,   s0_req)

  val s1_req_inv     = Non(s1_req_sel_q)
  val s1_req         =     s1_req_q && (s1_req_inv || Non(s1_req_sel_q & vlb_req_i.map(_.bits.kill(0)).U))
  val s1_req_rdy     = Non(s1_req_q &&  s1_req_inv)

  val s1_req_min_q   = RegEnable(s0_req_min,   s0_req)
  val s1_req_idx_q   = RegEnable(s0_req_idx,   s0_req)
  val s1_req_bot_q   = RegEnable(s0_req_bot,   s0_req)
  val s1_req_top_q   = RegEnable(s0_req_top,   s0_req)
  val s1_req_mcn_q   = RegEnable(s0_req_mcn,   s0_req)

  val s1_req_idx_ram = s1_req_inv ?? s1_req_mcn_q(1 :+ P.vscBits) ::
                                     s1_req_idx_q(0 :+ P.vscBits)


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
  val s2_req_mcn_q   = RegEnable(s1_req_mcn_q, s1_req_q)

  val s2_req_idx_ram = s2_req_inv ?? s2_req_mcn_q(1 :+ P.vscBits) ::
                                     s2_req_idx_q(0 :+ P.vscBits)

  val s2_req_idx     = s2_req_top_q |
                      (s2_req_idx_q ## !s2_req_min_q)

  // maximum 64 mb for the table, should be fine
  val s2_req_idx_ext = Any(s2_req_idx(32 := 20)) ||
                       Any(s2_req_idx(20.W) & uatc_i.tmask)

  val s2_rdata       = Pin(Vec(P.vscWays, new VMA(P)))
  val s2_hit_way     = Pin(Vec(P.vscWays, Bool()))
  val s2_clr_way     = Pin(Vec(P.vscWays, Bool()))
  val s2_old_way     = Pin(Vec(P.vscWays, Bool()))
  val s2_inv_way     = Pin(Vec(P.vscWays, Bool()))
  val s2_hit_mux     = OrM(s2_hit_way, s2_rdata)

  Chk(s2_req_q -> OHp(s2_hit_way.U, true.B))

  val s2_hit_any     = Any(s2_hit_way)
  val s2_clr_any     = Any(s2_clr_way)
  val s2_old_any     = Any(s2_old_way)
  val s2_inv_any     = Any(s2_inv_way)

  val s2_rpl_way     = s2_inv_any ?? PrL(s2_inv_way.U) ::
                       s2_old_any ?? PrL(s2_old_way.U) ::
                                     PRA(P.vscWays, s2_req)

  // actions
  val s2_hit_ttw     = Pin(Vec(P.ttwNum, Bool()))

  val s2_hit_inv     = s2_req &&  s2_req_inv &&  s2_clr_any
  val s2_hit_vlb     = s2_req && !s2_req_inv &&  s2_hit_any
  val s2_mis_vlb     = s2_req && !s2_req_inv && !s2_hit_any && Non(s2_hit_ttw)

  val s2_mis_vlb_int = s2_mis_vlb && !s2_req_idx_ext
  val s2_mis_vlb_ext = s2_mis_vlb &&  s2_req_idx_ext

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

  // simplification: by-asid/sdid flush just kills everything
  val rst_kill       = Any(kill_i(2, 1))

  rst_q := RegEnable(NeQ(rst_kill, rst_q + 1.U),
                     0.U,
                     rst_pend || rst_kill)

  for (i <- 0 until P.vscWays) {
    val ram = Module(new SPRAM(P.vscBits, new VMA(P).getWidth, 1))

    val s2_inv_we  = s2_hit_inv && s2_clr_way    (i)
    val s3_res_we  = s3_mem_res && s3_mem_res_way(i)

    val ram_ren    = s1_req_q
    val ram_raddr  = s1_req_idx_ram

    val ram_wen    = rst_pend || s2_inv_we || s3_res_we
    val ram_waddr  = rst_pend ?? rst_idx   :: s2_inv_we ?? s2_req_idx_ram :: s3_mem_res_idx
    val ram_wdata  = rst_pend ?? rst_vma   :: s2_inv_we ?? rst_vma        :: s3_mem_res_vma

    ram.clk       := clock
    ram.en        := ram_wen || ram_ren
    ram.wnr       := ram_wen
    ram.addr      := ram_wen ?? ram_waddr :: ram_raddr
    ram.wdata     := ram_wdata.asUInt
    ram.wstrb     := 1.U(1.W)

    s2_rdata  (i) := ram.rdata.asTypeOf(new VMA(P))

    s2_hit_way(i) := s2_req_q &&  s2_rdata(i).vld && s2_rdata(i).hit(s2_req_bot_q, asid_i, csid_i)
    s2_old_way(i) := s2_req_q &&  s2_rdata(i).vld && s2_rdata(i).old(              asid_i, csid_i)
    s2_inv_way(i) := s2_req_q && !s2_rdata(i).vld

    // remove the redundant vsc
    s2_clr_way(i) := s2_req_q &&
                         s2_rdata(i).vld &&
                        (s2_rdata(i).pmt(P.pmtBits := 1) === s2_req_mcn_q((P.vscBits + 1) :+ (P.pmtBits - 1))) &&
                        (s2_rdata(i).pmt(0)              === s2_req_mcn_q(0))
  }


  //
  // intf

  val mem_req_raw = Pin(Vec(P.ttwNum, Bool()))
  val mem_req_sel = PrL(mem_req_raw.U)
  val mem_res_sel = Dec(mem_res_pld.idx)

  val vsc_q = Pin(Vec(P.ttwNum, new VSCEntry(P)))

  for (i <- 0 until P.ttwNum) {
    // required to be accurate
    val sx_req_idx_q = RegEnable(vlb_req_i(i).bits.idx, s0_req_sel(i))

    val hit = s2_req_sel_q(i) && s2_hit_vlb
    val set = s2_req_sel_q(i) && s2_mis_vlb_int
    val ext = s2_req_sel_q(i) && s2_mis_vlb_ext

    val req = mem_req_sel (i) && mem_req
    val res = mem_res_sel (i) && mem_res

    vsc_q(i).idx := RegEnable(s2_req_idx(20.W), set)
    vsc_q(i).vpn := RegEnable(s2_req_pld_q.vpn, set)
    vsc_q(i).way := RegEnable(s2_rpl_way,       set)
    vsc_q(i).bot := RegEnable(s2_req_bot_q,     set)

    // fsm
    val fsm_en  = Pin(Bool())
    val fsm_nxt = Pin(UInt(2.W))

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
        fsm_en  := req || kill
        fsm_nxt := req ?? vsc_fsm_res  ::
                          vsc_fsm_idle
      }
      is (vsc_fsm_res) {
        fsm_en  := res
        fsm_nxt := vsc_fsm_idle
      }
    }

    val fsm_is_busy = vsc_q(i).fsm =/= vsc_fsm_idle
    val fsm_is_req  = vsc_q(i).fsm === vsc_fsm_req
    val fsm_is_res  = vsc_q(i).fsm === vsc_fsm_res

    kill   := Non(kill_q) &&
                 (vlb_req_i(i).bits.kill(0) ||
                  rst_kill)  &&
                 (fsm_is_req && req ||
                  fsm_is_res)

    kill_q := RegEnable(kill && !res,
                        false.B,
                        kill ||  res)

    vsc_q(i).fsm   := RegEnable(fsm_nxt, vsc_fsm_idle, fsm_en)

    mem_req_raw(i) := fsm_is_req

    val rdy = Non(fsm_is_busy) &&
                !(s1_req_q && s1_req_sel_q(i)) &&
                !(s2_req_q && s2_req_sel_q(i))

    idle_o     (i) := rdy || kill_q
    s0_req_rdy (i) := rdy && rst_done && !rst_kill && s1_req_rdy && !uat_req

    s2_hit_ttw (i) := fsm_is_busy &&
                         (s2_req_bot_q === vsc_q(i).bot)

    // output
    val res_vld = res && !kill_q

    vlb_res_o(i).valid    := hit || ext || res_vld
    vlb_res_o(i).bits     := hit ?? s2_hit_mux ::
                             ext ?? VMA(P)     ::
                                    s3_mem_res_vma

    // better for the fully associative vlb
    vlb_res_o(i).bits.pmt := hit ?? s2_req_idx  (P.pmtBits.W) ::
                             ext ?? 0.U                       ::
                                    vsc_q(i).idx(P.pmtBits.W)

    // allowed to be valid but with error
    val err = ext     ||
              res_vld && !s3_mem_res ||
            ((res_vld ??  vsc_q(i).vpn         :: s2_req_pld_q.vpn) >
             (res_vld ??  s3_mem_res_vma.bound :: s2_hit_mux.bound))

    vlb_ext_o(i) := TTWExt(P,
                           sx_req_idx_q,
                           vsc_q(i).vpn,
                           err)
  }

  // big partial muxes
  val mem_req_mux = OrM(mem_req_sel, vsc_q)
  val mem_res_mux = OrM(mem_res_sel, vsc_q)
  val mem_res_vma = mem_res_pld.data.asTypeOf(new VSCVMA(P))

  val mem_res_vma_hit  = mem_res_vma.tab.map(e => e.v && (e.csid === csid_i)).U
  val mem_res_vma_attr = mem_res_vma.e ##
                         mem_res_vma.p ##
                         0.U(1.U)      ##
                         mem_res_vma.u ##
                         OrM(mem_res_vma_hit,
                             mem_res_vma.tab.map(_.attr))

  // hit considering sdid
  val mem_res_vma_vld  = mem_res_vma.v &&
                            (mem_res_vma.g || Any(mem_res_vma_hit))

  s3_mem_res     := mem_res && mem_res_vma_vld
  s3_mem_res_idx := mem_res_mux.idx(1 :+ P.vscBits)
  s3_mem_res_way := mem_res_mux.way
  s3_mem_res_vma := VMA(P,
                        mem_res_vma_vld,
                        asid_i,
                        csid_i,
                        mem_res_mux.bot,
                        mem_res_vma.bound,
                        mem_res_vma.offs,
                        mem_res_vma.g ?? mem_res_vma.attr :: mem_res_vma_attr,
                        mem_res_mux.idx((P.vscBits + 1) :+ (P.pmtBits - 1)) ## mem_res_mux.idx(0))


  //
  // output

  val mem_req_arr  = uatp_i(48.W) ## 0.U(6.W)
  val mem_req_idx  = mem_req_mux.idx

  uat_req_i.ready := s1_req_rdy || rst_kill || rst_pend
  uat_req_o       := UATReq(P,
                            uat_req ## 0.U(1.W),
                            uat_req_pld.mcn,
                            uat_req_pld.vec)

  uat_res_o       := s2_hit_inv

  mem_req_o.valid := Any(mem_req_raw)
  mem_req_o.bits  := MemReq(P,
                            Enc(mem_req_sel),
                            mem_req_arr | mem_req_idx)

  mem_res_i.ready := Non(uat_req)

  int_req_o       := s2_req ## s2_req_inv ## s2_hit_any ## s2_clr_any
}
