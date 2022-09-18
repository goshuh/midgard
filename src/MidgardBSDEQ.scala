package midgard.backside

import  chisel3._
import  chisel3.util._
import  midgard._
import  midgard.util._


class DEQEntry(val P: Param) extends Bundle {
  val wid  = 31 - P.clWid

  val sel  = Bool()
  val fsm  = UInt(2.W)
  val idx  = UInt(wid.W)
  val pcn  = UInt(P.pcnBits.W)
  val data = UInt(P.clBits.W)
  val mask = UInt(P.clBytes.W)
}


class DEQ(val P: Param) extends Module {

  // ---------------------------
  // io

  val stb_req_i  = IO(Flipped(Decoupled(new MemReq (P, P.deqIdx))))
  val stb_resp_o = IO(        Decoupled(new MemResp(P, P.deqIdx)))

  val mrq_req_o  = IO(        Decoupled(new MemReq (P, P.llcIdx)))
  val mrq_resp_i = IO(Flipped(Decoupled(new MemResp(P, P.llcIdx))))

  val deq_busy_o = IO(           Output(UInt(P.deqWays.W)))
  val deq_head_o = IO(           Output(UInt(32.W)))

  val ctl_i      = IO(            Input(Vec (4, UInt(64.W))))
  val rst_i      = IO(            Input(Bool()))


  // ---------------------------
  // logic

  val
     (fsm_idle ::
      fsm_req  ::
      fsm_pend ::
      fsm_resp ::
      fsm_null) = Enum(4)

  val stb_req      = stb_req_i.fire
  val stb_resp     = stb_resp_o.fire
  val mrq_req      = mrq_req_o.fire
  val mrq_resp     = mrq_resp_i.fire

  val stb_req_sel  = Dec(stb_req_i.bits.idx)
  val mrq_resp_sel = Dec(mrq_resp_i.bits.idx)


  //
  // slots

  val deq_vld     = dontTouch(Wire(Vec (P.deqWays, Bool())))
  val deq_mrq_req = dontTouch(Wire(Vec (P.deqWays, Bool())))
  val deq_stb_req = dontTouch(Wire(Vec (P.deqWays, Bool())))

  val deq_mrq_sel = dontTouch(Wire(UInt(P.deqWays.W)))
  val deq_stb_sel = dontTouch(Wire(UInt(P.deqWays.W)))

  val deq_head_q  = dontTouch(Wire(UInt(32.W)))

  // body
  val deq_q = dontTouch(Wire(Vec(P.deqWays, new DEQEntry(P))))

  for (i <- 0 until P.deqWays) {
    val set = stb_req  && stb_req_sel(i)
    val clr = stb_resp && deq_stb_sel(i)

    val deq_fsm_en  = dontTouch(Wire(Bool()))
    val deq_fsm_nxt = dontTouch(Wire(UInt(3.W)))

    deq_fsm_en  := false.B
    deq_fsm_nxt := deq_q(i).fsm

    switch (deq_q(i).fsm) {
      is (fsm_idle) {
        deq_fsm_en  := set
        deq_fsm_nxt := fsm_req
      }
      is (fsm_req) {
        deq_fsm_en  := mrq_req_o.ready  && deq_mrq_sel (i)
        deq_fsm_nxt := fsm_pend
      }
      is (fsm_pend) {
        deq_fsm_en  := mrq_resp_i.valid && mrq_resp_sel(i)
        deq_fsm_nxt := fsm_resp
      }
      is (fsm_resp) {
        deq_fsm_en  := clr
        deq_fsm_nxt := fsm_idle
      }
    }

    val deq_fsm_is_busy = deq_q(i).fsm =/= fsm_idle
    val deq_fsm_is_req  = deq_q(i).fsm === fsm_req
    val deq_fsm_is_pend = deq_q(i).fsm === fsm_pend
    val deq_fsm_is_resp = deq_q(i).fsm === fsm_resp

    // lock-step
    assert(set -> !deq_fsm_is_busy)

    // make life easier
    deq_q(i).fsm  := RegEnable(deq_fsm_nxt, fsm_idle, deq_fsm_en)

    // variable fields
    val deq_tog      = clr &&  deq_fsm_is_resp
    val deq_set_data = set && !deq_q(i).sel
    val deq_set_misc = set &&  deq_q(i).sel

    deq_q(i).sel  := RegEnable(Non(deq_q(i).sel), false.B, deq_tog)
    deq_q(i).idx  := RegEnable(deq_head_q,                 deq_set_data)
    deq_q(i).data := RegEnable(stb_req_i.bits.data,        deq_set_data)

    deq_q(i).pcn  := RegEnable(stb_req_i.bits.data(P.paBits := P.clWid),   deq_set_misc)
    deq_q(i).mask := RegEnable(stb_req_i.bits.data(64       :+ P.clBytes), deq_set_misc)

    // output
    deq_vld    (i) := deq_fsm_is_busy
    deq_mrq_req(i) := deq_fsm_is_req
    deq_stb_req(i) := deq_fsm_is_resp
  }

  // global order
  deq_head_q := RegEnable(NeQ(rst_i, (deq_head_q + 1.U) & ctl_i(1)(32.W)),
                          0.U,
                          rst_i   ||
                          stb_req && Any(stb_req_sel & ~deq_vld.U))

  // arb
  val mrq_req_vld   = Any(deq_mrq_req)
  val stb_req_vld   = Any(deq_stb_req)
  val arb_mrq_req   = RRA(deq_mrq_req.U, mrq_req)
  val arb_stb_req   = RRA(deq_stb_req.U, stb_resp)

  // stability
  val mrq_req_vld_q = RegEnable(mrq_req_vld && !mrq_req_o.ready,  false.B, mrq_req_vld)
  val stb_req_vld_q = RegEnable(stb_req_vld && !stb_resp_o.ready, false.B, stb_req_vld)
  val arb_mrq_req_q = RegEnable(arb_mrq_req, mrq_req_vld && !mrq_req_vld_q)
  val arb_stb_req_q = RegEnable(arb_stb_req, stb_req_vld && !mrq_req_vld_q)

  deq_mrq_sel := mrq_req_vld_q ?? arb_mrq_req_q :: arb_mrq_req
  deq_stb_sel := stb_req_vld_q ?? arb_stb_req_q :: arb_stb_req

  // big partial mux
  val deq_mrq_mux = OrM(deq_mrq_sel, deq_q)
  val deq_stb_mux = OrM(deq_stb_sel, deq_q)

  // output
  stb_req_i.ready  := true.B

  stb_resp_o.valid := stb_req_vld
  stb_resp_o.bits  := MemResp(P,
                              Enc(deq_stb_sel),
                              false.B,
                              deq_stb_mux.sel,
                              0.U,
                              P.deqIdx)

  // os should guarantee that the region is properly aligned
  val mrq_req_addr = ctl_i(0) | (deq_mrq_mux.idx ## deq_mrq_mux.sel ## 0.U(P.clWid.W))
  val mrq_req_data = deq_mrq_mux.sel ?? (deq_mrq_mux.mask ## Ext(deq_mrq_mux.pcn, 64)) ::
                                         deq_mrq_mux.data

  mrq_req_o.valid  := mrq_req_vld
  mrq_req_o.bits   := MemReq (P,
                              Enc(deq_stb_sel),
                              false.B,
                              0.U,
                              mrq_req_addr,
                              mrq_req_data,
                              P.llcIdx)

  mrq_resp_i.ready := true.B

  deq_busy_o       := deq_vld.U
  deq_head_o       := deq_head_q
}