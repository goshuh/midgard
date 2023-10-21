module tb;

    //
    // clock & reset

    reg clock;
    reg reset;

    int rst_time;
    int end_time;
    int clk_period;

    initial begin
        verif::plusargs arg = new("tb.");

        rst_time   = arg.get_int("rst", 25);
        end_time   = arg.get_int("end", 100000000);
        clk_period = arg.get_int("clk", 10) / 2;

        clock = 1'b0;
        reset = 1'b1;

        #rst_time
        reset = 1'b0;

        #end_time
        $finish();
    end

    initial begin
        #0

        forever
            #clk_period clock = ~clock;
    end


    //
    // random seed

    int seed;

    initial begin
        verif::plusargs arg = new("");

        seed = arg.get_int("seed", 0);

        void'($random(seed));
    end


    //
    // connections

    tb_ctl_intf
    m_ctl (.clock (clock),
           .reset (reset));

    tb_vlb_intf
    m_ilb (.clock (clock),
           .reset (reset));

    tb_vlb_intf
    m_jlb (.clock (clock),
           .reset (reset));

    tb_vlb_intf
    m_dlb (.clock (clock),
           .reset (reset));

    tb_vlb_intf
    m_elb (.clock (clock),
           .reset (reset));

    tb_mem_intf
    m_mem (.clock (clock),
           .reset (reset));

    wire        ilb_ttw_o_valid;
    wire [ 5:0] ilb_ttw_o_bits_idx;
    wire        ilb_ttw_o_bits_vld;
    wire        ilb_ttw_o_bits_err;
    wire [51:0] ilb_ttw_o_bits_mpn;
    wire [ 7:0] ilb_ttw_o_bits_attr;
    wire [ 2:0] ilb_kill_i;
    wire        ilb_busy_o;

    wire        dlb_ttw_o_valid;
    wire [ 5:0] dlb_ttw_o_bits_idx;
    wire        dlb_ttw_o_bits_vld;
    wire        dlb_ttw_o_bits_err;
    wire [51:0] dlb_ttw_o_bits_mpn;
    wire [ 7:0] dlb_ttw_o_bits_attr;
    wire [ 2:0] dlb_kill_i;
    wire        dlb_busy_o;

    DUT
    u_dut (.clock                 (clock                         ),
           .reset                 (reset                         ),
           .ilb_req_i_0_valid     (m_ilb.vlb_req_i_valid         ),
           .ilb_req_i_0_bits_idx  (m_ilb.vlb_req_i_bits_idx      ),
           .ilb_req_i_0_bits_vpn  (m_ilb.vlb_req_i_bits_vpn      ),
           .ilb_req_i_0_bits_kill (m_ilb.vlb_req_i_bits_kill     ),
           .ilb_res_o_0_valid     (m_ilb.vlb_res_o_valid         ),
           .ilb_res_o_0_bits_idx  (m_ilb.vlb_res_o_bits_idx      ),
           .ilb_res_o_0_bits_vld  (m_ilb.vlb_res_o_bits_vld      ),
           .ilb_res_o_0_bits_err  (m_ilb.vlb_res_o_bits_err      ),
           .ilb_res_o_0_bits_mpn  (m_ilb.vlb_res_o_bits_mpn      ),
           .ilb_res_o_0_bits_attr (m_ilb.vlb_res_o_bits_attr     ),
           .ilb_req_i_1_valid     (m_jlb.vlb_req_i_valid         ),
           .ilb_req_i_1_bits_idx  (m_jlb.vlb_req_i_bits_idx      ),
           .ilb_req_i_1_bits_vpn  (m_jlb.vlb_req_i_bits_vpn      ),
           .ilb_req_i_1_bits_kill (m_jlb.vlb_req_i_bits_kill     ),
           .ilb_res_o_1_valid     (m_jlb.vlb_res_o_valid         ),
           .ilb_res_o_1_bits_idx  (m_jlb.vlb_res_o_bits_idx      ),
           .ilb_res_o_1_bits_vld  (m_jlb.vlb_res_o_bits_vld      ),
           .ilb_res_o_1_bits_err  (m_jlb.vlb_res_o_bits_err      ),
           .ilb_res_o_1_bits_mpn  (m_jlb.vlb_res_o_bits_mpn      ),
           .ilb_res_o_1_bits_attr (m_jlb.vlb_res_o_bits_attr     ),
           .ilb_ttw_o_valid       (ilb_ttw_o_valid               ),
           .ilb_ttw_o_bits_idx    (ilb_ttw_o_bits_idx            ),
           .ilb_ttw_o_bits_vld    (ilb_ttw_o_bits_vld            ),
           .ilb_ttw_o_bits_err    (ilb_ttw_o_bits_err            ),
           .ilb_ttw_o_bits_mpn    (ilb_ttw_o_bits_mpn            ),
           .ilb_ttw_o_bits_attr   (ilb_ttw_o_bits_attr           ),
           .ilb_kill_i            (ilb_kill_i                    ),
           .ilb_busy_o            (ilb_busy_o                    ),
           .dlb_req_i_0_valid     (m_dlb.vlb_req_i_valid         ),
           .dlb_req_i_0_bits_idx  (m_dlb.vlb_req_i_bits_idx      ),
           .dlb_req_i_0_bits_vpn  (m_dlb.vlb_req_i_bits_vpn      ),
           .dlb_req_i_0_bits_kill (m_dlb.vlb_req_i_bits_kill[1:0]),
           .dlb_res_o_0_valid     (m_dlb.vlb_res_o_valid         ),
           .dlb_res_o_0_bits_idx  (m_dlb.vlb_res_o_bits_idx      ),
           .dlb_res_o_0_bits_vld  (m_dlb.vlb_res_o_bits_vld      ),
           .dlb_res_o_0_bits_err  (m_dlb.vlb_res_o_bits_err      ),
           .dlb_res_o_0_bits_mpn  (m_dlb.vlb_res_o_bits_mpn      ),
           .dlb_res_o_0_bits_attr (m_dlb.vlb_res_o_bits_attr     ),
           .dlb_req_i_1_valid     (m_elb.vlb_req_i_valid         ),
           .dlb_req_i_1_bits_idx  (m_elb.vlb_req_i_bits_idx      ),
           .dlb_req_i_1_bits_vpn  (m_elb.vlb_req_i_bits_vpn      ),
           .dlb_req_i_1_bits_kill (m_elb.vlb_req_i_bits_kill[1:0]),
           .dlb_res_o_1_valid     (m_elb.vlb_res_o_valid         ),
           .dlb_res_o_1_bits_idx  (m_elb.vlb_res_o_bits_idx      ),
           .dlb_res_o_1_bits_vld  (m_elb.vlb_res_o_bits_vld      ),
           .dlb_res_o_1_bits_err  (m_elb.vlb_res_o_bits_err      ),
           .dlb_res_o_1_bits_mpn  (m_elb.vlb_res_o_bits_mpn      ),
           .dlb_res_o_1_bits_attr (m_elb.vlb_res_o_bits_attr     ),
           .dlb_ttw_o_valid       (dlb_ttw_o_valid               ),
           .dlb_ttw_o_bits_idx    (dlb_ttw_o_bits_idx            ),
           .dlb_ttw_o_bits_vld    (dlb_ttw_o_bits_vld            ),
           .dlb_ttw_o_bits_err    (dlb_ttw_o_bits_err            ),
           .dlb_ttw_o_bits_mpn    (dlb_ttw_o_bits_mpn            ),
           .dlb_ttw_o_bits_attr   (dlb_ttw_o_bits_attr           ),
           .dlb_kill_i            (dlb_kill_i                    ),
           .dlb_busy_o            (dlb_busy_o                    ),
           .vtd_req_i_wnr         (m_ctl.vtd_req_i_wnr           ),
           .vtd_req_i_mcn         (m_ctl.vtd_req_i_mcn           ),
           .vtd_req_i_vec         (m_ctl.vtd_req_i_vec           ),
           .mem_req_o_ready       (m_mem.mem_req_o_ready         ),
           .mem_req_o_valid       (m_mem.mem_req_o_valid         ),
           .mem_req_o_bits_idx    (m_mem.mem_req_o_bits_idx      ),
           .mem_req_o_bits_mcn    (m_mem.mem_req_o_bits_mcn      ),
           .mem_res_i_ready       (m_mem.mem_res_i_ready         ),
           .mem_res_i_valid       (m_mem.mem_res_i_valid         ),
           .mem_res_i_bits_idx    (m_mem.mem_res_i_bits_idx      ),
           .mem_res_i_bits_data   (m_mem.mem_res_i_bits_data     ),
           .satp_i                (m_ctl.satp_i                  ),
           .uatp_i                (m_ctl.uatp_i                  ),
           .uatc_i_idx            (m_ctl.uatc_i_idx              ),
           .uatc_i_vsc            (m_ctl.uatc_i_vsc              ),
           .uatc_i_top            (m_ctl.uatc_i_top              ),
           .uatc_i_tsl            (m_ctl.uatc_i_tsl              ),
           .uatc_i_mmask          (m_ctl.uatc_i_mmask            ),
           .uatc_i_imask          (m_ctl.uatc_i_imask            ),
           .uatc_i_vmask          (m_ctl.uatc_i_vmask            ),
           .uatc_i_tmask          (m_ctl.uatc_i_tmask            ));

    assign ilb_kill_i = {{2{m_ilb.vlb_kill_i[1] &
                            m_jlb.vlb_kill_i[1]}},
                            m_ilb.vlb_kill_i[0] |
                            m_jlb.vlb_kill_i[0]};

    assign dlb_kill_i = {{2{m_dlb.vlb_kill_i[1] |
                            m_elb.vlb_kill_i[1]}},
                            m_dlb.vlb_kill_i[0] |
                            m_elb.vlb_kill_i[0]};

    assign m_ilb.vlb_busy_o          = ilb_busy_o      & ~ilb_ttw_o_bits_idx[5];
    assign m_ilb.vlb_ttw_o_valid     = ilb_ttw_o_valid & ~ilb_ttw_o_bits_idx[5];
    assign m_ilb.vlb_ttw_o_bits_idx  = ilb_ttw_o_bits_idx;
    assign m_ilb.vlb_ttw_o_bits_vld  = ilb_ttw_o_bits_vld;
    assign m_ilb.vlb_ttw_o_bits_err  = ilb_ttw_o_bits_err;
    assign m_ilb.vlb_ttw_o_bits_mpn  = ilb_ttw_o_bits_mpn;
    assign m_ilb.vlb_ttw_o_bits_attr = ilb_ttw_o_bits_attr;

    assign m_jlb.vlb_busy_o          = ilb_busy_o      &  ilb_ttw_o_bits_idx[5];
    assign m_jlb.vlb_ttw_o_valid     = ilb_ttw_o_valid &  ilb_ttw_o_bits_idx[5];
    assign m_jlb.vlb_ttw_o_bits_idx  = ilb_ttw_o_bits_idx;
    assign m_jlb.vlb_ttw_o_bits_vld  = ilb_ttw_o_bits_vld;
    assign m_jlb.vlb_ttw_o_bits_err  = ilb_ttw_o_bits_err;
    assign m_jlb.vlb_ttw_o_bits_mpn  = ilb_ttw_o_bits_mpn;
    assign m_jlb.vlb_ttw_o_bits_attr = ilb_ttw_o_bits_attr;

    assign m_dlb.vlb_busy_o          = dlb_busy_o      & ~dlb_ttw_o_bits_idx[5];
    assign m_dlb.vlb_ttw_o_valid     = dlb_ttw_o_valid & ~dlb_ttw_o_bits_idx[5];
    assign m_dlb.vlb_ttw_o_bits_idx  = dlb_ttw_o_bits_idx;
    assign m_dlb.vlb_ttw_o_bits_vld  = dlb_ttw_o_bits_vld;
    assign m_dlb.vlb_ttw_o_bits_err  = dlb_ttw_o_bits_err;
    assign m_dlb.vlb_ttw_o_bits_mpn  = dlb_ttw_o_bits_mpn;
    assign m_dlb.vlb_ttw_o_bits_attr = dlb_ttw_o_bits_attr;

    assign m_elb.vlb_busy_o          = dlb_busy_o      &  dlb_ttw_o_bits_idx[5];
    assign m_elb.vlb_ttw_o_valid     = dlb_ttw_o_valid &  dlb_ttw_o_bits_idx[5];
    assign m_elb.vlb_ttw_o_bits_idx  = dlb_ttw_o_bits_idx;
    assign m_elb.vlb_ttw_o_bits_vld  = dlb_ttw_o_bits_vld;
    assign m_elb.vlb_ttw_o_bits_err  = dlb_ttw_o_bits_err;
    assign m_elb.vlb_ttw_o_bits_mpn  = dlb_ttw_o_bits_mpn;
    assign m_elb.vlb_ttw_o_bits_attr = dlb_ttw_o_bits_attr;


    //
    // err delaying

    initial begin
        // modified by g_logger
        wait(`trig >= 0);

        repeat (`trig)
            @(posedge clock);

        $finish();
    end


    //
    // test launching

    tb_env m_env;

    initial begin
        m_env = new(m_ctl, m_ilb, m_jlb, m_dlb, m_elb, m_mem);

        @(negedge reset);
        @(posedge clock);

        m_env.main();
    end

    final begin
        // suppress any outstanding liveness assertions
        $assertkill();

        $display("*** TEST %0s ***", `trig < 0 ? "PASSED" : "FAILED");
    end


    //
    // waveform dumping

    initial begin
`ifdef DUMPFSDB
        $fsdbDumpfile("dump.fsdb");
        $fsdbDumpvars();
        $fsdbDumpMDA();

        forever begin
            repeat (100000)
                @(posedge clock);

            $fsdbDumpflush();
        end
`endif

`ifdef DUMPSHM
        $shm_open("dump.shm");
        $shm_probe("AS");
`endif
    end

endmodule
