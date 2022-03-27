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

    tb_vlb_intf
    m_ilb (.clock (clock),
           .reset (reset));

    tb_vlb_intf
    m_jlb (.clock (clock),
           .reset (reset));

    tb_vlb_intf
    m_dlb (.clock (clock),
           .reset (reset));

    tb_mem_intf
    m_mem (.clock (clock),
           .reset (reset));

    wire         vlb_fill_o_valid;
    wire [ 5:0]  vlb_fill_o_bits_idx;
    wire         vlb_fill_o_bits_vld;
    wire         vlb_fill_o_bits_err;
    wire [51:0]  vlb_fill_o_bits_mpn;
    wire [ 3:0]  vlb_fill_o_bits_attr;
    wire [ 1:0]  vlb_kill_i;
    wire         vlb_busy_o;

    FST
    u_fst (.clock                  (clock                     ),
           .reset                  (reset                     ),
           .ilb_req_i_0_valid      (m_ilb.vlb_req_i_valid     ),
           .ilb_req_i_0_bits_idx   (m_ilb.vlb_req_i_bits_idx  ),
           .ilb_req_i_0_bits_vpn   (m_ilb.vlb_req_i_bits_vpn  ),
           .ilb_req_i_0_bits_kill  (m_ilb.vlb_req_i_bits_kill ),
           .ilb_resp_o_0_valid     (m_ilb.vlb_resp_o_valid    ),
           .ilb_resp_o_0_bits_idx  (m_ilb.vlb_resp_o_bits_idx ),
           .ilb_resp_o_0_bits_vld  (m_ilb.vlb_resp_o_bits_vld ),
           .ilb_resp_o_0_bits_err  (m_ilb.vlb_resp_o_bits_err ),
           .ilb_resp_o_0_bits_mpn  (m_ilb.vlb_resp_o_bits_mpn ),
           .ilb_resp_o_0_bits_attr (m_ilb.vlb_resp_o_bits_attr),
           .ilb_req_i_1_valid      (m_jlb.vlb_req_i_valid     ),
           .ilb_req_i_1_bits_idx   (m_jlb.vlb_req_i_bits_idx  ),
           .ilb_req_i_1_bits_vpn   (m_jlb.vlb_req_i_bits_vpn  ),
           .ilb_req_i_1_bits_kill  (m_jlb.vlb_req_i_bits_kill ),
           .ilb_resp_o_1_valid     (m_jlb.vlb_resp_o_valid    ),
           .ilb_resp_o_1_bits_idx  (m_jlb.vlb_resp_o_bits_idx ),
           .ilb_resp_o_1_bits_vld  (m_jlb.vlb_resp_o_bits_vld ),
           .ilb_resp_o_1_bits_err  (m_jlb.vlb_resp_o_bits_err ),
           .ilb_resp_o_1_bits_mpn  (m_jlb.vlb_resp_o_bits_mpn ),
           .ilb_resp_o_1_bits_attr (m_jlb.vlb_resp_o_bits_attr),
           .ilb_fill_o_valid       (vlb_fill_o_valid          ),
           .ilb_fill_o_bits_idx    (vlb_fill_o_bits_idx       ),
           .ilb_fill_o_bits_vld    (vlb_fill_o_bits_vld       ),
           .ilb_fill_o_bits_err    (vlb_fill_o_bits_err       ),
           .ilb_fill_o_bits_mpn    (vlb_fill_o_bits_mpn       ),
           .ilb_fill_o_bits_attr   (vlb_fill_o_bits_attr      ),
           .ilb_kill_i             (vlb_kill_i                ),
           .ilb_busy_o             (vlb_busy_o                ),
           .dlb_req_i_valid        (m_dlb.vlb_req_i_valid     ),
           .dlb_req_i_bits_idx     (m_dlb.vlb_req_i_bits_idx  ),
           .dlb_req_i_bits_vpn     (m_dlb.vlb_req_i_bits_vpn  ),
           .dlb_req_i_bits_kill    (m_dlb.vlb_req_i_bits_kill ),
           .dlb_resp_o_valid       (m_dlb.vlb_resp_o_valid    ),
           .dlb_resp_o_bits_idx    (m_dlb.vlb_resp_o_bits_idx ),
           .dlb_resp_o_bits_vld    (m_dlb.vlb_resp_o_bits_vld ),
           .dlb_resp_o_bits_err    (m_dlb.vlb_resp_o_bits_err ),
           .dlb_resp_o_bits_mpn    (m_dlb.vlb_resp_o_bits_mpn ),
           .dlb_resp_o_bits_attr   (m_dlb.vlb_resp_o_bits_attr),
           .dlb_fill_o_valid       (m_dlb.vlb_fill_o_valid    ),
           .dlb_fill_o_bits_idx    (m_dlb.vlb_fill_o_bits_idx ),
           .dlb_fill_o_bits_vld    (m_dlb.vlb_fill_o_bits_vld ),
           .dlb_fill_o_bits_err    (m_dlb.vlb_fill_o_bits_err ),
           .dlb_fill_o_bits_mpn    (m_dlb.vlb_fill_o_bits_mpn ),
           .dlb_fill_o_bits_attr   (m_dlb.vlb_fill_o_bits_attr),
           .dlb_kill_i             (m_dlb.vlb_kill_i          ),
           .dlb_busy_o             (m_dlb.vlb_busy_o          ),
           .mem_req_o_ready        (m_mem.mem_req_o_ready     ),
           .mem_req_o_valid        (m_mem.mem_req_o_valid     ),
           .mem_req_o_bits_mcn     (m_mem.mem_req_o_bits_mcn  ),
           .mem_resp_i_ready       (m_mem.mem_resp_i_ready    ),
           .mem_resp_i_valid       (m_mem.mem_resp_i_valid    ),
           .mem_resp_i_bits_data   (m_mem.mem_resp_i_bits_data),
           .satp_i                 (m_mem.satp_i              ));

    // special
    reg  ilb_kill_q;
    reg  jlb_kill_q;

    always_ff @(posedge clock or posedge reset)
        if (reset)
            ilb_kill_q <= 1'b0;
        else if (m_ilb.vlb_kill_i[0] | vlb_kill_i[0])
            ilb_kill_q <= m_ilb.vlb_kill_i[0] & ~vlb_kill_i[0];

    always_ff @(posedge clock or posedge reset)
        if (reset)
            jlb_kill_q <= 1'b0;
        else if (m_jlb.vlb_kill_i[0] | vlb_kill_i[0])
            jlb_kill_q <= m_jlb.vlb_kill_i[0] & ~vlb_kill_i[0];

    assign vlb_kill_i = {(m_ilb.vlb_kill_i[1] | ilb_kill_q) &
                         (m_jlb.vlb_kill_i[1] | jlb_kill_q),
                          m_ilb.vlb_kill_i[0] |
                          m_jlb.vlb_kill_i[0]};

    assign m_ilb.vlb_busy_o           = vlb_busy_o       & ~vlb_fill_o_bits_idx[5];
    assign m_ilb.vlb_fill_o_valid     = vlb_fill_o_valid & ~vlb_fill_o_bits_idx[5];
    assign m_ilb.vlb_fill_o_bits_idx  = vlb_fill_o_bits_idx;
    assign m_ilb.vlb_fill_o_bits_vld  = vlb_fill_o_bits_vld;
    assign m_ilb.vlb_fill_o_bits_err  = vlb_fill_o_bits_err;
    assign m_ilb.vlb_fill_o_bits_mpn  = vlb_fill_o_bits_mpn;
    assign m_ilb.vlb_fill_o_bits_attr = vlb_fill_o_bits_attr;

    assign m_jlb.vlb_busy_o           = vlb_busy_o       &  vlb_fill_o_bits_idx[5];
    assign m_jlb.vlb_fill_o_valid     = vlb_fill_o_valid &  vlb_fill_o_bits_idx[5];
    assign m_jlb.vlb_fill_o_bits_idx  = vlb_fill_o_bits_idx;
    assign m_jlb.vlb_fill_o_bits_vld  = vlb_fill_o_bits_vld;
    assign m_jlb.vlb_fill_o_bits_err  = vlb_fill_o_bits_err;
    assign m_jlb.vlb_fill_o_bits_mpn  = vlb_fill_o_bits_mpn;
    assign m_jlb.vlb_fill_o_bits_attr = vlb_fill_o_bits_attr;


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
        m_env = new(m_ilb, m_jlb, m_dlb, m_mem);

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