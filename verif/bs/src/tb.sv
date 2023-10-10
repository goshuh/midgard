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
    m_vif (.clock (clock),
           .reset (reset));

    tb_llc_intf
    m_llc (.clock (clock),
           .reset (reset));

    tb_mem_intf
    m_mem (.clock (clock),
           .reset (reset));

    MMU
    u_mmu (.clock               (clock                    ),
           .reset               (reset                    ),
           .llc_req_i_ready     (m_llc.llc_req_i_ready    ),
           .llc_req_i_valid     (m_llc.llc_req_i_valid    ),
           .llc_req_i_bits_idx  (m_llc.llc_req_i_bits_idx ),
           .llc_req_i_bits_wnr  (m_llc.llc_req_i_bits_wnr ),
           .llc_req_i_bits_mcn  (m_llc.llc_req_i_bits_mcn ),
           .llc_req_i_bits_pcn  (m_llc.llc_req_i_bits_pcn ),
           .llc_req_i_bits_data (m_llc.llc_req_i_bits_data),
           .llc_res_o_ready     (m_llc.llc_res_o_ready    ),
           .llc_res_o_valid     (m_llc.llc_res_o_valid    ),
           .llc_res_o_bits_idx  (m_llc.llc_res_o_bits_idx ),
           .llc_res_o_bits_err  (m_llc.llc_res_o_bits_err ),
           .llc_res_o_bits_wnr  (m_llc.llc_res_o_bits_wnr ),
           .llc_res_o_bits_data (m_llc.llc_res_o_bits_data),
           .llc_req_o_ready     (m_llc.llc_req_o_ready    ),
           .llc_req_o_valid     (m_llc.llc_req_o_valid    ),
           .llc_req_o_bits_mcn  (m_llc.llc_req_o_bits_mcn ),
           .llc_res_i_ready     (m_llc.llc_res_i_ready    ),
           .llc_res_i_valid     (m_llc.llc_res_i_valid    ),
           .llc_res_i_bits_hit  (m_llc.llc_res_i_bits_hit ),
           .llc_res_i_bits_data (m_llc.llc_res_i_bits_data),
           .mem_req_o_ready     (m_mem.mem_req_o_ready    ),
           .mem_req_o_valid     (m_mem.mem_req_o_valid    ),
           .mem_req_o_bits_idx  (m_mem.mem_req_o_bits_idx ),
           .mem_req_o_bits_wnr  (m_mem.mem_req_o_bits_wnr ),
           .mem_req_o_bits_mcn  (m_mem.mem_req_o_bits_mcn ),
           .mem_req_o_bits_pcn  (m_mem.mem_req_o_bits_pcn ),
           .mem_req_o_bits_data (m_mem.mem_req_o_bits_data),
           .mem_res_i_ready     (m_mem.mem_res_i_ready    ),
           .mem_res_i_valid     (m_mem.mem_res_i_valid    ),
           .mem_res_i_bits_idx  (m_mem.mem_res_i_bits_idx ),
           .mem_res_i_bits_err  (m_mem.mem_res_i_bits_err ),
           .mem_res_i_bits_wnr  (m_mem.mem_res_i_bits_wnr ),
           .mem_res_i_bits_data (m_mem.mem_res_i_bits_data),
           .ctl_i_0             (m_vif.ctl_i[0]           ),
           .ctl_i_1             (m_vif.ctl_i[1]           ),
           .ctl_i_2             (m_vif.ctl_i[2]           ),
           .ctl_i_3             (m_vif.ctl_i[3]           ),
           .ctl_i_4             (m_vif.ctl_i[4]           ),
           .ctl_i_5             (m_vif.ctl_i[5]           ),
           .ctl_i_6             (m_vif.ctl_i[6]           ),
           .ctl_i_7             (m_vif.ctl_i[7]           ),
           .rst_i               (m_vif.rst_i              ));


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
        m_env = new(m_vif, m_llc, m_mem);

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
