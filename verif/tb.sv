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
        end_time   = arg.get_int("end", 10000000);
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
    // connections

    tb_intf
    m_vif (.clock (clock),
           .reset (reset));

    tb_intf_mem
    m_llc (.clock (clock),
           .reset (reset));

    tb_intf_mem
    m_mem (.clock (clock),
           .reset (reset));

    MidgardMMU
    u_dut (.clock                (clock                                    ),
           .reset                (reset                                    ),
           .mmu_req_i_ready      (m_vif.mmu_req_i_ready                    ),
           .mmu_req_i_valid      (m_vif.mmu_req_i_valid                    ),
           .mmu_req_i_bits       (m_vif.mmu_req_i_bits                     ),
           .mmu_resp_o_ready     (m_vif.mmu_resp_o_ready                   ),
           .mmu_resp_o_valid     (m_vif.mmu_resp_o_valid                   ),
           .mmu_resp_o_bits_err  (m_vif.mmu_resp_o_bits_err                ),
           .mmu_resp_o_bits_ppn  (m_vif.mmu_resp_o_bits_ppn                ),
           .llc_req_o_ready      (m_llc.mem_req_o_ready                    ),
           .llc_req_o_valid      (m_llc.mem_req_o_valid                    ),
           .llc_req_o_bits       (m_llc.mem_req_o_bits[tb_base::maBits-1:0]),
           .llc_resp_i_ready     (m_llc.mem_resp_i_ready                   ),
           .llc_resp_i_valid     (m_llc.mem_resp_i_valid                   ),
           .llc_resp_i_bits_hit  (m_llc.mem_resp_i_bits_err                ),
           .llc_resp_i_bits_pte  (m_llc.mem_resp_i_bits_pte                ),
           .mem_req_o_ready      (m_mem.mem_req_o_ready                    ),
           .mem_req_o_valid      (m_mem.mem_req_o_valid                    ),
           .mem_req_o_bits       (m_mem.mem_req_o_bits[tb_base::paBits-1:0]),
           .mem_resp_i_ready     (m_mem.mem_resp_i_ready                   ),
           .mem_resp_i_valid     (m_mem.mem_resp_i_valid                   ),
           .mem_resp_i_bits_err  (m_mem.mem_resp_i_bits_err                ),
           .mem_resp_i_bits_pte  (m_mem.mem_resp_i_bits_pte                ),
           .cfg_req_i_ready      (m_vif.cfg_req_i_ready                    ),
           .cfg_req_i_valid      (m_vif.cfg_req_i_valid                    ),
           .cfg_req_i_bits_ren   (m_vif.cfg_req_i_bits_ren                 ),
           .cfg_req_i_bits_addr  (m_vif.cfg_req_i_bits_addr                ),
           .cfg_req_i_bits_data  (m_vif.cfg_req_i_bits_data                ),
           .cfg_resp_o_ready     (m_vif.cfg_resp_o_ready                   ),
           .cfg_resp_o_valid     (m_vif.cfg_resp_o_valid                   ),
           .cfg_resp_o_bits_vld  (m_vif.cfg_resp_o_bits_vld                ),
           .cfg_resp_o_bits_data (m_vif.cfg_resp_o_bits_data               ));


    //
    // err delaying

    initial begin
        // modified by g_logger
        wait(`trig >= 0);

        repeat(`trig)
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
`endif

`ifdef DUMPSHM
        $shm_open("dump.shm");
        $shm_probe("AS");
`endif
    end

endmodule
