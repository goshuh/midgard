`include "tb.svh"


class tb_l1d extends tb_base;

    ma_vif m_vif;
    tb_gen m_gen;

    ma_mem m_l1d;

    int    m_dis_req [2];
    int    m_dis_acc [2];

    function new(ref ma_vif vif);
        verif::plusargs arg = new("l1d.");

        m_mod = "l1d";
        m_vif =  vif;
        m_gen =  tb_gen::get_inst();

       `get(ma_mem, "l1d", m_l1d);

        m_dis_req = {arg.get_int("req_min", 0), arg.get_int("req_max", 100)};
        m_dis_acc = {arg.get_int("acc_min", 0), arg.get_int("acc_max", 100)};
    endfunction

    task main();
        forever begin
            mcn_t mcn;
            int   dly = `urand(m_dis_req[0], m_dis_req[1]);

            m_vif.satp_i               <= {{64-mpnBits{1'b0}}, m_gen.m_atp};

            m_vif.mem_req_o_ready      <= dly == 0;
           `waits(m_vif.mem_req_o_valid);

            if (dly) begin
               `waitn(dly - 1);
                m_vif.mem_req_o_ready  <= 1'b1;
               `waitn(1);
            end
            m_vif.mem_req_o_ready      <= 1'b0;

            mcn =  m_vif.mem_req_o_bits_mcn;
            dly = `urand(m_dis_acc[0], m_dis_acc[1]);
           `waitn(dly);

            m_vif.mem_resp_i_valid     <= 1'b1;
            m_vif.mem_resp_i_bits_data <= m_l1d.get_b(mcn);
           `waitt(m_vif.mem_resp_i_ready, TO_MAX, "l1d resp timeout");
            m_vif.mem_resp_i_valid     <= 1'b0;
        end
    endtask

endclass