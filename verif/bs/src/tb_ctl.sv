`include "tb.svh"


class tb_ctl extends tb_base;

    tb_vif m_vif;
    tb_env m_env;
    tb_gen m_gen;

    function new(ref tb_vif vif);
        m_mod = "ctl";
        m_vif =  vif;
        m_gen =  tb_gen::get_inst();

       `get(tb_env, "env", m_env);
    endfunction

    task main();
        forever begin
            // from tb_llc
            m_env.blk(1);

            m_gen.init();

            for (int i = 0; i <= ptwLvl; i++) begin
                m_vif.ctl_req_i_valid     <= 1'b1;
                m_vif.ctl_req_i_bits_rnw  <= 1'b0;
                m_vif.ctl_req_i_bits_addr <= i[3:0];
                m_vif.ctl_req_i_bits_data <= m_gen.m_ctl[i];
               `waitt(m_vif.ctl_req_i_ready,  TO_MAX, "req timeout");
                m_vif.ctl_req_i_valid     <= 1'b0;

                m_vif.ctl_resp_o_ready    <= 1'b1;
               `waitt(m_vif.ctl_resp_o_valid, TO_MAX, "resp timeout");
                m_vif.ctl_resp_o_ready    <= 1'b0;

                if (~m_vif.ctl_resp_o_bits_sel)
                   `err($sformatf("failed: %0d", i));
            end

            // to tb_seq
            m_env.set(2);
        end
    endtask

endclass