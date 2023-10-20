`include "tb.svh"


class tb_ctl extends tb_base;

    tb_vif m_vif;
    tb_env m_env;
    tb_vsc m_gen;

    function new(ref tb_vif vif);
        m_mod = "ctl";
        m_vif =  vif;
        m_gen =  tb_vsc::get_inst();

       `get(tb_env, "env", m_env);
    endfunction

    task main(input int stg, int max);
        forever begin
            m_env.blk(stg);

            m_gen.init();

            m_vif.satp_i       <= m_gen.m_atp;
            m_vif.uatp_i       <= m_gen.m_atp;
            m_vif.uatc_i_idx   <= m_gen.m_cfg_idx;
            m_vif.uatc_i_vsc   <= m_gen.m_cfg_vsc;
            m_vif.uatc_i_top   <= m_gen.m_cfg_top;
            m_vif.uatc_i_tsl   <= m_gen.m_cfg_tsl;
            m_vif.uatc_i_mmask <= m_gen.m_cfg_mmask;
            m_vif.uatc_i_imask <= m_gen.m_cfg_imask;
            m_vif.uatc_i_vmask <= m_gen.m_cfg_vmask;
            m_vif.uatc_i_tmask <= m_gen.m_cfg_tmask;

            m_env.add(1);

            m_env.blk(1 + stg + max);
            m_env.set(0);
        end
    endtask

endclass
