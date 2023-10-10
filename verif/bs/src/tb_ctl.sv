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

    task main(input int stg, int max);
        forever begin
            m_env.blk(stg);

            m_gen.init();

            for (int i = 0;          i <= ptwLvl; i++)
                m_vif.ctl_i[i] <= m_gen.m_ctl[i];
            for (int i = ptwLvl + 1; i <= 7;      i++)
                m_vif.ctl_i[i] <= 64'b0;

           `waitn(1);
            m_vif.rst_i        <= 1'b1;
           `waitn(1);
            m_vif.rst_i        <= 1'b0;

            m_env.add(1);

            m_env.blk(1 + stg + max);
            m_env.set(0);
        end
    endtask

endclass
