`include "tb.svh"


class tb_llc extends tb_base;

    ma_vif    m_vif;
    tb_env    m_env;

    ma_mem    m_llc;

    tb_req    m_req     [llcWays-1:0];
    semaphore m_sem;
    mailbox   m_box;
    mailbox   m_pte;

    int       m_dis_req [2];
    int       m_dis_acc [2];
    int       m_dis_pte [2];

    function new(ref ma_vif vif);
        verif::plusargs arg = new("llc.");

        m_mod = "llc";
        m_vif =  vif;
        m_sem =  new(1);
        m_pte =  new(0);

       `get(tb_env,  "env", m_env);
       `get(ma_mem,  "llc", m_llc);
       `get(mailbox, "box", m_box);

        m_dis_req = {arg.get_int("req_min", 0), arg.get_int("req_max", 100)};
        m_dis_acc = {arg.get_int("acc_min", 0), arg.get_int("acc_max", 100)};
        m_dis_pte = {arg.get_int("pte_clr", 1), arg.get_int("pte_set",   1)};

        foreach (m_req[i])
            m_req[i] = new();
    endfunction

    task cha_req();
        forever begin
            tb_req req = null;
            llc_t  idx;
            bit    vld;

            m_sem.get();
            foreach (m_req[i])
                if (!m_req[i].m_vld) begin
                    req = m_req[i];
                    idx = i[llcIdx-1:0];

                    break;
                end
            m_sem.put();

            if (req == null) begin
               `waitn(1);
                continue;
            end

           `rands(vld, with { vld dist {
                1'b0 := m_dis_pte[0],
                1'b1 := m_dis_pte[1]
            };});

            if (vld && !m_pte.try_get(req) || !vld)
                if (!m_box.try_get(req)) begin
                    // to tb_llc
                    m_env.set(0);

                    // from tb_seq
                    m_env.blk(3);
                    continue;
                end

           `waitn(req.m_dly);

            req.init();

            do begin
                vld = 1'b0;

                m_sem.get();
                foreach (m_req[i])
                    if  (m_req[i].m_vld && (m_req[i].m_mcn == req.m_mcn)) begin
                        vld = 1'b1;
                        break;
                    end
                m_sem.put();

                if (vld)
                   `waitn(1);
            end while (vld);

            m_vif.llc_req_i_valid     <= 1'b1;
            m_vif.llc_req_i_bits_idx  <= idx;
            m_vif.llc_req_i_bits_rnw  <= req.m_rnw;
            m_vif.llc_req_i_bits_mcn  <= req.m_mcn;
            m_vif.llc_req_i_bits_pcn  <= req.m_pcn;
            m_vif.llc_req_i_bits_data <= req.m_data;
           `waitt(m_vif.llc_req_i_ready, TO_MAX, "cha req timeout");
            m_vif.llc_req_i_valid     <= 1'b0;

            req.body();

            m_sem.get();
            req.m_idx  = idx;
            m_req[idx] = req;
            m_sem.put();
        end
    endtask

    task chd_resp();
        forever begin
            tb_req req;
            int    dly = `urand(m_dis_req[0], m_dis_req[1]);

            m_vif.llc_resp_o_ready     <= dly == 0;
           `waits(m_vif.llc_resp_o_valid);

            if (dly) begin
               `waitn(dly - 1);
                m_vif.llc_resp_o_ready <= 1'b1;
               `waitn(1);
            end
            m_vif.llc_resp_o_ready     <= 1'b0;

            m_sem.get();
            req = m_req[m_vif.llc_resp_o_bits_idx];

            if (req.m_vld == 1'b0)
               `err($sformatf("unexpected idx: %0x vs. %s", m_vif.llc_resp_o_bits_idx, req.show()));
            else begin
                if (req.m_rnw != m_vif.llc_resp_o_bits_rnw)
                   `err($sformatf("rnw mismatch: %0x vs. %s", m_vif.llc_resp_o_bits_rnw, req.show()));
                if (req.m_err != m_vif.llc_resp_o_bits_err)
                   `err($sformatf("err mismatch: %0x vs. %s", m_vif.llc_resp_o_bits_err, req.show()));

                if (req.m_err == 1'b0)
                    if (m_vif.llc_resp_o_bits_data != req.m_data)
                       `err($sformatf("dat mismatch: %0x vs. %s", m_vif.llc_resp_o_bits_data, req.show()));

                req.post();
            end

            m_sem.put();
        end
    endtask

    task chk();
        forever begin
            int num = 0;

           `waitn(1);

            m_sem.get();
            foreach (m_req[i])
                if  (m_req[i].m_vld) begin
                    if (++m_req[i].m_cnt >= TO_MAX)
                       `err($sformatf("cha req timeout: %0d", i));

                    num++;
                end
            m_sem.put();

            if (!num && !m_env.get()) begin
                // to tb_ctl
                m_env.set(1);

                // no any existing pte reqs
                m_pte = new(0);
            end
        end
    endtask

    task chc();
        forever begin
            mcn_t mcn;
            pdn_t mdn;
            bit   hit;
            bit   pte;
            int   dly = `urand(m_dis_req[0], m_dis_req[1]);

            m_vif.llc_req_o_ready      <= dly == 0;
           `waits(m_vif.llc_req_o_valid);

            if (dly) begin
               `waitn(dly - 1);
                m_vif.llc_req_o_ready  <= 1'b1;
               `waitn(1);
            end
            m_vif.llc_req_o_ready      <= 1'b0;

            mcn =  m_vif.llc_req_o_bits_mcn;
            mdn = {mcn, 3'b0};
            hit =  m_llc.chk(mdn, 8);

            dly = `urand(m_dis_acc[0], m_dis_acc[1]);
           `waitn(dly);

            m_vif.llc_resp_i_valid     <= 1'b1;
            m_vif.llc_resp_i_bits_hit  <= hit;
            m_vif.llc_resp_i_bits_data <= m_llc.get_d(mdn);
           `waitt(m_vif.llc_resp_i_ready, TO_MAX, "chc resp timeout");
            m_vif.llc_resp_i_valid     <= 1'b0;

            // llc actively loads ptes back
           `rands(pte, with { pte dist {
                1'b0 := m_dis_pte[0],
                1'b1 := m_dis_pte[1]
            };});

            if (pte && !hit) begin
                tb_req req = new();

                req.m_pte = 1'b1;
                req.m_mcn = mcn;
                req.init();
                req.body();

                m_pte.put(req);
            end
        end
    endtask

    task main();
        fork
            cha_req ();
            chd_resp();
            chk     ();
            chc     ();
        join
    endtask

endclass