`include "tb.svh"


class tb_llc extends tb_base;

    ma_vif    m_vif;
    tb_env    m_env;

    ma_mem    m_llc;

    llc_t     m_old     [$];
    llc_t     m_new     [$];
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

        for (int i = 0; i < llcWays; i++)
            m_old.push_back(i);
        foreach (m_req[i])
            m_req[i] = null;
    endfunction

    function void cmp(ref tb_req req, input llc_t idx, bit rnw, bit err, bit [511:0] data);
        if (req == null)
            return;

        if (1'b1 != req.m_vld)
           `err($sformatf("vld mismatch: %0x vs. %s", 1'b1, req.show()));
        if (idx  != req.m_idx)
           `err($sformatf("idx mismatch: %0x vs. %s", idx,  req.show()));
        if (rnw  != req.m_rnw)
           `err($sformatf("rnw mismatch: %0x vs. %s", rnw,  req.show()));
        if (err  != req.m_err)
           `err($sformatf("err mismatch: %0x vs. %s", err,  req.show()));

        if (!err)
            if (data != req.m_data)
               `err($sformatf("dat mismatch: %0x vs. %s", data, req.show()));
    endfunction

    function tb_req chk_req(input llc_t idx);
        tb_req req = m_req[idx];

        if (req == null)
           `err($sformatf("unexpected idx: %x", idx));

        return req;
    endfunction

    function llc_t  chk_new(input llc_t idx);
        int res [$] = m_new.find_first_index(i) with (i == idx);

        if (res.size() == 0)
           `err($sformatf("idx not found: %x", idx));
        else
            m_new.delete(res.pop_front());

        return idx;
    endfunction

    task cha_req();
        forever begin
            tb_req req = null;
            llc_t  idx;
            bit    sel;
            int    res [$];

            if (m_old.size() == 0) begin
                m_vif.llc_req_i_valid     <= 1'b0;
               `waitn(1);
                continue;
            end

           `rands(sel, with { sel dist {
                1'b0 := m_dis_pte[0],
                1'b1 := m_dis_pte[1]
            };});

            if (sel && !m_pte.try_get(req) || !sel)
                if (!m_box.try_get(req)) begin
                    m_vif.llc_req_i_valid <= 1'b0;
                   `waitn(1);
                    continue;
                end

           `waitn(req.m_dly);

            req.init();

            do begin
                sel = 1'b0;

                m_sem.get();
                foreach (m_new[i]) begin
                    tb_req old = m_req[m_new[i]];

                    if (old == null)
                       `err($sformatf("idx not found: %0x", m_new[i]));
                    if (old.m_mcn == req.m_mcn) begin
                        sel = 1'b1;
                        break;
                    end
                end
                m_sem.put();

                if (sel)
                   `waitn(1);
            end while (sel);

           `rands(idx, with {
                idx inside m_old;
            });

            m_vif.llc_req_i_valid         <= 1'b1;
            m_vif.llc_req_i_bits_idx      <= idx;
            m_vif.llc_req_i_bits_rnw      <= req.m_rnw;
            m_vif.llc_req_i_bits_mcn      <= req.m_mcn;
            m_vif.llc_req_i_bits_pcn      <= req.m_pcn;
            m_vif.llc_req_i_bits_data     <= req.m_data;
           `waitt(m_vif.llc_req_i_ready, TO_MAX, "cha req timeout");
            m_vif.llc_req_i_valid         <= 1'b0;

            req.body();

            m_sem.get();
            req.m_idx  = idx;
            m_req[idx] = req;

            res = m_old.find_first_index(i) with (i == idx);

            m_old.delete   (res.pop_front());
            m_new.push_back(idx);
            m_sem.put();
        end
    endtask

    task chd_resp();
        forever begin
            tb_req req;
            llc_t  idx;
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
            idx = chk_new(m_vif.llc_resp_o_bits_idx);
            req = chk_req(idx);

            cmp(req,
                idx,
                m_vif.llc_resp_o_bits_rnw,
                m_vif.llc_resp_o_bits_err,
                m_vif.llc_resp_o_bits_data);

            if (req) begin
                req.post();

                m_old.push_back(idx);
                m_req[idx] = null;
            end
            m_sem.put();
        end
    endtask

    task chk(input int stg, int max);
        forever begin
           `waitn(1);

            m_sem.get();
            foreach (m_new[i]) begin
                tb_req req = m_req[m_new[i]];

                if (req == null)
                   `err($sformatf("idx not found: %x", m_new[i]));
                if (++req.m_cnt >= TO_MAX)
                   `err($sformatf("cha req timeout: %s", req.show()));
            end
            m_sem.put();

            if (!m_new.size() && (m_env.get() <  max))
                m_env.add(1);

            if (!m_new.size() && (m_env.get() == stg) && !m_box.num() && !m_pte.num())
                m_env.add(1);
        end
    endtask

    task chc();
        forever begin
            mcn_t mcn;
            pdn_t mdn;
            bit   hit;
            bit   sel;
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
           `rands(sel, with { sel dist {
                1'b0 := m_dis_pte[0],
                1'b1 := m_dis_pte[1]
            };});

            if (sel && !hit) begin
                tb_req req = new();

                req.m_pte = 1'b1;
                req.m_mcn = mcn;
                req.init();
                req.body();

                m_pte.put(req);
            end
        end
    endtask

    task main(input int stg, int max);
        fork
            cha_req ();
            chd_resp();
            chk     (stg, max);
            chc     ();
        join
    endtask

endclass