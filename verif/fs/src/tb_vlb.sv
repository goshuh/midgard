`include "tb.svh"


class tb_vlb extends tb_base;

    tb_vif    m_vif;
    tb_env    m_env;

    vlb_t     m_old     [$];
    vlb_t     m_new     [$];
    tb_req    m_req     [1 << vlbIdx];
    mailbox   m_box;

    bit [1:0] m_kil;
    int       m_dis_get [2];
    int       m_dis_kil [2];

    function new(ref tb_vif vif, input string mod);
        verif::plusargs arg = new({mod, "."});

        m_mod = mod;
        m_vif = vif;

       `get(tb_env,  "env", m_env);
       `get(mailbox, "box", m_box);

        m_dis_get = {arg.get_int("get_clr",   1), arg.get_int("get_set", 100)};
        m_dis_kil = {arg.get_int("kil_clr", 100), arg.get_int("kil_set",   1)};

        for (int i = 0; i < (1 << vlbIdx); i++)
            m_old.push_back(i);
        foreach (m_req[i])
            m_req[i] = null;
    endfunction

    function void cmp(ref tb_req req, input vlb_t idx, bit vld, bit err, mpn_t mpn, bit [3:0] attr);
        if (idx != req.m_idx)
           `err($sformatf("idx mismatch: %0x vs. %s", idx, req.show()));
        if (vld != req.m_res.vld)
           `err($sformatf("vld mismatch: %0x vs. %s", vld, req.show()));
        if (err != req.m_res.err)
           `err($sformatf("err mismatch: %0x vs. %s", err, req.show()));

        if (vld && !err) begin
            if (mpn  != req.m_mpn)
               `err($sformatf("mpn mismatch: %0x vs. %s", mpn,  req.show()));
            if (attr != req.m_res.attr)
               `err($sformatf("att mismatch: %0x vs. %s", attr, req.show()));
        end
    endfunction

    task main();
        forever begin
            tb_req req;
            vlb_t  idx;
            vlb_t  ret [$];
            bit    sel;

            if (m_vif.vlb_resp_o_valid && m_vif.vlb_resp_o_bits_vld) begin
                int res [$];

                idx = m_vif.vlb_resp_o_bits_idx;
                res = m_new.find_first_index(i) with (i == idx);
                req = m_req[idx];

                ret.push_back(idx);

                if (req == null)
                   `err($sformatf("unexpected idx: %x", idx));
                else
                    cmp(req,
                        m_vif.vlb_resp_o_bits_idx,
                        m_vif.vlb_resp_o_bits_vld,
                        m_vif.vlb_resp_o_bits_err,
                        m_vif.vlb_resp_o_bits_mpn,
                        m_vif.vlb_resp_o_bits_attr);

                if (res.size() == 0)
                   `err($sformatf("idx not found: %x", idx));
                else
                    m_new.delete(res.pop_front());
            end

            if (m_vif.vlb_fill_o_valid && (m_vif.vlb_fill_o_bits_err ||
                                          !m_vif.vlb_fill_o_bits_vld)) begin
                int res [$];

                idx = m_vif.vlb_fill_o_bits_idx;
                res = m_new.find_first_index(i) with (i == idx);
                req = m_req[idx];

                ret.push_back(idx);

                if (req == null)
                   `err($sformatf("unexpected idx: %x", idx));
                else
                    cmp(req,
                        m_vif.vlb_fill_o_bits_idx,
                        m_vif.vlb_fill_o_bits_vld,
                        m_vif.vlb_fill_o_bits_err,
                        req.m_mpn,
                        m_vif.vlb_fill_o_bits_attr);

                if (res.size() == 0)
                   `err($sformatf("idx not found: %x", idx));
                else
                    m_new.delete(res.pop_front());
            end

            req = null;

           `rands(sel, with { sel dist {
                1'b0 := m_dis_get[0],
                1'b1 := m_dis_get[1]
            };});

            if (sel && m_old.size() && (m_new.size() < 8)) begin
               `rands(idx, with {
                    idx inside m_old;
                });

                if (m_box.try_get(req)) begin
                    int res [$] = m_old.find_first_index(i) with (i == idx);

                    req.m_idx  = idx;
                    m_req[idx] = req;

                    m_old.delete   (res.pop_front());
                    m_new.push_back(idx);
                end else
                    req = null;

            end else if (m_new.size()) begin
               `rands(idx, with {
                    idx inside m_new;
                });

                // current scheme cannot handle the situation where two b2b
                // reqs both hit vlb. the issue comes from the fact that reqs
                // are tracked by their idx. once the first req hits vlb, the
                // req is untracked anymore, but the second req has no chance
                // to know this
                // also avoid issuing the same req that is performing ptw
                if (m_vif.vlb_req_i_valid && (m_vif.vlb_req_i_bits_idx  == idx) ||
                    m_vif.vlb_busy_o      && (m_vif.vlb_fill_o_bits_idx == idx))
                    req = null;
                else
                    req = m_req[idx];
            end

            if (req) begin
                m_vif.vlb_req_i_valid     <= 1'b1;
                m_vif.vlb_req_i_bits_idx  <= req.m_idx;
                m_vif.vlb_req_i_bits_vpn  <= req.m_vpn;
            end else
                m_vif.vlb_req_i_valid     <= 1'b0;

            m_vif.vlb_req_i_bits_kill     <= m_kil[0];
            m_vif.vlb_kill_i[0]           <= m_kil[1];

            foreach (ret[i]) begin
                m_old.push_back(ret[i]);
                m_req[ret[i]] = null;
            end

            foreach (m_new[i]) begin
                req = m_req[m_new[i]];

                if (req == null)
                   `err($sformatf("idx not found: %x", m_new[i]));
                else if (++req.m_cnt >= TO_MAX)
                   `err($sformatf("req timeout: %s", req.show()));
            end

            if (!m_new.size() && (m_env.get() <= 1))
                m_env.add(1);

           `rands(sel, with { sel dist {
                1'b0 := m_dis_kil[0],
                1'b1 := m_dis_kil[1]
            };});

            m_kil = {m_kil[0], sel};

           `waitn(1);
        end
    endtask

endclass