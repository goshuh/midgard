`include "tb.svh"


class tb_mem extends tb_base;

    pa_vif    m_vif;

    pa_mem    m_mem;
    pe_mem    m_err;

    tb_req    m_req     [memWays-1:0];
    semaphore m_sem;

    int       m_dis_req [2];
    int       m_dis_acc [2];

    function new(ref pa_vif vif);
        verif::plusargs arg = new("mem.");

        m_mod = "mem";
        m_vif =  vif;
        m_sem =  new(1);

       `get(pa_mem, "mem", m_mem);
       `get(pe_mem, "err", m_err);

        m_dis_req[0] = {arg.get_int("req_min", 0), arg.get_int("req_max", 100)};
        m_dis_acc[0] = {arg.get_int("acc_min", 0), arg.get_int("acc_max", 100)};

        foreach (m_req[i])
            m_req[i] = new();
    endfunction

    task cha_req();
        forever begin
            tb_req req;
            int    dly = `urand(m_dis_req[0], m_dis_req[1]);

            m_vif.mem_req_o_ready     <= dly == 0;
           `waits(m_vif.mem_req_o_valid);

            if (dly) begin
               `waitn(dly - 1);
                m_vif.mem_req_o_ready <= 1'b1;
               `waitn(1);
            end
            m_vif.mem_req_o_ready     <= 1'b0;

            m_sem.get();
            req = m_req[m_vif.mem_req_o_bits_idx];

            if (req.m_vld)
               `err($sformatf("duplicated idx: %0x", m_vif.mem_req_o_bits_idx));

            req.m_vld  =  1'b1;
            req.m_dly  = `urand(m_dis_acc[0], m_dis_acc[1]);
            req.m_rnw  =  m_vif.mem_req_o_bits_rnw;
            req.m_pcn  =  m_vif.mem_req_o_bits_pcn;
            req.m_data =  m_vif.mem_req_o_bits_data;
            m_sem.put();
        end
    endtask

    task chd_resp();
        int cnt = 0;

        forever begin
           `waitn(1);

            m_sem.get();
            foreach (m_req[i]) begin
                tb_req req = m_req[i];

                if (req.m_vld && req.m_dly)
                    req.m_dly--;
            end
            m_sem.put();

            if (m_vif.mem_resp_i_valid)
                if (m_vif.mem_resp_i_ready) begin
                    m_vif.mem_resp_i_valid <= 1'b0;
                    cnt = 0;
                end else begin
                    if (++cnt >= TO_MAX)
                       `err("resp timeout");

                    continue;
                end

            m_sem.get();
            foreach (m_req[i]) begin
                tb_req req = m_req[i];

                if (req.m_vld && !req.m_dly) begin
                    pdn_t pdn = {req.m_pcn, 3'b0};

                    if (req.m_rnw)
                        req.m_data = m_mem.get_d(pdn);
                    else
                        m_mem.set_d(pdn, req.m_data);

                    req.m_vld = 1'b0;

                    m_vif.mem_resp_i_valid     <= 1'b1;
                    m_vif.mem_resp_i_bits_err  <= m_err.get_b(req.m_pcn);
                    m_vif.mem_resp_i_bits_idx  <= i[memIdx-1:0];
                    m_vif.mem_resp_i_bits_rnw  <= req.m_rnw;
                    m_vif.mem_resp_i_bits_data <= req.m_data;

                    break;
                end
            end
            m_sem.put();
        end
    endtask

    task main();
        fork
            cha_req ();
            chd_resp();
        join
    endtask

endclass