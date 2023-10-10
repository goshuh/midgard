`include "tb.svh"


class tb_l1d extends tb_base;

    ma_vif    m_vif;

    ma_mem    m_l1d;

    tb_req    m_req [1:0];
    semaphore m_sem;

    int       m_dis_req [2];
    int       m_dis_acc [2];

    function new(ref ma_vif vif);
        verif::plusargs arg = new("l1d.");

        m_mod = "l1d";
        m_vif =  vif;
        m_sem =  new(1);

       `get(ma_mem, "l1d", m_l1d);

        m_dis_req = {arg.get_int("req_min", 0), arg.get_int("req_max", 100)};
        m_dis_acc = {arg.get_int("acc_min", 0), arg.get_int("acc_max", 100)};

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

            if (req.m_res.vld)
               `err($sformatf("duplicated idx: %0x", m_vif.mem_req_o_bits_idx));

            req.m_res.vld =  1'b1;
            req.m_cnt     = `urand(m_dis_acc[0], m_dis_acc[1]);
            req.m_mcn     =  m_vif.mem_req_o_bits_mcn;
            m_sem.put();
        end
    endtask

    task chd_res();
        int cnt = 0;

        forever begin
           `waitn(1);

            m_sem.get();
            foreach (m_req[i]) begin
                tb_req req = m_req[i];

                if (req.m_res.vld && req.m_cnt)
                    req.m_cnt--;
            end
            m_sem.put();

            if (m_vif.mem_res_i_valid)
                if (m_vif.mem_res_i_ready) begin
                    m_vif.mem_res_i_valid <= 1'b0;
                    cnt = 0;
                end else begin
                    if (++cnt >= TO_MAX)
                       `err("res timeout");

                    continue;
                end

            m_sem.get();
            foreach (m_req[i]) begin
                tb_req req = m_req[i];

                if (req.m_res.vld && !req.m_cnt) begin
                    req.m_res.vld = 1'b0;

                    m_vif.mem_res_i_valid     <= 1'b1;
                    m_vif.mem_res_i_bits_idx  <= i[0];
                    m_vif.mem_res_i_bits_data <= m_l1d.get_b(req.m_mcn);

                    break;
                end
            end
            m_sem.put();
        end
    endtask

    task main();
        fork
            cha_req();
            chd_res();
        join
    endtask

endclass
