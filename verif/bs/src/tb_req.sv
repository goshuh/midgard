`include "tb.svh"


class tb_req extends tb_base;

   `register(tb_req);

    tb_gen      m_gen;

    bit         m_vld;
    bit         m_pte;
    bit         m_err;
    bit         m_wnr;
    llc_t       m_idx;
    bit [  2:0] m_siz;
    mcn_t       m_mcn;
    pcn_t       m_pcn;
    bit [511:0] m_data;

    int         m_cnt;
    int         m_dly;

    ma_mem      m_llc;
    pa_mem      m_mem;
    pe_mem      m_pem;

    function new();
        verif::plusargs arg = new("req.");

        int min = arg.get_int("min", 0);
        int max = arg.get_int("max", 10);

        m_mod = "req";
        m_gen =  tb_gen::get_inst();

        m_vld =  1'b0;
        m_pte =  1'b0;
        m_cnt =  0;

       `rands(m_dly, with {
            m_dly inside {[min: max]};
        });

       `get(ma_mem, "llc", m_llc);
       `get(pa_mem, "mem", m_mem);
       `get(pe_mem, "err", m_pem);
    endfunction

    virtual function void init(input bit i = 1'b0);
        tb_gen_res res;
        bit [11:6] pof;

       `rands(pof);
       `rands(m_wnr);
       `rands(m_data);

        res = i ? m_gen.main() :
                  m_gen.walk(m_mcn[mcnBits-1:6]);

        m_mcn = {res.mpn, pof};
        m_pcn = {res.ppn, pof};
        m_err =  res.err | m_pem.get_b(m_pcn);

        // don't try to overwrite the reserved pte region
        if (m_pte)
            m_wnr = 1'b0;

       `dbg($sformatf("res: %x/%x: %x",
                       m_mcn,
                       m_pcn,
                       m_err));
    endfunction

    virtual function void body();
        m_vld = 1'b1;

        if (m_wnr == 1'b0) begin
            pdn_t pdn = {m_pcn, 3'b0};

            // fortunately, we don't have any data cache
            if (m_pte)
                m_data = m_mem.get_d(pdn);
            else
                m_mem.set_d(pdn, m_data);
        end
    endfunction

    virtual function void post();
        m_vld = 1'b0;
        m_cnt = 0;

        if (m_pte && !m_err)
            m_llc.set_d({m_mcn, 3'b0}, m_data);
    endfunction

    virtual function string show();
        return $sformatf("req(vld: %x, err: %x, wnr: %x, idx: %x, mcn: %x %x, pcn: %x %x, data: %x)",
                          m_vld,
                          m_err,
                          m_wnr,
                          m_idx,
                          m_mcn, m_mcn[mcnBits-1:6],
                          m_pcn, m_pcn[pcnBits-1:6],
                          m_data);
    endfunction

endclass