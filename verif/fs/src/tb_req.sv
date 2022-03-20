`include "tb.svh"


class tb_req extends tb_base;

   `register(tb_req);

    tb_gen m_gen;

    tb_vma m_res;
    vpn_t  m_vpn;
    mpn_t  m_mpn;

    vlb_t  m_idx;
    int    m_cnt;

    function new();
        m_mod = "req";

        m_gen =  tb_gen::get_inst();
        m_cnt =  0;
    endfunction

    virtual function void init(input bit i = 1'b0);
        m_res = i ? m_gen.main(m_vpn) :
                    m_gen.walk(m_vpn);

        m_mpn = m_vpn[mpnBits-1:0] + m_res.offs[mpnBits-1:0];

       `dbg($sformatf("res: %x %x %x %x",
                       m_vpn,
                       m_mpn,
                       m_res.vld,
                       m_res.err));
    endfunction

    virtual function void body();
    endfunction

    virtual function void post();
    endfunction

    virtual function string show();
        return $sformatf("req(vpn: %x, mpn: %x, idx: %x, vld: %x, err: %x)",
                          m_vpn,
                          m_mpn,
                          m_idx,
                          m_res.vld,
                          m_res.err);
    endfunction

endclass