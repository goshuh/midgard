`include "tb.svh"


class tb_env extends tb_base;

    tb_gen    m_gen;
    tb_seq    m_seq;
    tb_ctl    m_ctl;
    tb_llc    m_uvc_llc;
    tb_mem    m_uvc_mem;

    ma_mem    m_llc;
    pa_mem    m_mem;
    pe_mem    m_err;

    mailbox   m_box;
    semaphore m_sem;

    int       m_stg;

    function new(input tb_vif vif, ma_vif llc, pa_vif mem);
        verif::plusargs arg = new("tb.");

       `set(tb_env,  "env", this);

        m_llc     = new("LLC");
        m_mem     = new("MEM");
        m_err     = new("ERR");
        m_box     = new(0);
        m_sem     = new(1);

       `set(ma_mem,  "llc", m_llc);
       `set(pa_mem,  "mem", m_mem);
       `set(pe_mem,  "err", m_err);
       `set(mailbox, "box", m_box);

       `spawn(arg.get_str("gen", "tb_gen"), m_gen);
       `spawn(arg.get_str("seq", "tb_seq"), m_seq);

       `set(tb_gen,  "gen", m_gen);
       `set(tb_seq,  "seq", m_seq);

        m_ctl     = new(vif);

       `set(tb_ctl,  "ctl", m_ctl);

        m_uvc_llc = new(llc);
        m_uvc_mem = new(mem);

        m_stg     = 0;
    endfunction

    task blk(input int v);
        @(m_stg == v);
    endtask

    function int get();
        return m_stg;
    endfunction

    task set(input int v);
        m_sem.get();
        m_stg  = v;
        m_sem.put();
    endtask

    task add(input int v);
        m_sem.get();
        m_stg += v;
        m_sem.put();
    endtask

    task main();
        fork
            m_ctl    .main();
            m_seq    .main();
            m_uvc_llc.main();
            m_uvc_mem.main();
        join
    endtask

endclass