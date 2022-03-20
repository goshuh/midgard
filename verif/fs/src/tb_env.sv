`include "tb.svh"


class tb_env extends tb_base;

    tb_gen    m_gen;
    tb_seq    m_seq;
    tb_vlb    m_uvc_ilb;
    tb_vlb    m_uvc_dlb;
    tb_l1d    m_uvc_l1d;

    ma_mem    m_l1d;

    mailbox   m_box;
    semaphore m_sem;

    int       m_stg;

    function new(input tb_vif ilb, tb_vif dlb, ma_vif mem);
        verif::plusargs arg = new("tb.");

       `set(tb_env,  "env", this);

        m_l1d     = new("L1D");
        m_box     = new(0);
        m_sem     = new(1);

       `set(ma_mem,  "l1d", m_l1d);
       `set(mailbox, "box", m_box);

       `spawn(arg.get_str("gen", "tb_gen"), m_gen);
       `spawn(arg.get_str("seq", "tb_seq"), m_seq);

       `set(tb_gen,  "gen", m_gen);
       `set(tb_seq,  "seq", m_seq);

        m_uvc_ilb = new(ilb, "ilb");
        m_uvc_dlb = new(dlb, "dlb");
        m_uvc_l1d = new(mem);

        m_stg     = 0;;
    endfunction

    task blk(input int v);
        @(m_stg == v);
    endtask

    function int get();
        return m_stg;
    endfunction

    task add(input int v);
        m_sem.get();
        m_stg += v;
        m_sem.put();
    endtask

    task set(input int v);
        m_sem.get();
        m_stg  = v;
        m_sem.put();
    endtask

    task main();
        fork
            m_seq    .main();
            m_uvc_ilb.main();
            m_uvc_dlb.main();
            m_uvc_l1d.main();
        join
    endtask

endclass