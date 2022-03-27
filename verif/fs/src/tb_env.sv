`include "tb.svh"


class tb_env extends tb_base;

    tb_gen    m_gen;
    tb_seq    m_seq;
    tb_vlb    m_uvc_ilb;
    tb_vlb    m_uvc_jlb;
    tb_vlb    m_uvc_dlb;
    tb_l1d    m_uvc_l1d;

    ma_mem    m_l1d;

    mailbox   m_box;
    semaphore m_sem;

    int       m_stg;

    function new(input tb_vif ilb, tb_vif jlb, tb_vif dlb, ma_vif mem);
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

        m_uvc_ilb = new(ilb, "ilb", 0,           vlbWays / 2);
        m_uvc_jlb = new(jlb, "jlb", vlbWays / 2, vlbWays);
        m_uvc_dlb = new(dlb, "dlb", 0,           vlbWays);
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
            // 0 -> 3: ilb/jlb/dlb
            // 3 -> 4: seq
            // 4 -> 7: ilb/jlb/dlb
            // 7 -> 0: seq
            m_seq    .main(3, 3);
            m_uvc_ilb.main(4, 3);
            m_uvc_jlb.main(5, 3);
            m_uvc_dlb.main(6, 3);
            m_uvc_l1d.main();
        join
    endtask

endclass