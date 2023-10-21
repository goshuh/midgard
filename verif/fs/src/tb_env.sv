`include "tb.svh"


class tb_env extends tb_base;

    tb_vsc    m_gen;
    tb_seq    m_seq;
    tb_ctl    m_ctl;
    tb_vlb    m_uvc_ilb;
    tb_vlb    m_uvc_jlb;
    tb_vlb    m_uvc_dlb;
    tb_vlb    m_uvc_elb;
    tb_l1d    m_uvc_l1d;

    ma_mem    m_l1d;

    mailbox   m_box;
    semaphore m_sem;

    int       m_stg;

    function new(input tb_vif vif, va_vif ilb, va_vif jlb, va_vif dlb, va_vif elb, ma_vif mem);
        verif::plusargs arg = new("tb.");

       `set(tb_env,  "env", this);

        m_l1d     = new("L1D");
        m_box     = new(0);
        m_sem     = new(1);

       `set(ma_mem,  "l1d", m_l1d);
       `set(mailbox, "box", m_box);

       `spawn(arg.get_str("gen", "tb_vsc"), m_gen);
       `spawn(arg.get_str("seq", "tb_seq"), m_seq);

       `set(tb_vsc,  "gen", m_gen);
       `set(tb_seq,  "seq", m_seq);

        m_ctl     = new(vif);

       `set(tb_ctl,  "ctl", m_ctl);

        // see: tb.sv
        m_uvc_ilb = new(ilb, "ilb", 0,           vlbWays / 2);
        m_uvc_jlb = new(jlb, "jlb", vlbWays / 2, vlbWays);
        m_uvc_dlb = new(dlb, "dlb", 0,           vlbWays / 2);
        m_uvc_elb = new(elb, "elb", vlbWays / 2, vlbWays);
        m_uvc_l1d = new(mem);

        m_stg     = 0;
    endfunction

    task blk(input int v);
        wait(m_stg == v);
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
            //  0 ->  4: ilb/jlb/dlb/elb
            //  4 ->  5: ctl
            //  5 ->  6: seq
            //  6 -> 10: ilb/jlb/dlb/elb
            // 10 ->  0: ctl
            m_ctl    .main(4, 5);
            m_uvc_ilb.main(6, 4);
            m_uvc_jlb.main(7, 4);
            m_uvc_dlb.main(8, 4);
            m_uvc_elb.main(9, 4);
            m_seq    .main(5);
            m_uvc_l1d.main();
        join
    endtask

endclass
