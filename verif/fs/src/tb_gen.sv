`include "tb.svh"


class tb_gen extends tb_base;

   `register(tb_gen);

    ma_mem        m_l1d;

    mpn_t         m_atp;
    mcn_t         m_cur;
    tb_vma        m_vma     [vpn_t];
    vpn_t         m_vpn     [$];

    int           m_dis_old [];

    static tb_gen m_inst;

    static function tb_gen get_inst();
        if (m_inst == null)
            m_inst =  new();

        return m_inst;
    endfunction

    function new();
        m_mod  = "gen";
        m_inst =  this;

       `get(ma_mem, "l1d", m_l1d);

        init();
        hook();
    endfunction

    virtual function void init();
        // TODO: spec only allows maximum 44 bits for satp
        m_atp = {{mpnBits-44{1'b0}}, 1'b1, {43{1'b0}}};

        m_vma.delete();
        m_vpn.delete();
        m_l1d.clr();
    endfunction

    virtual function void hook();
        m_dis_old = '{1, 100};
    endfunction

    virtual function vpn_t gen_vpn();
        vpn_t vpn;
        int   old;

       `rands(old, with { old dist {
            0 := m_dis_old[0],
            1 := m_dis_old[1]
        };});

        if (old) begin
            if (m_vpn.size() == 0)
                m_vpn = m_vma.find_index() with (1'b1);

           `rands(vpn, with {
                vpn inside m_vpn;
            });
        end else
           `rands(vpn);

        return vpn;
    endfunction

    virtual function tb_vma gen_vma();
        tb_vma vma;

        do begin
            vpn_t min = {vpnBits{1'b0}};
            vpn_t max = {vpnBits{1'b1}};
            vpn_t prv;

           `rands(vma);

            prv = vma.base;
            min = vma.base;

            if (m_vma.prev(prv))
                if (m_vma[prv].bound + 1 > min)
                    continue;

            if (m_vma.next(prv))
                max = m_vma[prv].base  - 1;
            if (max < min)
                continue;

            // don't be super big
            if (max - min > {9'b0, {vpnBits-9{1'b1}}})
                max = min + {9'b0, {vpnBits-9{1'b1}}};

           `rands(prv, with {
                prv inside {[min: max]};
            });

            vma.bound = prv;
            break;
        end while (1);

        vma.vld = 1'b1;
        vma.err = 1'b0;

        m_vma[vma.base] = vma;

        return vma;
    endfunction

    function mcn_t gen_tab(input tb_tree n);
        tb_tab tab = 1024'b0;
        mcn_t  mcn = m_cur;

       `dbg($sformatf("[%x] <- %s", mcn, n.show()));

        // to the next double-cache line
        m_cur += {{mcnBits-2{1'b0}}, 2'h2};

        for (int i = 0; i <  n.m_num; i++)
            tab.vma[i] = n.m_vma[i];

        if (n.m_bot)
            tab.bot = 1'b1;
        else
            for (int i = 0; i <= n.m_num; i++)
                tab.ptr[i] = gen_tab(n.m_ptr[i]);

        // vlb entries are nearly ordered in memory
        m_l1d.set_h(mcn, tab);

        return mcn;
    endfunction

    function void gen_all(input int num);
        // build a b-tree
        tb_tree top = new();

        for (int i = 0; i < num; i++)
            top = top.insert(gen_vma());

        // write b-tree to the memory
        m_cur = {m_atp, 6'b0};

        gen_tab(top);
    endfunction

    virtual function tb_vma walk(input vpn_t vpn);
        tb_vma vma;
        vpn_t  prv = vpn;

        // exact hit (prev doesn't cover)
        if (m_vma.exists(vpn))
            return m_vma[vpn];

        // range hit
        vma.vld = m_vma.prev(prv);

        if (vma.vld) begin
            vma     = m_vma[prv];
            vma.vld = vpn <= vma.bound;
        end

        return vma;
    endfunction

    function tb_vma main(ref vpn_t vpn);
        vpn = gen_vpn();

        return walk(vpn);
    endfunction

endclass