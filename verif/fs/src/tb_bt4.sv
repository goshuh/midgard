`include "tb.svh"


typedef struct packed {
    bit [     7:0] attr;
    bit [     3:0] res;
    tb_base::vpn_t offs;
    tb_base::vpn_t bound;
    tb_base::vpn_t base;
} bt4_pvma;


typedef struct packed {
    tb_base::ptr_t [tb_base::btOrder  :0] ptr;
    bt4_pvma       [tb_base::btOrder-1:0] vma;
    bit            [                15:0] sdid;
    bit            [                11:0] res;
    bit            [                 2:0] num;
    bit                                   bot;
} bt4_node;


class bt4_abs extends tb_base;

    int     m_num;
    int     m_bot;
    int     m_idx;

    tb_vma  m_vma [0:btOrder-1];
    bt4_abs m_ptr [0:btOrder  ];

    static int m_seq = 0;

    function new();
        m_num  = 0;
        m_bot  = 1;
        m_idx  = m_seq++;

        foreach (m_ptr[i])
            m_ptr[i] = null;
    endfunction

    function bt4_abs insert(input tb_vma v);
        if (m_num == btOrder) begin
            bt4_abs n = new();

            n.m_bot    = 0;
            n.m_ptr[0] = this;

            n.split (0, this);
            n.insert(v);

            return n;

        end else begin
            int p = m_num;

            if (m_bot) begin
                while ((p > 0) && (v.base < m_vma[p - 1].base)) begin
                    m_vma[p] = m_vma[p - 1];
                    p--;
                end

                m_vma[p] = v;
                m_num++;

            end else begin
                while ((p > 0) && (v.base < m_vma[p - 1].base))
                    p--;

                if (m_ptr[p].m_num == btOrder) begin
                    split(p, m_ptr[p]);

                    if (v.base > m_vma[p].base)
                        p++;
                end

                m_ptr[p].insert(v);
            end

            return this;
        end
    endfunction

    function void split(input int p, bt4_abs c);
        bt4_abs n = new();

        int l = btOrder / 2;
        int m = l       + 1;
        int h = btOrder - m;

        c.m_num = l;
        n.m_num = h;
        n.m_bot = c.m_bot;

        for (int i = m; i <  btOrder; i++)
            n.m_vma[i - m] = c.m_vma[i];
        for (int i = m; i <= btOrder; i++)
            n.m_ptr[i - m] = c.m_ptr[i];

        for (int i = m_num - 1; i >= p; i--)
            m_vma[i + 1] = m_vma[i];
        for (int i = m_num;     i >  p; i--)
            m_ptr[i + 1] = m_ptr[i];

        m_vma[p    ] = c.m_vma[l];
        m_ptr[p + 1] = n;

        m_num++;
    endfunction

    function string show();
        string ret = $sformatf("vma %0d", m_idx);

        for (int i = 0; i <= m_num; i++) begin
            if (m_ptr[i])
                ret = {ret, $sformatf("\n  p: %0d",
                                       m_ptr[i].m_idx)};
            if (i < m_num)
                ret = {ret, $sformatf("\n  v: %x-%x/%x %x",
                                       m_vma[i].base,
                                       m_vma[i].bound,
                                       m_vma[i].offs,
                                       m_vma[i].attr)};
        end

        return ret;
    endfunction

endclass


class tb_bt4 extends tb_base;

   `register(tb_bt4);

    ma_mem     m_l1d;

    mpn_t      m_atp;
    mcn_t      m_cur;
    tb_vma     m_vma     [vpn_t];
    vpn_t      m_vpn     [$];

    bit [ 5:0] m_cfg_idx;
    bit [ 5:0] m_cfg_vsc;
    bit [ 5:0] m_cfg_top;
    bit [ 5:0] m_cfg_tsl;
    bit [31:0] m_cfg_mmask;
    bit [31:0] m_cfg_imask;
    bit [ 4:0] m_cfg_vmask;
    bit [19:0] m_cfg_tmask;

    int        m_dis_old [];

    static tb_bt4 m_inst;

    static function tb_bt4 get_inst();
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
        m_cfg_idx   =  6'h0;
        m_cfg_vsc   =  6'h0;
        m_cfg_top   =  6'h0;
        m_cfg_tsl   =  6'h0;
        m_cfg_mmask = 32'h0;
        m_cfg_imask = 32'h0;
        m_cfg_vmask =  5'h0;
        m_cfg_tmask = 20'h0;

        m_dis_old   = '{1, 100};
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

    function mcn_t gen_tab(input bt4_abs n);
        bt4_node tab = 1024'b0;
        mcn_t    mcn = m_cur;

       `dbg($sformatf("[%x] <- %s", mcn, n.show()));

        // to the next double-cache line
        m_cur += {{mcnBits-2{1'b0}}, 2'h2};

        for (int i = 0; i < n.m_num; i++) begin
            tab.vma[i].attr  = n.m_vma[i].attr;
            tab.vma[i].base  = n.m_vma[i].base;
            tab.vma[i].bound = n.m_vma[i].bound;
            tab.vma[i].offs  = n.m_vma[i].offs;
        end

        tab.num = n.m_num;

        if (n.m_bot)
            tab.bot = 1'b1;
        else
            for (int i = 0; i <= n.m_num; i++)
                tab.ptr[i] = {gen_tab(n.m_ptr[i]), 6'b0};

        // vlb entries are nearly ordered in memory
        m_l1d.set_h(mcn, tab);

        return mcn;
    endfunction

    function void gen_all(input int num);
        // build a b-tree
        bt4_abs top = new();

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