class tb_tree extends tb_base;

    int     m_num;
    int     m_bot;
    int     m_idx;

    tb_vma  m_vma [0:btOrder-1];
    tb_tree m_ptr [0:btOrder  ];

    static int m_seq = 0;

    function new();
        m_num  = 0;
        m_bot  = 1;
        m_idx  = m_seq;

        foreach (m_ptr[i])
            m_ptr[i] = null;

        m_seq++;
    endfunction

    function tb_tree insert(input tb_vma v);
        if (m_num == btOrder) begin
            tb_tree n = new();

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

    function void split(input int p, tb_tree c);
        tb_tree n = new();

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
        string ret = $sformatf("%0d:", m_idx);

        for (int i = 0; i <= m_num; i++) begin
            if (m_ptr[i])
                ret = {ret, $sformatf("\n  p: %0d",
                                       m_ptr[i].m_idx)};
            if (i < m_num)
                ret = {ret, $sformatf("\n  v: %x-%x/%x %x %x",
                                       m_vma[i].base,
                                       m_vma[i].bound,
                                       m_vma[i].offs,
                                       m_vma[i].err,
                                       m_vma[i].attr)};
        end

        return ret;
    endfunction

endclass