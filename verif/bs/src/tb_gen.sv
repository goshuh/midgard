`include "tb.svh"


class tb_gen extends tb_base;

   `register(tb_gen);

    ma_mem     m_llc;
    pa_mem     m_mem;
    pe_mem     m_err;

    bit [63:0] m_ctl     [ptwLvl:0];
    bit [ 8:0] m_idx     [ptwLvl:1][$];
    int        m_ppn     [ppn_t];
    int        m_shf     [ptwLvl:1];
    mdn_t      m_mdm     [ptwLvl:1];
    pdn_t      m_pdm     [ptwLvl:1];

    int        m_dis_old [ptwLvl:1][];
    int        m_dis_err [ptwLvl:1][];
    int        m_dis_blk [ptwLvl:1][];

    static tb_gen m_inst;

    static function tb_gen get_inst();
        if (m_inst == null)
            m_inst =  new();

        return m_inst;
    endfunction

    function new();
        m_mod  = "gen";
        m_inst =  this;

       `get(ma_mem, "llc", m_llc);
       `get(pa_mem, "mem", m_mem);
       `get(pe_mem, "err", m_err);

        init();
        hook();
    endfunction

    virtual function void init();
        // level-i ptes are located at the highest portion of the ma space
        // occupied by level-i+1 ptes. as root ptes, level-i ptes map
        // level-i+1 page table pages
        bit [mpnBits+66:0] all = {{64{1'b1}}, {mpnBits+3{1'b0}}};
        ppn_t              top = {    1'b1,   {ppnBits-1{1'b0}}};

        for (int i = 1; i <= ptwLvl; i++)
            m_idx[i] = {};
        m_ppn.delete();

        m_llc.clr();
        m_mem.clr();
        m_err.clr();

        // pa root
        m_ctl[  0] = {top, 12'b0} | {{paBits-1{1'b0}}, 1'b1};
        m_ppn[top] =  1;

        // recursive ptw
        // the first pte accessed when translating a reserved pte ma just
        // jumps to itself. this makes accessing all the ptes relevant to
        // a specific ma possible with creating recursively creating new
        // ptes for these ptes
        if (ptwTop < 9)
            m_ctl[0][11-:9-ptwTop] = {9-ptwTop{1'b1}};

        m_mem.set_b({top, {9{1'b1}}}, {{55-ppnBits{1'b0}}, top, 9'b0, 1'b1});

        // ma bases
        for (int i = 1; i <= ptwLvl; i++) begin
            bit [mpnBits+66:0] shf = all >> ((ptwLvl - i) * 9);

            m_ctl[i] = (maBits > 63) ? shf[63:0] : {{64-maBits{shf[maBits-1]}}, shf[maBits-1:0]};
            m_shf[i] = (ptwLvl -  i) * 9;

            m_mdm[i] = ~m_ctl[i][maBits-1:3];
            m_pdm[i] = (i == 1) ? {{pdnBits-ptwTop{1'b0}}, {ptwTop{1'b1}}} :
                                  {{pdnBits-9     {1'b0}}, {     9{1'b1}}};
        end

       `info($sformatf("lvl: %0d", ptwLvl));

        for (int i = 0; i <= ptwLvl; i++)
           `info($sformatf("ctl: %x", m_ctl[i]));
    endfunction

    virtual function void hook();
        for (int i = 1; i <= ptwLvl; i++) begin
            m_dis_old[i] = '{10, 40};
            m_dis_err[i] = '{10,  1,  1};
            m_dis_blk[i] = '{10, (i > 3) ? 1 : 0, 1};
        end
    endfunction

    virtual function bit [8:0] gen_idx(const ref bit [8:0] arr[$]);
        bit [8:0] idx;

        // solely because m_idx[i] is not supported by the tool
       `rands(idx, with {
           idx inside arr;
        });

        return idx;
    endfunction

    virtual function mpn_t gen_mpn();
        bit [mpnBits+8:0] mpn;
        bit [        8:0] idx;

        for (int i = 1; i <= ptwLvl; i++) begin
            int old;

           `rands(old, with { old dist {
                0 := m_dis_old[i][0],
                1 := m_dis_old[i][1]
            };});

            if (old && !m_idx[i].size() || !old) begin
               `rands(idx);

                // can have duplications
                m_idx[i].push_back(idx);
            end else
                idx = gen_idx(m_idx[i]);

            mpn[i*9-1-:9] = idx;
        end

        // non-top of ma space
        return mpn[mpnBits-1:0] & {1'b0, {mpnBits-1{1'b1}}};
    endfunction

    virtual function ppn_t gen_ppn(input bit ptp, int lvl);
        // num of 4k pages to allocate
        longint num = ptp ? 1 << ((ptwLvl - lvl) * 9) : 1;
        longint msk = ~(num - 1);
        ppn_t   ppn;
        ppn_t   prv;

        do begin
           `rands(ppn);

            // page table pages are placed at the top the pa space, while the
            // first page aligns to the given boundary
            ppn = {ptp, ppn[ppnBits-2:0]} & msk[ppnBits-1:0];
            prv =  ppn;

        // no single page is allocated twice
        end while (m_ppn.prev(prv) && (prv + m_ppn[prv] >= ppn) ||
                   m_ppn.next(prv) && (ppn + num        >= prv));

        m_ppn[ppn] = num;

        return ppn;
    endfunction

    virtual function tb_gen_res walk(input mpn_t mpn);
        mdn_t mdn;
        ppn_t ppn = m_ctl[0][paBits-1:12];
        pdn_t pdn = m_ctl[0][paBits-1: 3];

       `dbg($sformatf("mpn: %x", mpn));

        // top-down ptw: creating missed ptes in memory
        for (int i = 1; i <= ptwLvl; i++) begin
            bit [64:0] pte;
            bit [ 1:0] err;
            bit [ 1:0] blk;
            int        j = ptwLvl - i + 1;

            // no loss of bits
            mdn_t msh =  {9'b0, mpn} >> m_shf[j];
            mpn_t psh =   mpn        >> m_shf[i];
            pdn_t top = {{pdnBits-9{1'b0}}, psh[8:0]};

            mdn = m_ctl[j][maBits-1:3] & ~m_mdm[j] | // base
                  msh                  &  m_mdm[j];  // idx

            pdn = pdn & ~m_pdm[i] | // base
                  top &  m_pdm[i];  // idx

           `rands(err, with { err dist {
                2'h0 := m_dis_err[i][0], // normal
                2'h1 := m_dis_err[i][1], // bus err
                2'h2 := m_dis_err[i][2]  // invalid
            };});
           `rands(blk, with { blk dist {
                2'h0 := m_dis_blk[i][0], // normal
                2'h1 := m_dis_blk[i][1], // huge page
                2'h2 := m_dis_blk[i][2]  // invalid leaf
            };});

            // re-interpret based on the current level
            blk[0] = blk[0] & (i <  ptwLvl) |
                    ~blk[1] & (i == ptwLvl);

            if (m_err.chk(pdn[pdnBits-1:3])) begin
                // the page is already marked as invalid

                if (m_mem.chk(pdn)) begin
                    pte  = m_mem.get_b(pdn);
                    ppn  = pte[paBits-3:10];

                    err[0] =  1'b1;
                    err[1] = ~pte[0];
                    blk[0] =  pte[1];

                end else begin
                    // just fill an invalid pte
                    ppn = {ppnBits{1'b0}};
                    pte = {1'b1,  63'b0};

                    m_mem.set_b(pdn, pte);
                end

            end else begin
                if (m_mem.chk(pdn)) begin
                    pte =  m_mem.get_b(pdn);
                    ppn =  pte[paBits-3:10];

                    // coarse-grain. depending on the width of the mem bus
                    err[0] =  m_err.get_b(pdn[pdnBits-1:3]);
                    err[1] = ~pte[ 0];
                    blk[0] =  pte[ 1];

                end else begin
                    // allocate a free new (huge) page
                    ppn =  gen_ppn(blk[0], i);

                    pte = {err[0],  // err
                          {54-ppnBits{1'b0}},
                           ppn,     // ppn
                           6'b0,    // TODO
                           blk[0],  // x
                           blk[0],  // w
                           blk[0],  // r
                          ~err[1]}; // vld

                    m_mem.set_b(pdn, pte);

                    if (err[0])
                        m_err.set_b(pdn[pdnBits-1:3], 1'b1);
                end
            end

           `dbg($sformatf("  %x/%x: %x %x",
                           mdn[mdnBits-1:3],
                           pdn[pdnBits-1:3],
                           pte,
                           err));

            if (blk[0]) begin
                longint msk = (1 << m_shf[i]) - 1;

                ppn = ppn              & ~msk[ppnBits-1:0] |
                      mpn[ppnBits-1:0] &  msk[ppnBits-1:0];
            end

            pdn = {ppn, 9'b0};

            // pte err
            if (err[0] | err[1])
                return '{err: 1'b1,
                         mpn: mpn,
                         ppn: ppn};

            // normal (huge) page
            if (pte[0] & blk[0])
                return '{err: i <= ptwEnd,
                         mpn: mpn,
                         ppn: ppn};
        end

        // invalid leaf or valid pte
        return '{err: mpn[mpnBits-1-:ptwTop] != {ptwTop{1'b1}},
                 mpn: mpn,
                 ppn: ppn};
    endfunction

    function tb_gen_res main();
        return walk(gen_mpn());
    endfunction

endclass