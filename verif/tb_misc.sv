`include "verif.svh"


//
// base class

class tb_base extends verif::object;

    localparam maBits  =  64;
    localparam paBits  =  48;

    localparam llcWays =  8;
    localparam memWays =  4;

    // derived
    localparam mpnBits =  maBits - 12;
    localparam mcnBits =  maBits - 6;
    localparam mdnBits =  maBits - 3;

    localparam ppnBits =  paBits - 12;
    localparam pcnBits =  paBits - 6;
    localparam pdnBits =  paBits - 3;

    localparam ptwLvl  = (mpnBits + (9      - 1)) / 9;
    localparam ptwTop  =  mpnBits - (ptwLvl - 1)  * 9;
    localparam ptwEnd  = (maBits  - ptwTop) <= paBits ? 0 : (maBits - ptwTop - paBits) / 9 + 1;

    localparam llcIdx  = $clog2(llcWays);
    localparam memIdx  = $clog2(memWays);

    typedef bit [mpnBits-1:0] mpn_t;
    typedef bit [mcnBits-1:0] mcn_t;
    typedef bit [mdnBits-1:0] mdn_t;

    typedef bit [ppnBits-1:0] ppn_t;
    typedef bit [pcnBits-1:0] pcn_t;
    typedef bit [pdnBits-1:0] pdn_t;

    typedef bit [llcIdx -1:0] llc_t;
    typedef bit [memIdx -1:0] mem_t;

    // tb usage
    static  int TO_MAX = 5000;

    // common functions
    function bit [maBits-4:0] add_mcd(input mcn_t m);
        return {m, 3'b0};
    endfunction

    function bit [paBits-4:0] add_pcd(input pcn_t p);
        return {p, 3'b0};
    endfunction

    function bit [519:0] set_err(input bit [511:0] d, bit e);
        return {e, d[7*64+:64],
                e, d[6*64+:64],
                e, d[5*64+:64],
                e, d[4*64+:64],
                e, d[3*64+:64],
                e, d[2*64+:64],
                e, d[1*64+:64],
                e, d[0*64+:64]};
    endfunction

    function bit [511:0] clr_err(input bit [519:0] d);
        return {d[7*65+:64],
                d[6*65+:64],
                d[5*65+:64],
                d[4*65+:64],
                d[3*65+:64],
                d[2*65+:64],
                d[1*65+:64],
                d[0*65+:64]};
    endfunction

    function bit get_err(input bit [519:0] d);
        return  d[8*65-1] |
                d[7*65-1] |
                d[6*65-1] |
                d[5*65-1] |
                d[4*65-1] |
                d[3*65-1] |
                d[2*65-1] |
                d[1*65-1];
    endfunction

endclass


//
// interface

`define clk tb.m_vif.clock
`define rst tb.m_vif.reset

interface tb_ctl_intf (
    input wire clock,
    input wire reset
);

    bit            ctl_req_i_ready;
    bit            ctl_req_i_valid;
    bit            ctl_req_i_bits_rnw;
    bit [     3:0] ctl_req_i_bits_addr;
    bit [    63:0] ctl_req_i_bits_data;
    bit            ctl_resp_o_ready;
    bit            ctl_resp_o_valid;
    bit            ctl_resp_o_bits_sel;
    bit            ctl_resp_o_bits_rnw;
    bit [    63:0] ctl_resp_o_bits_data;

    initial begin
        ctl_req_i_valid  <= 1'b0;
        ctl_resp_o_ready <= 1'b0;
    end

endinterface


interface tb_llc_intf (
    input wire clock,
    input wire reset
);

    bit            llc_req_i_ready;
    bit            llc_req_i_valid;
    tb_base::llc_t llc_req_i_bits_idx;
    bit            llc_req_i_bits_rnw;
    tb_base::mcn_t llc_req_i_bits_mcn;
    tb_base::pcn_t llc_req_i_bits_pcn;
    bit [   511:0] llc_req_i_bits_data;
    bit            llc_resp_o_ready;
    bit            llc_resp_o_valid;
    tb_base::llc_t llc_resp_o_bits_idx;
    bit            llc_resp_o_bits_err;
    bit            llc_resp_o_bits_rnw;
    bit [   511:0] llc_resp_o_bits_data;

    bit            llc_req_o_ready;
    bit            llc_req_o_valid;
    tb_base::mcn_t llc_req_o_bits_mcn;
    bit            llc_resp_i_ready;
    bit            llc_resp_i_valid;
    bit            llc_resp_i_bits_hit;
    bit [   511:0] llc_resp_i_bits_data;

    initial begin
        llc_req_i_valid  <= 1'b0;
        llc_resp_o_ready <= 1'b0;

        llc_req_o_ready  <= 1'b0;
        llc_resp_i_valid <= 1'b0;
    end

endinterface


interface tb_mem_intf (
    input wire clock,
    input wire reset
);

    bit            mem_req_o_ready;
    bit            mem_req_o_valid;
    tb_base::mem_t mem_req_o_bits_idx;
    bit            mem_req_o_bits_rnw;
    tb_base::mcn_t mem_req_o_bits_mcn;
    tb_base::pcn_t mem_req_o_bits_pcn;
    bit [   511:0] mem_req_o_bits_data;
    bit            mem_resp_i_ready;
    bit            mem_resp_i_valid;
    tb_base::mem_t mem_resp_i_bits_idx;
    bit            mem_resp_i_bits_err;
    bit            mem_resp_i_bits_rnw;
    bit [   511:0] mem_resp_i_bits_data;

    initial begin
        mem_req_o_ready  <= 1'b0;
        mem_resp_i_valid <= 1'b0;
    end

endinterface


typedef virtual tb_ctl_intf tb_vif;
typedef virtual tb_llc_intf ma_vif;
typedef virtual tb_mem_intf pa_vif;


//
// components

typedef verif::memory #(tb_base::mdnBits, 64) ma_mem;
typedef verif::memory #(tb_base::pdnBits, 65) pa_mem;


typedef struct {
    bit            err;
    tb_base::mpn_t mpn;
    tb_base::ppn_t ppn;
} tb_gen_res;


class tb_gen extends tb_base;

   `register(tb_gen);

    pa_mem     m_mem;

    bit [63:0] m_ctl     [ptwLvl:0];
    bit [ 8:0] m_idx     [ptwLvl:1][$];
    int        m_map     [ppn_t];
    int        m_shf     [ptwLvl:1];
    ppn_t      m_msk     [ptwLvl:1];

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
        verif::plusargs arg = new("gen.");

        m_mod  = "gen";
        m_inst =  this;

       `get(pa_mem, "mem", m_mem);

        init();
        hook();
    endfunction

    virtual function void init();
        // level-i ptes are located at the highest portion of the ma space
        // occupied by level-i+1 ptes. as root ptes, level-i ptes map
        // level-i+1 page table pages
        bit [mpnBits+66:0] all = {{64{1'b1}}, {mpnBits+3{1'b0}}};
        ppn_t              top = {    1'b1,   {ppnBits-1{1'b0}}};

        // pa root
        m_ctl[  0] = {top, 12'b0} | {{paBits-1{1'b0}}, 1'b1};
        m_map[top] =  1;

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
            m_msk[i] = (i == 1) ? {{ppnBits-ptwTop{1'b0}}, {ptwTop{1'b1}}} :
                                  {{ppnBits-9     {1'b0}}, {     9{1'b1}}};
        end

       `info($sformatf("lvl: %0d", ptwLvl));

        for (int i = 0; i <= ptwLvl; i++)
           `info($sformatf("ctl: %0x", m_ctl[i]));
    endfunction

    virtual function void hook();
        for (int i = 1; i <= ptwLvl; i++) begin
            m_dis_old[i] = new [2];
            m_dis_err[i] = new [3];
            m_dis_blk[i] = new [3];

            m_dis_old[i] = '{10, 40};
            m_dis_err[i] = '{10,  0, 0};
            m_dis_blk[i] = '{10,  0, 0};
        end
    endfunction

    function bit [8:0] gen_idx(const ref bit [8:0] arr[$]);
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
        int     yes = 0;
        longint num = ptp ? 1 << ((ptwLvl - lvl) * 9) : 1;
        longint msk = ~(num - 1);
        ppn_t   ppn;

        do begin
            yes = 0;

           `rands(ppn);

            // page table pages are placed at the top the pa space, and the
            // first page aligns to the given boundary
            ppn = {ptp, ppn[ppnBits-2:0]} & msk[ppnBits-1:0];

            // no single page is allocated twice
            for (longint i = 0; i < num; i++)
                if (m_map.exists(ppn + i[ppnBits-1:0])) begin
                    yes = 1;
                    break;
                end
        end while (yes);

        for (longint i = 0; i < num; i++)
            m_map[ppn + i[ppnBits-1:0]] = 1;

        return ppn;
    endfunction

    virtual function tb_gen_res walk(input mpn_t mpn);
        ppn_t ppn = m_ctl[0][paBits-1:12];
        pdn_t pdn = m_ctl[0][paBits-1: 3];

       `info($sformatf("mpn: %x", mpn));

        // top-down ptw: creating missed ptes in memory
        for (int i = 1; i <= ptwLvl; i++) begin
            bit [64:0] pte;
            bit [ 1:0] err;
            bit [ 1:0] blk;
            bit        hit;

            // no loss of bits
            mpn_t shf =   mpn >> m_shf[i];
            ppn_t top = {{pdnBits-9{1'b0}}, shf[8:0]};

            pdn = pdn & ~m_msk[i] | // base
                  top &  m_msk[i];  // idx

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

            hit = m_mem.chk(pdn);

            if (hit) begin
                pte =  m_mem.get_b(pdn);
                ppn =  pte[paBits-3:10];

                err[0] =  pte[64];
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
            end

           `info($sformatf("ptw: %x %x %x %x", {pdn, 3'b0}, err, ppn, hit));

            if (blk[0]) begin
                longint msk = (1 << ((ptwLvl - i) * 9)) - 1;

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


class tb_req extends tb_base;

   `register(tb_req);

    bit         m_vld;
    bit         m_ldp;
    bit         m_err;
    bit         m_rnw;
    llc_t       m_idx;
    mcn_t       m_mcn;
    pcn_t       m_pcn;
    bit [511:0] m_data;

    int         m_cnt;
    int         m_dly;

    ma_mem      m_llc;
    pa_mem      m_mem;

    function new();
        verif::plusargs arg = new("req.");

        int min = arg.get_int("min", 0);
        int max = arg.get_int("max", 10);

        m_mod = "req";

        m_vld =  1'b0;
        m_ldp =  1'b0;
        m_cnt =  0;
        m_dly = `urand(min, max);

       `get(ma_mem, "llc", m_llc);
       `get(pa_mem, "mem", m_mem);
    endfunction

    virtual function void init();
        tb_gen     gen = tb_gen::get_inst();
        tb_gen_res res;
        bit [11:6] pof;

       `rands(pof);
       `rands(m_rnw);
       `rands(m_data);

        res = m_ldp ? gen.walk(m_mcn[mcnBits-1:6]) :
                      gen.main();

        m_err =  res.err;
        m_mcn = {res.mpn, pof};
        m_pcn = {res.ppn, pof};

        // don't try to overwrite the reserved pte region
        if (m_ldp)
            m_rnw = 1'b1;

       `info($sformatf("%x %x -> %x %x %x", m_mcn, m_vld, m_pcn, res.ppn, m_err));
    endfunction

    virtual function void body();
        m_vld = 1'b1;

        if (m_rnw) begin
            pdn_t pdn = add_pcd(m_pcn);

            if (m_ldp || m_mem.chk(pdn, 8))
                m_data = clr_err(m_mem.get_d(pdn));
            else if (!m_ldp)
                m_mem.set_d(pdn, set_err(m_data, m_err));
        end
    endfunction

    virtual function void post();
        m_vld = 1'b0;
        m_cnt = 0;

        if (m_ldp && m_rnw && !m_err)
            m_llc.set_d(add_mcd(m_mcn), m_data);
    endfunction

    virtual function string show();
        return $sformatf("req(vld: %x, err: %x, rnw: %x, idx: %x, mcn: %x %x, pcn: %x %x, data: %x)",
                          m_vld,
                          m_err,
                          m_rnw,
                          m_idx,
                          m_mcn, m_mcn[mcnBits-1:6],
                          m_pcn, m_pcn[pcnBits-1:6],
                          m_data);
    endfunction

endclass


class tb_seq extends tb_base;

   `register(tb_seq);

    mailbox m_box;
    event   m_evt;

    function new();
        m_mod = "seq";

       `get(mailbox, "box", m_box);
    endfunction

    task put(ref tb_req req);
        m_box.put(req);
    endtask

    virtual task main();
        verif::plusargs arg = new("seq.");

        string str = arg.get_str("req", "tb_req");
        int    num = arg.get_int("num",  1000);

        // cache is everywhere. make things simple
        tb_req reqs [$];

        for (int i = 0; i < num; i++) begin
            tb_req req;

           `spawn(str, req);

            reqs.push_back(req);

            req.init();
            req.body();
            req.post();
        end

        // triggered in tb_drv
        @(m_evt);

        foreach (reqs[i])
            put(reqs[i]);

       `info("done.");
    endtask

endclass


class tb_ctl extends tb_base;

    tb_vif m_vif;

    tb_seq m_seq;
    tb_gen m_gen;

    function new(ref tb_vif vif);
        m_mod = "ctl";

        m_vif =  vif;
        m_gen =  tb_gen::get_inst();

       `get(tb_seq, "seq", m_seq);
    endfunction

    task main();
        for (int i = 0; i <= ptwLvl; i++) begin
            m_vif.ctl_req_i_valid     <= 1'b1;
            m_vif.ctl_req_i_bits_rnw  <= 1'b0;
            m_vif.ctl_req_i_bits_addr <= i[3:0];
            m_vif.ctl_req_i_bits_data <= m_gen.m_ctl[i];
           `waitt(m_vif.ctl_req_i_ready,  TO_MAX, "req timeout");
            m_vif.ctl_req_i_valid     <= 1'b0;

            m_vif.ctl_resp_o_ready    <= 1'b1;
           `waitt(m_vif.ctl_resp_o_valid, TO_MAX, "resp timeout");
            m_vif.ctl_resp_o_ready    <= 1'b0;

            if (~m_vif.ctl_resp_o_bits_sel)
               `err($sformatf("failed: %0d", i));
        end

        // trigger
        ->m_seq.m_evt;
    endtask

endclass


class tb_llc extends tb_base;

    ma_vif    m_vif;
    ma_mem    m_llc;

    tb_req    m_req [llcWays-1:0];
    semaphore m_sem;
    mailbox   m_box;
    mailbox   m_ldp;

    int       m_req_min;
    int       m_req_max;
    int       m_acc_min;
    int       m_acc_max;
    int       m_ldp_clr;
    int       m_ldp_set;

    function new(ref ma_vif vif);
        verif::plusargs arg = new("llc.");

        m_mod = "llc";
        m_vif =  vif;
        m_sem =  new(1);
        m_ldp =  new(0);

       `get(ma_mem,  "llc", m_llc);
       `get(mailbox, "box", m_box);

        m_req_min = arg.get_int("req_min", 0);
        m_req_max = arg.get_int("req_max", 100);
        m_acc_min = arg.get_int("acc_min", 0);
        m_acc_max = arg.get_int("acc_max", 100);
        m_ldp_clr = arg.get_int("ldp_clr", 1);
        m_ldp_set = arg.get_int("ldp_set", 1);

        foreach (m_req[i])
            m_req[i] = new();
    endfunction

    task cha_req();
        forever begin
            tb_req req = null;
            llc_t  idx;
            bit    vld;

            m_sem.get();
            foreach (m_req[i])
                if (!m_req[i].m_vld) begin
                    req = m_req[i];
                    idx = i[llcIdx-1:0];
                    break;
                end
            m_sem.put();

            if (req == null) begin
               `waitn(1);
                continue;
            end

            do begin
               `rands(vld, with { vld dist {
                    1'b0 := m_ldp_clr,
                    1'b1 := m_ldp_set
                };});

                if (vld && !m_ldp.try_get(req) || !vld)
                    m_box.get(req);

                vld = 1'b0;

                m_sem.get();
                foreach (m_req[i])
                    if  (m_req[i].m_vld && (m_req[i].m_mcn == req.m_mcn)) begin
                        vld = 1'b1;
                        break;
                    end
                m_sem.put();

            end while (vld);
           `waitn(req.m_dly);

            m_vif.llc_req_i_valid     <= 1'b1;
            m_vif.llc_req_i_bits_idx  <= idx;
            m_vif.llc_req_i_bits_rnw  <= req.m_rnw;
            m_vif.llc_req_i_bits_mcn  <= req.m_mcn;
            m_vif.llc_req_i_bits_pcn  <= req.m_pcn;
            m_vif.llc_req_i_bits_data <= req.m_data;
           `waitt(m_vif.llc_req_i_ready, TO_MAX, "cha req timeout");
            m_vif.llc_req_i_valid     <= 1'b0;

            req.body();

            m_sem.get();
            req.m_idx  = idx;
            m_req[idx] = req;
            m_sem.put();
        end
    endtask

    task cha_resp();
        forever begin
            tb_req req;
            int    dly = `urand(m_req_min, m_req_max);

            m_vif.llc_resp_o_ready     <= dly == 0;
           `waits(m_vif.llc_resp_o_valid);

            if (dly) begin
               `waitn(dly - 1);
                m_vif.llc_resp_o_ready <= 1'b1;
               `waitn(1);
            end
            m_vif.llc_resp_o_ready     <= 1'b0;

            m_sem.get();
            req = m_req[m_vif.llc_resp_o_bits_idx];

            if (!req.m_vld)
               `err($sformatf("unwanted idx: %0x vs. %s", m_vif.llc_resp_o_bits_idx,  req.show()));
            if ( req.m_err !=  m_vif.llc_resp_o_bits_err)
               `err($sformatf("err mismatch: %0x vs. %s", m_vif.llc_resp_o_bits_err,  req.show()));
            if ( req.m_rnw !=  m_vif.llc_resp_o_bits_rnw)
               `err($sformatf("rnw mismatch: %0x vs. %s", m_vif.llc_resp_o_bits_rnw,  req.show()));
            if (!req.m_err && (m_vif.llc_resp_o_bits_data != req.m_data))
               `err($sformatf("dat mismatch: %0x vs. %s", m_vif.llc_resp_o_bits_data, req.show()));

            req.post();
            m_sem.put();
        end
    endtask

    task cha_to();
        forever begin
           `waitn(1);

            m_sem.get();
            foreach (m_req[i])
                if  (m_req[i].m_vld && (++m_req[i].m_cnt >= TO_MAX))
                   `err($sformatf("cha req timeout: %0d", i));
            m_sem.put();
        end
    endtask

    task chc();
        forever begin
            mcn_t mcn;
            bit   hit;
            bit   ldp;
            int   dly = `urand(m_req_min, m_req_max);

            m_vif.llc_req_o_ready      <= dly == 0;
           `waits(m_vif.llc_req_o_valid);

            if (dly) begin
               `waitn(dly - 1);
                m_vif.llc_req_o_ready  <= 1'b1;
               `waitn(1);
            end
            m_vif.llc_req_o_ready      <= 1'b0;

            dly = `urand(m_acc_min, m_acc_max);
           `waitn(dly);

            mcn =  m_vif.llc_req_o_bits_mcn;
            hit =  m_llc.chk(add_mcd(mcn), 8);

            m_vif.llc_resp_i_valid     <= 1'b1;
            m_vif.llc_resp_i_bits_hit  <= hit;
            m_vif.llc_resp_i_bits_data <= m_llc.get_d(add_mcd(mcn));
           `waitt(m_vif.llc_resp_i_ready, TO_MAX, "chc resp timeout");
            m_vif.llc_resp_i_valid     <= 1'b0;

            // llc actively loads ptes back
           `rands(ldp, with { ldp dist {
                1'b0 := m_ldp_clr,
                1'b1 := m_ldp_set
            };});

            if (ldp && !hit) begin
                tb_req req = new();

                req.m_ldp = 1'b1;
                req.m_mcn = mcn;
                req.init();
                req.body();

                m_ldp.put(req);
            end
        end
    endtask

    task main();
        fork
            cha_req ();
            cha_resp();
            cha_to  ();
            chc     ();
        join
    endtask

endclass


class tb_mem extends tb_base;

    pa_vif    m_vif;
    pa_mem    m_mem;

    tb_req    m_req [memWays-1:0];
    semaphore m_sem;

    int       m_req_min;
    int       m_req_max;
    int       m_acc_min;
    int       m_acc_max;

    function new(ref pa_vif vif);
        verif::plusargs arg = new("mem.");

        m_mod = "mem";
        m_vif =  vif;
        m_sem =  new(1);

       `get(pa_mem, "mem", m_mem);

        m_req_min = arg.get_int("req_min", 0);
        m_req_max = arg.get_int("req_max", 100);
        m_acc_min = arg.get_int("acc_min", 0);
        m_acc_max = arg.get_int("acc_max", 100);

        foreach (m_req[i])
            m_req[i] = new();
    endfunction

    task cha_req();
        forever begin
            tb_req req;
            int    dly = `urand(m_req_min, m_req_max);

            m_vif.mem_req_o_ready     <= dly == 0;
           `waits(m_vif.mem_req_o_valid);

            if (dly) begin
               `waitn(dly - 1);
                m_vif.mem_req_o_ready <= 1'b1;
               `waitn(1);
            end
            m_vif.mem_req_o_ready     <= 1'b0;

            m_sem.get();
            req = m_req[m_vif.mem_req_o_bits_idx];

            if (req.m_vld)
               `err($sformatf("duplicated idx: %0x", m_vif.mem_req_o_bits_idx));

            req.m_vld  =  1'b1;
            req.m_dly  = `urand(m_acc_min, m_acc_max);
            req.m_rnw  =  m_vif.mem_req_o_bits_rnw;
            req.m_pcn  =  m_vif.mem_req_o_bits_pcn;
            req.m_data =  m_vif.mem_req_o_bits_data;
            m_sem.put();
        end
    endtask

    task cha_resp();
        int cnt = 0;

        forever begin
           `waitn(1);

            m_sem.get();
            foreach (m_req[i]) begin
                tb_req req = m_req[i];

                if (req.m_vld && req.m_dly)
                    req.m_dly--;
            end
            m_sem.put();

            if (m_vif.mem_resp_i_valid)
                if (m_vif.mem_resp_i_ready) begin
                    m_vif.mem_resp_i_valid <= 1'b0;
                    cnt = 0;
                end else begin
                    if (++cnt >= TO_MAX)
                       `err("resp timeout");

                    continue;
                end

            m_sem.get();
            foreach (m_req[i]) begin
                tb_req req = m_req[i];

                if (req.m_vld && !req.m_dly) begin
                    bit [519:0] d = m_mem.get_d(add_pcd(req.m_pcn));

                    if (req.m_rnw)
                        req.m_data = clr_err(d);
                    else
                        m_mem.set_d(add_pcd(req.m_pcn),
                                    set_err(req.m_data, 1'b0));

                    req.m_vld = 1'b0;

                    m_vif.mem_resp_i_valid     <= 1'b1;
                    m_vif.mem_resp_i_bits_err  <= get_err(d);
                    m_vif.mem_resp_i_bits_idx  <= i[memIdx-1:0];
                    m_vif.mem_resp_i_bits_rnw  <= req.m_rnw;
                    m_vif.mem_resp_i_bits_data <= req.m_data;

                    break;
                end
            end
            m_sem.put();
        end
    endtask

    task main();
        fork
            cha_req ();
            cha_resp();
        join
    endtask

endclass


class tb_env extends tb_base;

    tb_ctl  m_ctl;
    tb_gen  m_gen;
    tb_seq  m_seq;
    tb_llc  m_uvc_llc;
    tb_mem  m_uvc_mem;

    mailbox m_box;

    ma_mem  m_llc;
    pa_mem  m_mem;

    function new(input tb_vif vif, ma_vif llc, pa_vif mem);
        verif::plusargs arg = new("env.");

        m_box     = new(1);
        m_llc     = new("LLC");
        m_mem     = new("MEM");

       `set(ma_mem,  "llc", m_llc);
       `set(pa_mem,  "mem", m_mem);
       `set(mailbox, "box", m_box);

       `spawn(arg.get_str("gen", "tb_gen"), m_gen);
       `spawn(arg.get_str("seq", "tb_seq"), m_seq);

       `set(tb_gen,  "gen", m_gen);
       `set(tb_seq,  "seq", m_seq);

        m_ctl     = new(vif);
        m_uvc_llc = new(llc);
        m_uvc_mem = new(mem);
    endfunction

    task main();
        fork
            m_ctl    .main();
            m_seq    .main();
            m_uvc_llc.main();
            m_uvc_mem.main();
        join
    endtask

endclass