`include "verif.svh"


//
// base class

class tb_base extends verif::object;

    localparam maBits = 64;
    localparam paBits = 48;

endclass


//
// interface

interface tb_intf (
    input wire clock,
    input wire reset
);

    logic                        mmu_req_i_ready;
    logic                        mmu_req_i_valid;
    logic [tb_base::maBits-13:0] mmu_req_i_bits;
    logic                        mmu_resp_o_ready;
    logic                        mmu_resp_o_valid;
    logic                        mmu_resp_o_bits_err;
    logic [tb_base::paBits-13:0] mmu_resp_o_bits_ppn;

    logic                        cfg_req_i_ready;
    logic                        cfg_req_i_valid;
    logic                        cfg_req_i_bits_ren;
    logic [                 3:0] cfg_req_i_bits_addr;
    logic [                63:0] cfg_req_i_bits_data;
    logic                        cfg_resp_o_ready;
    logic                        cfg_resp_o_valid;
    logic                        cfg_resp_o_bits_vld;
    logic [                63:0] cfg_resp_o_bits_data;

    initial begin
        mmu_req_i_valid  <= 1'b0;
        mmu_resp_o_ready <= 1'b0;

        cfg_req_i_valid  <= 1'b0;
        cfg_resp_o_ready <= 1'b0;
    end

endinterface


interface tb_intf_mem (
    input wire clock,
    input wire reset
);

    logic        mem_req_o_ready;
    logic        mem_req_o_valid;
    logic [63:0] mem_req_o_bits;
    logic        mem_resp_i_ready;
    logic        mem_resp_i_valid;
    logic        mem_resp_i_bits_err;
    logic [63:0] mem_resp_i_bits_pte;

    initial begin
        mem_req_o_ready  <= 1'b0;
        mem_resp_i_valid <= 1'b0;
    end

    // vcs compile bug:
    //   if higher unused bits are initialized here, vcs would stack overflow
    //   Info: [STACK_OVERFLOW] Stack Overflow Detected.
    //   Note: [STACK_INCREASED] Bumping stack from xxx to xxx bytes

endinterface


typedef virtual tb_intf     tb_vintf;
typedef virtual tb_intf_mem tb_vintf_mem;


`define clk m_vif.clock
`define rst m_vif.reset


//
// tb spefic

typedef verif::memory #(tb_base::maBits, 64) ma_mem;
typedef verif::memory #(tb_base::paBits, 65) pa_mem;


class tb_gen extends tb_base;

   `register(tb_gen);

    localparam ptwLvl = (maBits - 4) / 9;
    localparam ptwTop =  maBits - 9 * (ptwLvl - 1) - 12;

    verif::plusargs m_arg;
    ma_mem          m_llc;
    pa_mem          m_mem;

    bit [63:0] m_csr [0:ptwLvl];
    bit [ 8:0] m_mpn [1:ptwLvl][$];

    int m_dist_mpn [1:ptwLvl][4];
    int m_dist_err [1:ptwLvl][4];
    int m_dist_blk [1:ptwLvl][2];
    int m_dist_llc [1:ptwLvl][2];

    bit [paBits-1:12] m_ppn [bit [paBits-1:12]];

    function new();
        m_mod = "gen";
        m_arg = new("gen.");

       `get(ma_mem, "llc", m_llc);
       `get(pa_mem, "mem", m_mem);

        init();
        hook();
    endfunction

    virtual function void init();
        m_csr = '{{{64-paBits{1'b0}},   1'b1, {paBits-1{1'b0}}},  // root
                  {{64-maBits{1'b0}}, 3'b001, {maBits-3{1'b0}}},  // l1 base
                  {{64-maBits{1'b0}}, 3'b010, {maBits-3{1'b0}}},  // l2 base
                  {{64-maBits{1'b0}}, 3'b011, {maBits-3{1'b0}}},  // l3 base
                  {{64-maBits{1'b0}}, 3'b100, {maBits-3{1'b0}}},  // l4 base
                  {{64-maBits{1'b0}}, 3'b101, {maBits-3{1'b0}}},  // l5 base
                  {{64-maBits{1'b0}}, 3'b110, {maBits-3{1'b0}}}}; // l6 base
    endfunction

    virtual function void hook();
        for (int i = 1; i <= ptwLvl; i++) begin
            m_dist_mpn[i][0] = 1;
            m_dist_mpn[i][1] = 30;
            m_dist_mpn[i][2] = 0;
            m_dist_mpn[i][3] = 1;

            m_dist_err[i][0] = 0;
            m_dist_err[i][1] = 1;
            m_dist_err[i][2] = 1;
            m_dist_err[i][3] = 30;

            m_dist_blk[i][0] = 30;
            m_dist_blk[i][1] = 1;

            m_dist_llc[i][0] = 1;
            m_dist_llc[i][1] = 1;
        end
    endfunction

    virtual function bit [maBits-1:12] gen_mpn();
        bit [ptwLvl*9-1:0] mpn;
        bit [         8:0] lvl;

        for (int i = 1; i <= ptwLvl; i++) begin
            int idx;
            bit get;
            bit set;

            // vcs parser bug
            //   not yet implemented: usage of dist inside the property
            randcase
                m_dist_mpn[i][0]: get = 1'b0;
                m_dist_mpn[i][1]: get = 1'b1;
            endcase

            randcase
                m_dist_mpn[i][2]: set = 1'b0;
                m_dist_mpn[i][3]: set = 1'b1;
            endcase

            if (get & (m_mpn[i].size() != 0)) begin
                // vcs parser bug
                //   cannot write m_mpn[i] here
                //`rands(lvl, with { lvl inside m_mpn[i]; });
               `rands(idx, with { idx >= 0; idx < m_mpn[i].size(); });

                lvl = m_mpn[i][idx];
            end else
               `rands(lvl);

            if (set)
                m_mpn[i].push_back(lvl);

            mpn[i*9-:9] = lvl;
        end

        return mpn[maBits-13:0] & {3'b0, {maBits-15{1'b1}}};
    endfunction

    virtual function bit [paBits-1:12] gen_ppn(input bit [paBits-1:12] ppa);
        bit [paBits-1:12] ppn;

        if (m_ppn.exists(ppa))
            ppn = m_ppn[ppa];
        else begin
            do
               `rands(ppn);
            while (m_ppn.exists(ppn));

            m_ppn[ppa] = ppn;
        end

        return ppn;
    endfunction

    virtual function bit [maBits-1:0] cal_pma(input int i,
                                                    bit [maBits-1:12] mpn);
        bit [63:0] ret = 64'b0;

        case (i)
            1: ret = {m_csr[1][63:10], mpn[63:57], 3'b0};
            2: ret = {m_csr[2][63:19], mpn[63:48], 3'b0};
            3: ret = {m_csr[3][63:28], mpn[63:39], 3'b0};
            4: ret = {m_csr[4][63:37], mpn[63:30], 3'b0};
            5: ret = {m_csr[5][63:46], mpn[63:21], 3'b0};
            6: ret = {m_csr[6][63:55], mpn[63:12], 3'b0};
            default:
               `err($sformatf("cal_pma: invalid level: %0d", i));
        endcase

        return ret[maBits-1:0];
    endfunction

    virtual function bit [paBits-1:0] cal_ppa(input int i,
                                                    bit [maBits-1:12] mpn,
                                                    bit [      64:0 ] pte);
        bit [47:0] ret = 48'b0;

        case (i)
            1: ret = {pte[47:10], mpn[63:57], 3'b0};
            2: ret = {pte[47:12], mpn[56:48], 3'b0};
            3: ret = {pte[47:12], mpn[47:39], 3'b0};
            4: ret = {pte[47:12], mpn[38:30], 3'b0};
            5: ret = {pte[47:12], mpn[29:21], 3'b0};
            6: ret = {pte[47:12], mpn[20:12], 3'b0};
            default:
               `err($sformatf("cal_ppa: invalid level: %0d", i));
        endcase

        return ret[paBits-1:0];
    endfunction

    virtual function bit [paBits-1:12] cal_ppn(input int i,
                                                     bit [maBits-1:12] mpn,
                                                     bit [      64:0 ] pte);
        bit [47:12] ret = 36'b0;

        case (i)
            3: ret = {pte[47:39], mpn[38:12]};
            4: ret = {pte[47:30], mpn[29:12]};
            5: ret = {pte[47:21], mpn[20:12]};
            6: ret =  pte[47:12];
            default:
               `err($sformatf("cal_ppn: invalid level: %0d", i));
        endcase

        return ret[paBits-1:12];
    endfunction

    virtual function bit [maBits+paBits-24:0] main();
        bit [maBits-1:12] mpn =  gen_mpn();
        bit [paBits-1:12] ppn = {paBits-12{1'b0}};
        bit [      64:0 ] pte = {1'b0,  m_csr[0]};

        for (int i = 1; i <= ptwLvl; i++) begin
            bit [maBits-1:0] pma = cal_pma(i, mpn);
            bit [paBits-1:0] ppa = cal_ppa(i, mpn, pte);

            bit [1:0] err;
            bit       blk;
            bit       llc;

            randcase
                m_dist_err[i][0]: err = 2'h0; // skip: consistent issue
                m_dist_err[i][1]: err = 2'h1; // bus err
                m_dist_err[i][2]: err = 2'h2; // inv
                m_dist_err[i][3]: err = 2'h3; // vld, last non-blk
            endcase

            randcase
                m_dist_blk[i][0]: blk = 1'h0;
                m_dist_blk[i][1]: blk = 1'h1;
            endcase

            randcase
                m_dist_llc[i][0]: llc = 1'h0;
                m_dist_llc[i][1]: llc = 1'h1;
            endcase

            if (m_mem.chk(ppa)) begin
                pte =  m_mem.get_byte(ppa);

               `info($sformatf("%0x: l%0d %0x %0x%0x%0x old", mpn, i, pte[paBits-1:12],
                                                                      pte[64],
                                                                      pte[ 1],
                                                                      pte[ 0]));

                if (m_llc.chk(pma))
                   `info($sformatf("%0x: llc %0x", mpn, pma));
                if (1)
                   `info($sformatf("%0x: mem %0x", mpn, ppa));
            end else begin
                ppn =  gen_ppn(ppa[paBits-1:12]);

                pte = {err == 2'h1,
                      {64-paBits{1'b0}},
                       ppn,
                       10'b0,
                       blk | (i == ptwLvl) & (err == 2'h3),
                       err != 2'h2};

               `info($sformatf("%0x: l%0d %0x %0x%0x%0x", mpn, i, ppn, pte[64],
                                                                       pte[ 1],
                                                                       pte[ 0]));

                if (llc & ~pte[64]) begin
                   `info($sformatf("%0x: llc %0x", mpn, pma));
                    m_llc.set_byte(pma, pte[63:0]);
                end

                if ( 1 ) begin
                   `info($sformatf("%0x: mem %0x", mpn, ppa));
                    m_mem.set_byte(ppa, pte[64:0]);
                end
            end

            if (pte[0] & ~pte[1] & (i == ptwLvl) |
                pte[0] &  pte[1] & (i <= 2) |
               ~pte[0] |  pte[64]) begin
               `info($sformatf("%0x: err", mpn));
                return {1'b1, ppn, mpn};
            end

            if (pte[0] &  pte[1] & ~pte[64]) begin
                ppn = cal_ppn(i, mpn, pte);

               `info($sformatf("%0x: %0x", mpn, ppn));
                return {1'b0, ppn, mpn};
            end
        end

        return {1'b1, ppn, mpn};
    endfunction

endclass


class tb_trans extends tb_base;

   `register(tb_trans);

    bit [maBits-1:12] m_mpn;
    bit [paBits-1:12] m_ppn;
    bit               m_err;

    bit [      31:0 ] m_dly;

    function new();
        m_mpn = {maBits-12{1'b0}};
        m_ppn = {paBits-12{1'b0}};
        m_err =   1'b0;
        m_dly =  32'b0;
    endfunction

    function void init();
        verif::plusargs arg = new("trans.");

        m_min =  arg.get_int("min", 0);
        m_max =  arg.get_int("max", 100);
        m_dly = `urand();
    endfunction

    virtual function void pre();
        tb_gen gen;
       `get(tb_gen, "gen", gen);

        init();

       {m_err, m_ppn, m_mpn} = gen.main();
    endfunction

    virtual function void body();
    endfunction

    virtual function void post();
    endfunction

endclass


class tb_seq extends tb_base;

   `register(tb_seq);

    mailbox m_mbx_drv;
    event   m_evt;

    function new();
        m_mod = "seq";

       `get(mailbox, "seq", m_mbx_drv);
    endfunction

    task put(ref tb_trans tr);
        fork
            m_mbx_drv.put(tr);
        join
    endtask

    virtual task main();
        verif::plusargs arg = new("seq.");

        string str = arg.get_str("name", "tb_trans");

        // triggered in tb_drv
        @(m_evt);

        forever
           `start(tb_trans, str);
    endtask

endclass


class tb_drv extends tb_base;

    tb_vintf m_vif;
    tb_seq   m_seq;
    tb_gen   m_gen;

    mailbox  m_mbx_seq;
    mailbox  m_mbx_scb;

    function new(ref tb_vintf vif);
        m_mod = "drv";
        m_vif =  vif;

       `get(tb_seq,  "seq", m_seq);
       `get(tb_gen,  "gen", m_gen);
       `get(mailbox, "seq", m_mbx_seq);
       `get(mailbox, "drv", m_mbx_scb);
    endfunction

    task cfg();
        foreach (m_gen.m_csr[i]) begin
            m_vif.cfg_req_i_valid     <= 1'b1;
            m_vif.cfg_req_i_bits_ren  <= 1'b0;
            m_vif.cfg_req_i_bits_addr <= i[3:0];
            m_vif.cfg_req_i_bits_data <= m_gen.m_csr[i];

           `waits(m_vif.cfg_req_i_ready);
           `info($sformatf("cfg %0x: %0x", i[3:0], m_gen.m_csr[i]));
            m_vif.cfg_req_i_valid     <= 1'b0;

            m_vif.cfg_resp_o_ready    <= 1'b1;

           `waits(m_vif.cfg_resp_o_valid);
           `info($sformatf("cfg %0x: %0x", i[3:0], m_vif.cfg_resp_o_bits_vld));
            m_vif.cfg_resp_o_ready    <= 1'b0;

            if (~m_vif.cfg_resp_o_bits_vld)
               `err($sformatf("cfg: initialization failed on CSR[%0d]", i));
        end

        // trigger
        ->m_seq.m_evt;
    endtask

    task mmu();
        tb_trans tr;

        forever begin
            m_mbx_seq.get(tr);

            tr.pre();

           `waits(m_vif.mmu_req_i_ready);
           `waitn(tr.m_dly);
            m_vif.mmu_req_i_valid  <= 1'b1;
            m_vif.mmu_req_i_bits   <= tr.m_mpn;

           `waits(m_vif.mmu_req_i_ready);
            m_vif.mmu_req_i_valid  <= 1'b0;

           `info($sformatf("%0x: %0x %0x", tr.m_mpn, tr.m_err, tr.m_ppn));

            tr.body();

            m_vif.mmu_resp_o_ready <= 1'b1;
           `waits(m_vif.mmu_resp_o_valid);

            tr.post();

            m_vif.mmu_resp_o_ready <= 1'b0;

           `info($sformatf("%0x: to scb", tr.m_mpn));

            m_mbx_scb.put(tr);
        end
    endtask

    task main();
        fork
            cfg();
            mmu();
        join
    endtask

endclass


class tb_slv extends tb_base;

    tb_vintf_mem m_vif;

    int m_req_min;
    int m_req_max;
    int m_acc_min;
    int m_acc_max;

    function new(ref tb_vintf_mem vif, input string str);
        verif::plusargs arg = new($sformatf("drv.%s.", str));

        m_vif = vif;

        m_req_min = arg.get_int("req_min", 0);
        m_req_max = arg.get_int("req_max", 100);
        m_acc_min = arg.get_int("acc_min", 0);
        m_acc_max = arg.get_int("acc_max", 100);
    endfunction

    virtual function bit [64:0] get_data(input bit [63:0] addr);
    endfunction

    task slv();
        bit [31:0] dly;
        bit [64:0] data;

        forever begin
            dly = `urand(m_req_min, m_req_max);

            if (dly == 0)
                m_vif.mem_req_o_ready <= 1'b1;

           `waits(m_vif.mem_req_o_valid);
            data = get_data(m_vif.mem_req_o_bits);

            if (dly) begin
               `waitn(dly - 1);

                m_vif.mem_req_o_ready <= 1'b1;
               `waitn();
            end

            m_vif.mem_req_o_ready     <= 1'b0;

            dly = `urand(m_acc_min, m_acc_max);

           `waitn(dly);
            m_vif.mem_resp_i_valid    <= 1'b1;
            m_vif.mem_resp_i_bits_err <= data[64  ];
            m_vif.mem_resp_i_bits_pte <= data[63:0];

           `waits(m_vif.mem_resp_i_ready);
            m_vif.mem_resp_i_valid    <= 1'b0;
        end
    endtask

    task main();
        fork
            slv();
        join
    endtask

endclass


class tb_slv_llc extends tb_slv;

    ma_mem m_llc;

    function new(ref tb_vintf_mem vif);
        super.new(vif, "llc");
        m_mod = "llc";

       `get(ma_mem, "llc", m_llc);
    endfunction

    virtual function bit [64:0] get_data(input bit [63:0] addr);
        bit        hit =  m_llc.chk(addr[maBits-1:0]);
        bit [64:0] ret = {hit, hit ? m_llc.get_byte(addr[maBits-1:0]) : 64'hdeadbeef};

       `info($sformatf("%0x: %0x %0x", addr[maBits-1:0], ret[64], ret[63:0]));
        return ret;
    endfunction

endclass


class tb_slv_mem extends tb_slv;

    pa_mem m_mem;

    function new(ref tb_vintf_mem vif);
        super.new(vif, "mem");
        m_mod = "mem";

       `get(pa_mem, "mem", m_mem);
    endfunction

    virtual function bit [64:0] get_data(input bit [63:0] addr);
        bit        hit = m_mem.chk(addr[paBits-1:0]);
        bit [64:0] ret = hit ? m_mem.get_byte(addr[paBits-1:0]) : {1'b1, 64'hdeadbeef};

       `info($sformatf("%0x: %0x %0x", addr[paBits-1:0], ret[64], ret[63:0]));
        return ret;
    endfunction

endclass


class tb_mon extends tb_base;

    tb_vintf m_vif;

    mailbox  m_mbx_scb;

    function new(ref tb_vintf vif);
        m_mod = "mon";
        m_vif =  vif;

       `get(mailbox, "mon", m_mbx_scb);
    endfunction

    task mon();
        verif::plusargs arg = new("mon.");
        int max = arg.get_int("max", 5000);

        forever begin
            tb_trans tr = new();

           `waits(m_vif.mmu_req_i_valid);
           `waitt(m_vif.mmu_req_i_ready,  max, "req hs t/o!");

            tr.m_mpn = m_vif.mmu_req_i_bits;

           `waitt(m_vif.mmu_resp_o_valid, max, "req t/o!");
           `waitt(m_vif.mmu_resp_o_ready, max, "rsp hs t/o!");

            tr.m_err = m_vif.mmu_resp_o_bits_err;
            tr.m_ppn = m_vif.mmu_resp_o_bits_ppn;

           `info($sformatf("%0x: %0x %0x: to scb", tr.m_mpn, tr.m_err, tr.m_ppn));

            m_mbx_scb.put(tr);
        end
    endtask

    task main();
        fork
            mon();
        join
    endtask

endclass


class tb_scb extends tb_base;

    mailbox m_mbx_drv;
    mailbox m_mbx_mon;

    function new();
        m_mod = "scb";

       `get(mailbox, "drv", m_mbx_drv);
       `get(mailbox, "mon", m_mbx_mon);
    endfunction

    task scb();
        forever begin
            tb_trans tx;
            tb_trans rx;

            fork
                m_mbx_drv.get(tx);
                m_mbx_mon.get(rx);
            join

           `info($sformatf("%0x: %0x %0x", tx.m_mpn, tx.m_err, tx.m_ppn));
           `info($sformatf("%0x: %0x %0x", rx.m_mpn, rx.m_err, rx.m_ppn));

            if (tx.m_mpn != rx.m_mpn)
               `err("mpn mismatch");
            if (tx.m_err != rx.m_err)
               `err("err mismatch");
            if (~rx.m_err & (tx.m_ppn != rx.m_ppn))
               `err("ppn mismatch");
        end
    endtask

    task main();
        fork
            scb();
        join
    endtask

endclass


class tb_env extends tb_base;

    tb_vintf   m_vif;

    tb_gen     m_gen;
    tb_seq     m_seq;
    tb_drv     m_drv;
    tb_slv_llc m_slv_llc;
    tb_slv_mem m_slv_mem;
    tb_mon     m_mon;
    tb_scb     m_scb;

    mailbox    m_mbx_seq;
    mailbox    m_mbx_drv;
    mailbox    m_mbx_mon;

    ma_mem     m_llc;
    pa_mem     m_mem;

    function new(input tb_vintf     vif,
                       tb_vintf_mem vif_llc,
                       tb_vintf_mem vif_mem);

        verif::plusargs arg = new("env.");

        // 1.
        m_mbx_seq = new(1);
        m_mbx_drv = new();
        m_mbx_mon = new();

        m_llc     = new("llc");
        m_mem     = new("mem");

       `set(mailbox, "seq", m_mbx_seq);
       `set(mailbox, "drv", m_mbx_drv);
       `set(mailbox, "mon", m_mbx_mon);

       `set(ma_mem,  "llc", m_llc);
       `set(pa_mem,  "mem", m_mem);

        // 2.
       `spawn(arg.get_str("seq", "tb_seq"), m_seq);
       `spawn(arg.get_str("gen", "tb_gen"), m_gen);

       `set(tb_gen,  "gen", m_gen);
       `set(tb_seq,  "seq", m_seq);

        m_drv     = new(vif);
        m_slv_llc = new(vif_llc);
        m_slv_mem = new(vif_mem);
        m_mon     = new(vif);
        m_scb     = new();
    endfunction

    task main();
        fork
            m_seq    .main();
            m_drv    .main();
            m_slv_llc.main();
            m_slv_mem.main();
            m_mon    .main();
            m_scb    .main();
        join
    endtask

endclass
