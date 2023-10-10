`include "tb.svh"


typedef struct packed {
    bit            vld;
    bit [     2:0] pad;
    bit [     3:0] attr;
    bit [    15:0] sdid;
    tb_base::vpn_t offs;
    tb_base::vpn_t bound;
} vsc_pvma;


class tb_vsc extends tb_base;

   `register(tb_vsc);

    ma_mem     m_l1d;

    mpn_t      m_atp;
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
    int        m_dis_err [];

    static tb_vsc m_inst;

    static function tb_vsc get_inst();
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
        // TODO: make these configurable
        m_cfg_idx   =  6'd24;
        m_cfg_vsc   =  6'd40;
        m_cfg_top   =  6'd44;
        m_cfg_tsl   =  6'd15;
        m_cfg_mmask = 32'hffffff;
        m_cfg_imask = 32'hffff;
        m_cfg_vmask =  5'hf;
        m_cfg_tmask = 20'hfffff;

        m_dis_old   = '{1, 100};
        m_dis_err   = '{10, 5, 100};
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

            randcase
            m_dis_err[0]: vpn[15: 0] |= vpn[47:32];
            m_dis_err[1]: vpn[51:49]  = 3'b1;
            m_dis_err[2]: vpn[51:49]  = 3'b0;
            endcase

        end else
           `rands(vpn);

        return vpn;
    endfunction

    virtual function tb_vma gen_vma();
        tb_vma   vma;
        vsc_pvma raw;

        bit [511:0] dat;
        bit [ 36:0] off;
        mcn_t       mcn;

        do begin
            bit [ 63:0] bot;
            bit [ 63:0] ran;
            bit [ 15:0] idx;
            bit [  3:0] vsc;
            bit [ 19:0] top;

           `rands(bot, with {
                bot[63:49] == 3'b0;
            });

            idx = bot[39:24];
            vsc = bot[43:40];
            top = bot[63:44];

            // the formula
            if (vsc == 4'b0)
                off = {top, idx, 1'b0};
            else begin
                bit [15:0] msk = (16'b1 << vsc) - 16'b1;

                off = {top, idx & ~msk | (msk >> 1), 1'b1};
            end

            mcn = {m_atp, 6'b0} + off[36:2];
            dat =  m_l1d.get_b(mcn);

            case (off[1:0])
            2'd0: raw = dat[127:  0];
            2'd1: raw = dat[255:128];
            2'd2: raw = dat[383:256];
            2'd3: raw = dat[511:384];
            endcase

            if (raw.vld == 1'b0) begin
                bit [63:0] msk = (64'h1 << ({4'b0, vsc} + 8'd24)) - 64'h1;

               `rands(ran);

                vma.vld   =  1'b1;
                vma.err   =  1'b0;
                vma.base  = {bot[vaBits-1:44], vsc, idx & ~msk[39:24], 12'b0};
                vma.bound =  vma.base + (ran[vaBits-1:12] & msk[vaBits-1:12]);
                vma.offs  =  mcn;
                vma.attr  =  4'b0;

                break;
            end
        end while (1);

        m_vma[vma.base] = vma;

        raw.vld   =  1'b1;
        raw.bound =  vma.bound;
        raw.offs  = {vpnBits{1'b0}};
        raw.attr  =  vma.attr;
        raw.sdid  =  16'b0;

        case (off[1:0])
        2'd0: dat[127:  0] = raw;
        2'd1: dat[255:128] = raw;
        2'd2: dat[383:256] = raw;
        2'd3: dat[511:384] = raw;
        endcase

        m_l1d.set_b(mcn, dat);

        return vma;
    endfunction

    function void gen_all(input int num);
        for (int i = 0; i < num; i++)
            gen_vma();
    endfunction

    virtual function tb_vma walk(input vpn_t vpn);
        tb_vma vma;
        vpn_t  prv;

        bit [63:0] bot = {vpn, 12'b0};
        bit [15:0] idx =  bot[39:24];
        bit [ 3:0] vsc =  bot[43:40];
        bit [63:0] msk = (64'b1 << ({4'b0, vsc} + 8'd24)) - 64'h1;

        prv = {bot[vaBits-1:44], vsc, idx & ~msk[39:24], 12'b0};

        if (m_vma.exists(prv)) begin
            vma     = m_vma[prv];
            vma.err = vpn > vma.bound;
        end else if (|bot[63:49]) begin
            vma.vld = 1'b0;
            vma.err = 1'b1;
        end else begin
            vma.vld = 1'b0;
            vma.err = 1'b0;
        end

        return vma;
    endfunction

    function tb_vma main(ref vpn_t vpn);
        vpn = gen_vpn();

        return walk(vpn);
    endfunction

endclass