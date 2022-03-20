typedef struct packed {
    bit            vld;
    bit            err;
    bit [     3:0] attr;
    tb_base::vpn_t base;
    tb_base::vpn_t bound;
    tb_base::vpn_t offs;
} tb_vma;


typedef struct packed {
    tb_base::pad_t                        pad;
    bit                                   bot;
    tb_base::mcn_t [tb_base::btOrder  :0] ptr;
    tb_vma         [tb_base::btOrder-1:0] vma;
} tb_tab;


typedef verif::memory #(tb_base::mcnBits, 512) ma_mem;