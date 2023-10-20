typedef struct {
    bit            vld;
    bit            err;
    bit [     7:0] attr;
    tb_base::vpn_t base;
    tb_base::vpn_t bound;
    tb_base::vpn_t offs;
} tb_vma;


typedef verif::memory #(tb_base::mcnBits, 512) ma_mem;