typedef struct {
    bit            err;
    tb_base::mpn_t mpn;
    tb_base::ppn_t ppn;
} tb_gen_res;


typedef verif::memory #(tb_base::mdnBits, 64) ma_mem;
typedef verif::memory #(tb_base::pdnBits, 64) pa_mem;
typedef verif::memory #(tb_base::pcnBits,  1) pe_mem;