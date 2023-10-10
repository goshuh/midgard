interface tb_ctl_intf (
    input wire clock,
    input wire reset
);

    bit            inv_req_i_valid;
    bit [     1:0] inv_req_i_bits_idx;
    bit [    57:0] inv_req_i_bits_mcn;

    bit [    63:0] satp_i;
    bit [    63:0] uatp_i;
    bit [     5:0] uatc_i_idx;
    bit [     5:0] uatc_i_vsc;
    bit [     5:0] uatc_i_top;
    bit [     5:0] uatc_i_tsl;
    bit [    31:0] uatc_i_mmask;
    bit [    31:0] uatc_i_imask;
    bit [     4:0] uatc_i_vmask;
    bit [    19:0] uatc_i_tmask;

    initial begin
        inv_req_i_valid <= 1'b0;
    end

endinterface


interface tb_vlb_intf (
    input wire clock,
    input wire reset
);

    bit            vlb_req_i_valid;
    tb_base::vlb_t vlb_req_i_bits_idx;
    tb_base::vpn_t vlb_req_i_bits_vpn;
    bit [     2:0] vlb_req_i_bits_kill;
    bit            vlb_res_o_valid;
    tb_base::vlb_t vlb_res_o_bits_idx;
    bit            vlb_res_o_bits_vld;
    bit            vlb_res_o_bits_err;
    tb_base::mpn_t vlb_res_o_bits_mpn;
    bit [     3:0] vlb_res_o_bits_attr;
    bit            vlb_ttw_o_valid;
    tb_base::vlb_t vlb_ttw_o_bits_idx;
    bit            vlb_ttw_o_bits_vld;
    bit            vlb_ttw_o_bits_err;
    tb_base::mpn_t vlb_ttw_o_bits_mpn;
    bit [     3:0] vlb_ttw_o_bits_attr;
    bit [     2:0] vlb_kill_i;
    bit            vlb_busy_o;

    // tb usage
    bit            vlb_req_i_valid_q;
    tb_base::vlb_t vlb_req_i_bits_idx_q;

    initial begin
        vlb_req_i_valid <= 1'b0;
        vlb_kill_i      <= 3'b0;
    end

endinterface


interface tb_mem_intf (
    input wire clock,
    input wire reset
);

    bit            mem_req_o_valid;
    bit            mem_req_o_ready;
    tb_base::ttw_t mem_req_o_bits_idx;
    tb_base::mcn_t mem_req_o_bits_mcn;
    bit            mem_res_i_ready;
    bit            mem_res_i_valid;
    tb_base::ttw_t mem_res_i_bits_idx;
    bit [   511:0] mem_res_i_bits_data;

    initial begin
        mem_req_o_ready <= 1'b0;
        mem_res_i_valid <= 1'b0;
    end

endinterface


typedef virtual tb_ctl_intf tb_vif;
typedef virtual tb_vlb_intf va_vif;
typedef virtual tb_mem_intf ma_vif;
