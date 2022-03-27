interface tb_vlb_intf (
    input wire clock,
    input wire reset
);

    bit            vlb_req_i_valid;
    tb_base::vlb_t vlb_req_i_bits_idx;
    tb_base::vpn_t vlb_req_i_bits_vpn;
    bit [     1:0] vlb_req_i_bits_kill;
    bit            vlb_resp_o_valid;
    tb_base::vlb_t vlb_resp_o_bits_idx;
    bit            vlb_resp_o_bits_vld;
    bit            vlb_resp_o_bits_err;
    tb_base::mpn_t vlb_resp_o_bits_mpn;
    bit [     3:0] vlb_resp_o_bits_attr;
    bit            vlb_fill_o_valid;
    tb_base::vlb_t vlb_fill_o_bits_idx;
    bit            vlb_fill_o_bits_vld;
    bit            vlb_fill_o_bits_err;
    tb_base::mpn_t vlb_fill_o_bits_mpn;
    bit [     3:0] vlb_fill_o_bits_attr;
    bit [     1:0] vlb_kill_i;
    bit            vlb_busy_o;

    initial begin
        vlb_req_i_valid  <= 1'b0;
        vlb_kill_i       <= 3'b0;
    end

endinterface


interface tb_mem_intf (
    input wire clock,
    input wire reset
);

    bit            mem_req_o_ready;
    bit            mem_req_o_valid;
    tb_base::mcn_t mem_req_o_bits_mcn;
    bit            mem_resp_i_ready;
    bit            mem_resp_i_valid;
    bit [   511:0] mem_resp_i_bits_data;

    bit [    63:0] satp_i;

    initial begin
        mem_req_o_ready  <= 1'b0;
        mem_resp_i_valid <= 1'b0;
    end

endinterface


typedef virtual tb_vlb_intf tb_vif;
typedef virtual tb_mem_intf ma_vif;