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