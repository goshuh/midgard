module SPRAM #(parameter A = 16,
               parameter D = 32,
               parameter S = 2
) (
    input  wire         clk,

    input  wire         en,
    input  wire         wnr,
    input  wire [A-1:0] addr,

    output wire [D-1:0] rdata,
    input  wire [D-1:0] wdata,
    input  wire [S-1:0] wstrb
);

    localparam E = D / S;

    reg [D-1:0] ram [2**A-1:0];
    reg [D-1:0] rdata_q;

    wire ren = en & ~wnr;
    wire wen = en &  wnr;

    always_ff @(posedge clk)
        if (ren)
            rdata_q <= ram[addr];

    generate
    genvar i;
    for (i = 0; i < S; i++)
        always_ff @(posedge clk)
            if (wen & wstrb[i])
                ram[addr][i*E+:E] <= wdata[i*E+:E];
    endgenerate

    assign rdata = rdata_q;

endmodule
