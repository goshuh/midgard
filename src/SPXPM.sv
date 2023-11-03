module SPXPM #(parameter A =  6,
               parameter D =  32,
               parameter F = "none"
) (
    input  wire         clk,
    input  wire         rst,

    input  wire         en,
    input  wire         wnr,
    input  wire [A-1:0] addr,

    output wire [D-1:0] rdata,
    input  wire [D-1:0] wdata
);

`ifdef SIM
    reg [D-1:0] ram [2**A-1:0];
    reg [D-1:0] rdata_q;

    always_ff @(posedge clk)
        if (en & ~wnr)
            rdata_q <= ram[addr];

    always_ff @(posedge clk)
        if (en &  wnr)
            ram[addr] <= wdata;

    assign rdata = rdata_q;

    initial begin
        if (F != "none")
            $readmemh(F, ram);
    end

`else
    localparam T = D * (2 ** A);

    xpm_memory_spram #(
       .ADDR_WIDTH_A        ( A             ),
       .AUTO_SLEEP_TIME     ( 0             ),
       .BYTE_WRITE_WIDTH_A  ( D             ),
       .CASCADE_HEIGHT      ( 0             ),
       .ECC_MODE            ("no_ecc"       ),
       .MEMORY_INIT_FILE    ( F             ),
       .MEMORY_INIT_PARAM   (""             ),
       .MEMORY_OPTIMIZATION ("false"        ),
       .MEMORY_PRIMITIVE    ("block"        ),
       .MEMORY_SIZE         ( T             ),
       .MESSAGE_CONTROL     ( 0             ),
       .READ_DATA_WIDTH_A   ( D             ),
       .READ_LATENCY_A      ( 1             ),
       .READ_RESET_VALUE_A  ("0"            ),
       .RST_MODE_A          ("SYNC"         ),
       .SIM_ASSERT_CHK      ( 0             ),
       .USE_MEM_INIT        ( 1             ),
       .WAKEUP_TIME         ("disable_sleep"),
       .WRITE_DATA_WIDTH_A  ( D             ),
       .WRITE_MODE_A        ("read_first"   ))
    u_ram (
       .dbiterra            (               ),
       .douta               ( rdata         ),
       .sbiterra            (               ),
       .addra               ( addr          ),
       .clka                ( clk           ),
       .dina                ( wdata         ),
       .ena                 ( en            ),
       .injectdbiterra      ( 1'b0          ),
       .injectsbiterra      ( 1'b0          ),
       .regcea              ( 1'b1          ),
       .rsta                ( rst           ),
       .sleep               ( 1'b0          ),
       .wea                 ( wnr           ));
`endif

endmodule
