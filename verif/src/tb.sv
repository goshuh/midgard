class tb_base extends verif::object;

    localparam vaBits  =  64;
    localparam maBits  =  64;
    localparam paBits  =  48;

    localparam vlbWays =  64;
    localparam llcWays =  8;
    localparam memWays =  4;

    // derived
    localparam vpnBits =  vaBits - 12;
    localparam vcnBits =  vaBits - 6;
    localparam vdnBits =  vaBits - 3;

    localparam mpnBits =  maBits - 12;
    localparam mcnBits =  maBits - 6;
    localparam mdnBits =  maBits - 3;

    localparam ppnBits =  paBits - 12;
    localparam pcnBits =  paBits - 6;
    localparam pdnBits =  paBits - 3;

    localparam ptwLvl  = (mpnBits + (9      - 1)) / 9;
    localparam ptwTop  =  mpnBits - (ptwLvl - 1)  * 9;
    localparam ptwEnd  = (maBits  - ptwTop) <= paBits ? 0 : (maBits - ptwTop - paBits) / 9 + 1;

    localparam vlbIdx  = $clog2(vlbWays);
    localparam llcIdx  = $clog2(llcWays);
    localparam memIdx  = $clog2(memWays);

    localparam btOrder =  4;
    localparam padBits =  1023 -  btOrder      * (vpnBits * 3 + 6)
                               - (btOrder + 1) *  mcnBits;

    // tb usage
    localparam TO_MAX  =  10000;

    typedef bit [vpnBits-1:0] vpn_t;
    typedef bit [vcnBits-1:0] vcn_t;
    typedef bit [vdnBits-1:0] vdn_t;

    typedef bit [mpnBits-1:0] mpn_t;
    typedef bit [mcnBits-1:0] mcn_t;
    typedef bit [mdnBits-1:0] mdn_t;

    typedef bit [ppnBits-1:0] ppn_t;
    typedef bit [pcnBits-1:0] pcn_t;
    typedef bit [pdnBits-1:0] pdn_t;

    typedef bit [vlbIdx -1:0] vlb_t;
    typedef bit [llcIdx -1:0] llc_t;
    typedef bit [memIdx -1:0] mem_t;

    typedef bit [padBits-1:0] pad_t;

endclass