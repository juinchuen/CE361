module sc_cpu(halt, clk, rst);

    output halt;
    input clk, rst;



    InstMem im0 (InstAddr, InstSize, InstOut, clk);
    DataMem dm0 (DataAddr, DataSize, DataIn, DataOut, DWEN, clk);
    RegFile rf0 (AddrA, DataOutA, AddrB, DataOutB, AddrW, DataInW, RWEN, clk);

endmodule