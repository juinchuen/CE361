module sc_cpu(halt, clk, rst);

    output halt;
    input clk, rst;

    reg [15:0] InstAddr; //aka program counter

    always @ (posedge clk or posedge rst) begin
        
        if (rst) begin

            //rst, set everything to zero
            InstAddr <= 0;

        end else begin

            case (opcode)

                

            endcase

        end

    end

    InstMem im0 (InstAddr, InstSize, InstOut, clk);
    DataMem dm0 (DataAddr, DataSize, DataIn, DataOut, DWEN, clk);
    RegFile rf0 (AddrA, DataOutA, AddrB, DataOutB, AddrW, DataInW, RWEN, clk);

endmodule