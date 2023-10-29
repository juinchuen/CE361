module signed_lt(out, opA, opB);  

   //determines whether opA < opB for signed inputs

   input [31:0] opA, opB;
   output out;

   assign out =   opA[31] ?

                     opB[31] ? opA < opB : 1

                     :

                     opB[31] ? 0 : opA < opB;

endmodule

module branch_flag(branch, halt, funct3, rs1, rs2);

    input [2:0] funct3;
    input [31:0] opA, opB;

    output branch, halt;

    wire beq, bne, blt, bge, bltu, bgeu, slt;

    // if opcode and inequality match, asset flag
    assign beq  = (funct3 == 3'b000) & (opA == opB);
    assign bne  = (funct3 == 3'b001) & (opA != opB);
    assign blt  = (funct3 == 3'b100) & slt;
    assign bge  = (funct3 == 3'b101) & !slt;
    assign bltu = (funct3 == 3'b110) & (opA <  opB);
    assign bgeu = (funct3 == 3'b111) & (opA >= opB);

    assign branch = beq | bne | blt | bge | bltu | bgeu;
    
    // assert halt if unrecognized funct 3
    assign halt = !((funct3 == 3'b000) | 
                    (funct3 == 3'b001) | 
                    (funct3 == 3'b100) | 
                    (funct3 == 3'b101) | 
                    (funct3 == 3'b110) | 
                    (funct3 == 3'b111));

    //signed less than module
    signed_lt slt0 (slt, rs1, rs2)    

endmodule

module load_extend(reg_val, mem_val, funct3);

    output [31:0] reg_val;
    input [31:0] mem_val;
    input [2:0] funct3;

    assign reg_val =    funct3[2] ? 
                            funct3[1] ?
                                funct3[0] ? 32'b0 //111
                                : 
                                            32'b0 //110
                            :
                                funct3[0] ? {16'b0, mem_val[15:0]} //101, LHU
                                : 
                                            {24'b0, mem_val[7:0]} //100, LBU
                        :
                            funct3[1] ?
                                funct3[0] ? 32'b0 //011
                                : 
                                            mem_val //010, LW
                            :
                                funct3[0] ? {16{mem_val[15]}, mem_val[15:0]} //001, LH
                                : 
                                            {24{mem_val[7], mem_val[7:0]}} //000, LB

endmodule

module register_write(out, imm_U, mem_val, PC, opcode, funct3);

    output [31:0] out;

    input [31:0] imm_U, mem_val, PC;
    input [6:0] opcode;
    input [2:0] funct3;

    wire [31:0] out_ui, out_load, out_jal;

    assign out =    ((opcode == 7'b0110111) | (opcode == 7'b0010111))   ? out_ui    : //upper immediate
                    ((opcode == 7'b1101111) | (opcode == 7'b1100111))   ? PC + 4    : //JAL and JALR
                    (opcode == 7'b0000011)                              ? out_load  : //loads
                    (opcode == 7'b0010011)                              ? out_ari_i : //immediate arithmetic
                    (opcode == 7'b0110011)                              ? out_ari   : //arithmetic

    uppper_imm  ui0 (out_ui, imm_U, PC, opcode); //LUI and AUIPC
    load_extend le0 (out_load, mem_val, funct3); //loads
    ari_imm     ai0 ()

endmodule

module uppper_imm(out, imm_U, PC, opcode);

    output [31:0] out;

    input [31:0] imm_U, PC;
    input [6:0] opcode;

    assign out =    (opcode == 7'b0110111) ? 
                        imm_U
                    :
                        (opcode == 7'b0010111) ?
                            PC + imm_U
                        :
                            7'b0;

endmodule

module sc_cpu(halt, clk, rst);

    output halt;
    input clk, rst;


    //internal registers
    reg [31:0] PC; //aka program counter
    wire [31:0] DataAddr;
    reg [4:0] AddrWR;
    reg DWEN, RWEN, halt;

    //instruction parse wires
    wire [6:0] funct7;
    wire [4:0] rs1, rs2, rd;
    wire [2:0] funct3;
    wire [6:0] opcode;
    wire [31:0] imm_I, imm_S, imm_SB, imm_U, imm_UJ;
    wire halt_branch, branch;

    //data wires/reg
    wire [31:0] InstOut, DataOutM, DataRS1, DataRS2;
    reg [31:0] DataInM, DataInR;

    //wires for load
    wire [31:0] loadVal;
    wire [31:0] DataAddr;
    assign DataAddr = DataRS1 + imm_I;

    //wires for JALR
    wire [31:0] JALR_add_rs1_immI;
    assign JALR_add_rs1_immI = imm_I + DataRS1;

    always @ (posedge clk or posedge rst) begin
        
        if (rst) begin

            //rst, set everything to zero
            PC <= 0;

            AddrWR <= 0;

            DWEN <= 0;
            RWEN <= 0;

            halt <= 0;

            DataInM <= 0;
            DataInR <= 0;

        end
        
        else if (halt) //breaks computer if halt asserted
        
        else begin

            DWEN <= 0;
            RWEN <= 0;

            case (opcode)

                7'b0110111: begin //LUI

                                DataInR <= imm_U;
                                AddrWR  <= rd;
                                RWEN    <= 1;
                                PC      <= PC + 4;

                            end

                7'b0010111: begin //AUIPC

                                DataInR <= imm_U;
                                AddrWR  <= rd;
                                RWEN    <= 1;
                                PC      <= PC + imm_U;

                            end

                7'b1101111: begin //JAL

                                DataInR <= PC + 4;
                                AddrWR  <= rd;
                                RWEN    <= 1;

                                PC      <= PC + imm_UJ;

                            end

                7'b1100111: begin //JALR 

                                halt    <= (funct3 == 3'b000) ? 0 : 1;

                                DataInR <= PC + 4;
                                AddrWR  <= rd;
                                RWEN    <= 1;

                                PC      <= {JALR_add_rs1_immI[31:1], 1'b0};

                            end

                7'b1100011: begin //BRANCH

                                halt    <= halt_branch;

                                PC      <= branch ? PC + imm_SB : PC + 4;

                            end

                7'b0000011: begin //LOAD

                                RWEN <= 1;

                                DataInR <= loadVal;

                            end




            endcase

        end

    end

    

    InstMem     im0 (PC, InstSize, InstOut, clk);
    DataMem     dm0 (DataAddr, {1'b0, funct3[1:0]}, DataInM, DataOutM, DWEN, clk);
    RegFile     rf0 (rs1, DataRS1, rs2, DataRS2, AddrWR, DataInR, RWEN, clk);
    Parse       p0  (InstOut, funct7, rs2, rs1, funct3, rd, opcode, imm_I, imm_S, imm_SB, imm_U, imm_UJ);
    branch_flag bf0 (branch, halt_branch, funct3, rs1, rs2);
    load_extend le0 (loadVal, DataOutM, funct3);


endmodule