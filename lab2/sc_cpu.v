`include "lib_lab2.v"

`define SIZE_WORD  2'b10

module Parse (  ins,
                funct7,
                rs2, 
                rs1, 
                funct3, 
                rd, 
                opcode, 
                imm_I, 
                imm_S, 
                imm_SB, 
                imm_U, 
                imm_UJ);

    input [31:0] ins;

    output [6:0] funct7;
    output [4:0] rs1, rs2, rd;
    output [2:0] funct3;
    output [6:0] opcode;

    output [31:0] imm_I, imm_S;
    output [31:0] imm_SB;
    output [31:0] imm_U;
    output [31:0] imm_UJ;

    assign funct7 = ins[31:25];
    assign funct3 = ins[14:12];

    assign rs1 = ins[19:15];
    assign rs2 = ins[24:20];
    assign rd  = ins[11:7];

    assign opcode = ins[6:0];

    assign imm_I[11:0] = ins[31:20];
    assign imm_I[31:12] = ins[31] ? 20'hFFFFF : 20'h00000;

    assign imm_S[11:0] = {ins[31:25],ins[11:7]};
    assign imm_S[31:12] = ins[31] ? 20'hFFFFF : 20'h00000;

    assign imm_SB = {{19{ins[31]}}, ins[31], ins[7], ins[30:25], ins[11:8], 1'b0};
    

    assign imm_U = {ins[31:12], 12'b0};   

    assign imm_UJ[20:0] = {ins[31], ins[19:12], ins[20], ins[30:21], 1'b0};
    assign imm_UJ[31:21] = ins[31] ? 11'b11111111111 : 11'b00000000000;

endmodule

module signed_lt(out, opA, opB);  

   //determines whether opA < opB for signed inputs

   input [31:0] opA, opB;
   output out;

   assign out =   opA[31] ?

                     opB[31] ? opA < opB : 1

                     :

                     opB[31] ? 0 : opA < opB;

endmodule

module branch_flag(branch, halt, funct3, opA, opB);

    // checks for branching conditions, asserts halt if funct3 is invalid

    input [2:0] funct3;
    input [31:0] opA, opB;

    output branch, halt;

    wire beq, bne, blt, bge, bltu, bgeu, slt;

    // if opcode and inequality match, assert flag
    assign beq  = (funct3 == 3'b000) & (opA == opB);
    assign bne  = (funct3 == 3'b001) & (opA != opB);
    assign blt  = (funct3 == 3'b100) & slt;
    assign bge  = (funct3 == 3'b101) & !slt;
    assign bltu = (funct3 == 3'b110) & (opA <  opB);
    assign bgeu = (funct3 == 3'b111) & (opA >= opB);

    assign branch = beq || bne || blt || bge || bltu || bgeu;
    
    // assert halt if unrecognized funct 3
    assign halt = (funct3 == 3'b010) || (funct3 == 3'b011);

    //signed less than module
    signed_lt slt0 (.out(slt), .opA(opA), .opB(opB));   

endmodule

module load_extend(out, halt, mem_val, funct3);
    //performs extension on memory value before register store
    //asserts halt if unrecognized funct3

    output [31:0] out;
    output halt;

    input [31:0] mem_val;
    input [2:0] funct3;

    assign out =    funct3[2] ? 
                        funct3[1] ?
                            funct3[0] ? 32'b0 //111
                            : 
                                        32'b0 //110
                        :
                            funct3[0] ? {16'h0000, mem_val[15:0]} //101, LHU
                            : 
                                        {24'h000000, mem_val[7:0]} //100, LBU
                    :
                        funct3[1] ?
                            funct3[0] ? 32'b0 //011
                            : 
                                        mem_val //010, LW
                        :
                            funct3[0] ? {{16{mem_val[15]}}, mem_val[15:0]} //001, LH
                            : 
                                    {{24{mem_val[7]}}, mem_val[7:0]}; //000, LB

    assign halt =   (funct3 == 3'b011) ||
                    (funct3 == 3'b110) ||
                    (funct3 == 3'b111) ;

                    
endmodule

module store_extend(out, halt, DataRS2, funct3);
    //performs sign extension on register value before store
    //asserts halt if unrecognized funct3

    output [31:0] out;
    output halt;

    input [31:0] DataRS2;
    input [2:0] funct3;

    assign out = funct3[2] ?
                    funct3[1] ?
                        funct3[0] ? 32'b0 //111
                        :
                            32'b0 //110
                    :
                        32'b0 //101 and 100
                :   
                    funct3[1] ?
                        funct3[0] ? 32'b0 //011
                        :
                            DataRS2 //010, SW
                    :
                        funct3[0] ? {{16{DataRS2[15]}}, DataRS2[15:0]} //001, SH
                        :
                            {{24{DataRS2[7]}}, DataRS2[7:0]}; //000, SB

    assign halt =   !((funct3 == 3'b000) ||
                      (funct3 == 3'b001) ||
                      (funct3 == 3'b010)) ;

endmodule

module effective_addr(EffectiveDataAddr, halt, DataRS1, imm_S, imm_I, opcode, func3);
    // calculate effective address for loads and stores
    // asserts halt if address is not appropriately aligned for loads and stores

    output [31:0] EffectiveDataAddr;
    output halt;

    input [31:0] DataRS1, imm_S, imm_I;
    input [6:0] opcode;
    input [2:0] func3;

    wire halt_alignment;

    assign EffectiveDataAddr =   (opcode == 7'b0100011) ? DataRS1 + imm_S : //stores
                                 (opcode == 7'b0000011) ? DataRS1 + imm_I: //loads
                                 32'b0;
    
    
    assign halt_alignment = (func3[1:0] == 2'b00) ? 1'b0 : // byte aligned
                            (func3[1:0] == 2'b01) ? (EffectiveDataAddr[0] != 1'b0) : // half word aligned
                            (func3[1:0] == 2'b10) ? (EffectiveDataAddr[1:0] != 2'b00) : // word aligned
                            1'b0; // other cases, don't halt

    assign halt =   (opcode == 7'b0100011 || opcode == 7'b0000011) ? halt_alignment : //stores and loads
                    1'b0; //other cases, don't halt

endmodule

module register_write(DataInRd, RWEN, DataAddr, DWEN, DataInM, halt, PC_next, imm_I, imm_S, imm_SB, imm_U, imm_UJ, DataOutM, PC_curr, opcode, funct3, funct7, DataRS1, DataRS2);
    //combinational module to generate what is written to reg file

    output [31:0]   DataInRd;           //data to be written to register file
    output          RWEN;               //register write enable signal
    output [31:0]   DataAddr, DataInM;  //address and data to be written to memory
    output          DWEN;               //data write enable signal
    output          halt;
    output [31:0]   PC_next;            //next PC value

    input [31:0] imm_I, imm_S, imm_SB, imm_U, imm_UJ;
    input [31:0] PC_curr;
    input [6:0] opcode;
    input [2:0] funct3;
    input [6:0] funct7;
    input [31:0] DataRS1, DataRS2;
    input [31:0] DataOutM;

    wire [31:0] out_ui, out_load, out_ari_i, out_ari, out_store;
    wire halt_load, halt_store, halt_ari_i, halt_ari, halt_branch, halt_effective_addr;
    wire [31:0] JALR_add_rs1_immI;
    wire [31:0] EffectiveDataAddr;

    assign JALR_add_rs1_immI = DataRS1 + imm_I;

    assign DataInRd =   ((opcode == 7'b0110111) || (opcode == 7'b0010111))   ? out_ui    : //upper immediate
                        ((opcode == 7'b1101111) || (opcode == 7'b1100111))   ? PC_curr + 4    : //JAL and JALR
                        (opcode == 7'b0000011)                               ? out_load  : //loads
                        (opcode == 7'b0010011)                               ? out_ari_i : //immediate arithmetic
                        (opcode == 7'b0110011)                               ? out_ari   : //arithmetic
                        32'b0;

    assign halt =   
                    (opcode == 7'b1100111)  ? (funct3 != 3'b000)                    : //JALR
                    (opcode == 7'b1101111)  ? 0                                     : //JAL
                    (opcode == 7'b0000011)  ? halt_load  || halt_effective_addr   : //loads
                    (opcode == 7'b0100011)  ? halt_store || halt_effective_addr   : //stores
                    (opcode == 7'b0010011)  ? halt_ari_i                            : //immediate arithmetic
                    (opcode == 7'b0110011)  ? halt_ari                              : //arithmetic
                    (opcode == 7'b0110111)  ? 0                                     : //LUI
                    (opcode == 7'b0010111)  ? 0                                     : //AUIPC
                    (opcode == 7'b1100011)  ? halt_branch                          : //branch
                    1;                                             //unrecognized opcode
    

    // RWEN is neg assert
    assign RWEN = !(((opcode == 7'b0110111) || (opcode == 7'b0010111))      ? !halt : //upper immediate
                    ((opcode == 7'b1101111) || (opcode == 7'b1100111))      ? !halt : //JAL and JALR
                    (opcode == 7'b0000011)                                  ? !halt : //loads
                    (opcode == 7'b0010011)                                  ? !halt : //immediate arithmetic
                    (opcode == 7'b0110011)                                  ? !halt : //arithmetic
                    0); 
    
    assign DataAddr = EffectiveDataAddr; //address to be read from memory or written to memory

    // DWEN is neg assert
    assign DWEN = !((opcode == 7'b0100011) ? !halt : //stores
                    0); 

    assign DataInM = ((opcode == 7'b0100011) ? out_store : //stores
                     32'b0); 
    
    upper_imm       ui0 (.out(out_ui), .imm_U(imm_U), .PC(PC_curr), .opcode(opcode)); //upper immediate
    load_extend     le0 (.out(out_load), .halt(halt_load), .mem_val(DataOutM), .funct3(funct3)); //loads
    store_extend    se0 (.out(out_store), .halt(halt_store), .DataRS2(DataRS2), .funct3(funct3)); //stores
    ari_imm         ai0 (.out(out_ari_i), .halt(halt_ari_i), .imm_I(imm_I), .DataRS1(DataRS1), .funct3(funct3)); //immediate arithmetic
    arithmetic      ar0 (.out(out_ari), .halt(halt_ari), .DataRS1(DataRS1), .DataRS2(DataRS2), .funct3(funct3), .funct7(funct7)); //arithmetic
    effective_addr  ea0 (.EffectiveDataAddr(EffectiveDataAddr), .halt(halt_effective_addr), .DataRS1(DataRS1), .imm_S(imm_S), .imm_I(imm_I), .opcode(opcode), .func3(funct3)); //effective address
    pc_update       pc_up0 (.out(PC_next), .halt_branch(halt_branch), .PC(PC_curr), .imm_SB(imm_SB), .imm_UJ(imm_UJ), .JALR_add_rs1_immI(JALR_add_rs1_immI), .opcode(opcode), .funct3(funct3), .rs1(DataRS1), .rs2(DataRS2)); //PC update

endmodule

module ari_imm(out, halt, imm_I, DataRS1, funct3);

    //implements immediate arithmetic

    output [31:0] out;
    output halt;

    input [31:0] imm_I, DataRS1;
    input [2:0] funct3;

    wire out_slti;
    wire [31:0] out_srai;

    assign out =    funct3[2] ? 
                            funct3[1] ?
                                funct3[0] ? DataRS1 & imm_I //111, ANDI
                                : 
                                            DataRS1 | imm_I //110, ORI
                            :
                                funct3[0] ?     (imm_I[11:5] == 7'b0000000) ? DataRS1 >> imm_I[4:0] //101, SRLI
                                                : 
                                                                        out_srai //101, SRAI
                                : 
                                            DataRS1 ^ imm_I //100, XORI
                        :
                            funct3[1] ?
                                funct3[0] ? 32'b0 //011, not valid
                                : 
                                            {31'b0, out_slti} //010, SLTI
                            :
                                funct3[0] ? DataRS1 << imm_I[4:0] //001, SLLI
                                : 
                                            DataRS1 + imm_I ; //000, ADDI

    assign halt =   (funct3 == 3'b011) ||
                    ((funct3 == 3'b001) & (imm_I[11:5] != 7'b0000000)) ||
                    ((funct3 == 3'b101) & ({imm_I[11], imm_I[9:5]} != 6'b000000));
                    
    signed_lt               slt0 (.out(out_slti), .opA(DataRS1), .opB(imm_I)); //signed less than module
    arithmetic_right_shift  ars0 (.opA(DataRS1), .opB(imm_I), .out(out_srai)); //arithmetic right shift module

endmodule

module arithmetic_right_shift (opA, opB, out);

   input [31:0] opA, opB;
   output [31:0] out;

   assign out = opA[31] ?   ((opA >> opB[4:0]) | (~(32'hffffffff >> opB[4:0])))
                : 
                            (opA >> opB[4:0]);

endmodule

module arithmetic(out, halt, DataRS1, DataRS2, funct3, funct7);
    input [31:0] DataRS1, DataRS2;
    input [2:0] funct3;
    input [6:0] funct7;
    output [31:0] out;
    output halt;
    
    wire out_slt;
    wire [31:0] out_sra;
    
    assign out = funct3[2] ?
                        funct3[1] ?
                            funct3[0] ? DataRS1 & DataRS2 //111, AND
                            :
                                DataRS1 | DataRS2 //110, OR
                        :
                            funct3[0] ? 
                                funct7 == 7'b0000000 ? DataRS1 >> DataRS2[4:0] //101, SRL
                                :
                                    out_sra//101, SRA
                            :
                                DataRS1 ^ DataRS2 //100, XOR
                    : 
                        funct3[1] ?
                            funct3[0] ? DataRS1 < DataRS2 //011, SLTU
                            :
                                out_slt //010, SLT
                        :
                            funct3[0] ? DataRS1 << DataRS2[4:0] //001, SLL
                            :
                                funct7 == 7'b0000000 ? DataRS1 + DataRS2 //000, ADD
                                :
                                    DataRS1 - DataRS2 ; //000, SUB

    assign halt =   ((funct3 == 3'b111) & (funct7 != 7'b0000000)) ||
                    (funct3 == 3'b110 & (funct7 != 7'b0000000)) || 
                    ((funct3 == 3'b101) & {funct7[6], funct7[4:0]} != 6'b000000) ||
                    ((funct3 == 3'b100) & (funct7 != 7'b0000000)) || 
                    ((funct3 == 3'b011) & (funct7 != 7'b0000000)) || 
                    ((funct3 == 3'b010) & (funct7 != 7'b0000000)) || 
                    ((funct3 == 3'b001) & (funct7 != 7'b0000000)) || 
                    ((funct3 == 3'b000) & {funct7[6], funct7[4:0]} != 6'b000000) ;

    signed_lt               slt0 (.out(out_slt), .opA(DataRS1), .opB(DataRS2)); //signed less than module
    arithmetic_right_shift  ars0 (.opA(DataRS1), .opB(DataRS2), .out(out_sra)); //arithmetic right shift module

endmodule

module upper_imm(out, imm_U, PC, opcode);

    //supports LUI and AUIPC 

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

module pc_update(out, halt_branch, PC, imm_SB, imm_UJ, JALR_add_rs1_immI, opcode, funct3, rs1, rs2);
    
    //updates PC based on opcode

    output [31:0] out;
    output halt_branch;

    input [6:0] opcode;
    input [2:0] funct3;
    input [31:0] PC, rs1, rs2;
    input [31:0] imm_SB, imm_UJ, JALR_add_rs1_immI;

    wire branch;

    assign out =    (opcode == 7'b1101111) ? PC + imm_UJ : // JAL
                    (opcode == 7'b1100111) ? PC + {JALR_add_rs1_immI[31:1], 1'b0} : // JALR
                    (opcode == 7'b1100011) ?
                            branch ? 
                                PC + imm_SB // BRANCH if branch flag asserted
                            :
                                PC + 4 // DO NOT BRANCH if branch flag asserted
                        :
                            (opcode == 7'b0110011 || opcode == 7'b0010011 || opcode == 7'b0000011 || 7'b0110111 || 7'b0010111 || 7'b0100011) ?
                                PC + 4 // INCREMENT PC for arithmetic, load, store, auipc, and lui
                            :
                                PC ; // DO NOT INCREMENT PC if opcode not recognized

    // assign halt_opcodes = !((opcode == 7'b1101111) || //JAL
    //                         (opcode == 7'b1100111) || //JALR
    //                         (opcode == 7'b1100011) || //branch
    //                         (opcode == 7'b0110011) || //arithmetic
    //                         (opcode == 7'b0010011) || //immediate arithmetic
    //                         (opcode == 7'b0000011) || //loads
    //                         (opcode == 7'b0100011) || //stores
    //                         (opcode == 7'b0010111) || //AUIPC
    //                         (opcode == 7'b0110111));  //LUI 
    
    // assign halt = (opcode == 7'b1100011) ? halt_branch : //branch
    //               halt_opcodes; //other cases

    branch_flag bf0 (.branch(branch), .halt(halt_branch), .funct3(funct3), .opA(rs1), .opB(rs2)); //branch flag module                   
endmodule



module SingleCycleCPU(halt, clk, rst);

    output halt;
    input clk, rst;

    //internal registers
    reg [31:0] PC; //aka program counter 
    wire [31:0] PC_next; // next PC value

    //instruction parse wires
    wire [6:0] funct7;
    wire [4:0] rs1, rs2, rd;
    wire [2:0] funct3;
    wire [6:0] opcode;
    wire [31:0] imm_I, imm_S, imm_SB, imm_U, imm_UJ;

    //data wires/reg
    wire [31:0] InstWord;
    wire [31:0] DataAddr, DataInM, DataOutM;
    wire [31:0]  DataRS1, DataRS2, DataInRd;
    wire DWEN, RWEN;
    wire halt_trial;

    wire [1:0] DataSize;
    assign DataSize = funct3[1:0];

    always @ (negedge clk or negedge rst) begin
        if (!rst) begin
            //rst, set everything to zero
            PC <= 0;

            // DWEN <= 0;
            // RWEN <= 0;

            // halt <= 0;
            
            // DataAddr <= 0;
            // DataInM <= 0;
            // DataInRd <= 0;

        end
        
        else if (halt) begin //breaks computer if halt asserted
        end
        
        else begin

            PC <= PC_next;

        end

    end

    InstMem         IMEM    (.Addr(PC), .Size(`SIZE_WORD), .DataOut(InstWord), .CLK(clk)); //instruction memory
    Parse           p0      (.ins(InstWord), .funct7(funct7), .rs2(rs2), .rs1(rs1), .funct3(funct3), .rd(rd), .opcode(opcode), .imm_I(imm_I), .imm_S(imm_S), .imm_SB(imm_SB), .imm_U(imm_U), .imm_UJ(imm_UJ)); //parses instruction
    DataMem         DMEM    (.Addr(DataAddr), .Size(DataSize), .DataIn(DataInM), .DataOut(DataOutM), .WEN(DWEN), .CLK(clk)); //data memory
    RegFile         RF      (.AddrA(rs1), .DataOutA(DataRS1), .AddrB(rs2), .DataOutB(DataRS2), .AddrW(rd), .DataInW(DataInRd), .WenW(RWEN), .CLK(clk)); //register file
    register_write  rw0     (.DataInRd(DataInRd), .RWEN(RWEN), .DataAddr(DataAddr), .DWEN(DWEN), .DataInM(DataInM), .halt(halt), .PC_next(PC_next), .imm_I(imm_I), .imm_S(imm_S), .imm_SB(imm_SB), .imm_U(imm_U), .imm_UJ(imm_UJ), .DataOutM(DataOutM), .PC_curr(PC), .opcode(opcode), .funct3(funct3), .funct7(funct7), .DataRS1(DataRS1), .DataRS2(DataRS2)); //register write

endmodule