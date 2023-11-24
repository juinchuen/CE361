`include "lib_lab2.v"

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

    assign imm_SB = {{19{ins[31]}}, ins[31], ins[7], ins[30:25], ins[11:8], 0};
    

    assign imm_U = {ins[31:12], 12'b0};   

    assign imm_UJ[20:0] = {ins[31], ins[19:12], ins[20], ins[30:21], 0};
    assign imm_UJ[31:21] = ins[31] ? 111111111111 : 100000000000;

endmodule

module branch(branch_target_addr, branch_flag, halt, funct3, opA, opB, PC, imm_SB);

    // checks for branching conditions and calculates branch target address, asserts halt if unrecognized funct3
    output branch_flag, halt;
    output [31:0] branch_target_addr;

    input [2:0] funct3;
    input [31:0] opA, opB;

    wire beq, bne, blt, bge, bltu, bgeu, slt;

    // if opcode and inequality match, assert flag
    assign beq  = (funct3 == 3'b000) & (opA == opB);
    assign bne  = (funct3 == 3'b001) & (opA != opB);
    assign blt  = (funct3 == 3'b100) & slt;
    assign bge  = (funct3 == 3'b101) & !slt;
    assign bltu = (funct3 == 3'b110) & (opA <  opB);
    assign bgeu = (funct3 == 3'b111) & (opA >= opB);

    assign branch_flag = beq || bne || blt || bge || bltu || bgeu;

    // target address for branch
    assign branch_target_addr = PC + imm_SB;
    
    // assert halt if unrecognized funct 3
    assign halt = (funct3 == 3'b010) || (funct3 == 3'b011);

    //signed less than module
    signed_lt slt0 (.out(slt), .opA(opA), .opB(opB));   

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
                                            {30, out_slti} //010, SLTI
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

module jump(out, halt, PC_next, PC, DataRS1, imm_I, imm_UJ, func3, opcode);
    //supports JAL and JALR

    output [31:0] out;
    output halt;
    output [31:0] PC_next;

    input [31:0] PC, DataRS1, imm_I;

    // For JAL, PC is incemented by imm_I during instruction fetch stage but we do it here just for consistency
    assign PC_next =    (opcode == 7'b1101111) ? 
                            PC + imm_UJ 
                    :
                        (opcode == 7'b1100111) ?
                            DataRS1 + imm_I
                    :
                            32'b0;

    assign out =  PC + 4;

    assign halt =  !(((opcode == 7'b1100111) & (func3 != 3'b000)) ||
                    (opcode == 7'b1101111))

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

module execute(ex_out, RWEN, DWEN, branch_flag, branch_or_jump_target_addr, load_store_effective_addr, Rd, halt, clk, PC, DataRS1, DataRS2, Rd_decode, imm_I, imm_S, imm_SB, imm_U, imm_UJ, opcode, funct3, funct7);
    
    output reg [31:0] ex_out;
    output reg RWEN, DWEN;
    output branch_flag; // this is from the instruction fetch stage
    output reg [31:0] branch_or_jump_target_addr; // this is from the instruction fetch stage
    output reg [31:0] load_store_effective_addr;
    output reg [4:0] Rd;
    output halt;

    input clk;
    input [31:0] PC, DataRS1, DataRS2, Rd, imm_I, imm_S, imm_SB, imm_U, imm_UJ;
    input [4:0] Rd_decode;
    input [6:0] opcode;
    input [2:0] funct3;
    input [6:0] funct7;

    wire [31:0] out_upper_imm, out_ari_imm, out_arithmetic, out_jump, branch_target_addr;
    wire halt_ari_imm, halt_arithmetic, halt_branch, halt_jump, halt_effective_addr;
    wire branch_flag_internal;
    wire [31:0] jalr_PC_next;
    wire [31:0] load_store_effective_addr_internal;

    assign halt =   (opcode == 7'b0110011)                          ? halt_arithmetic       : //arithmetic
                    (opcode == 7'b0010011)                          ? halt_ari_imm          : //immediate arithmetic
                    (opcode == 7'b0000011 || opcode == 7'b0100011)  ? halt_effective_addr   : //loads, stores effective address
                    (opcode == 7'b1100011)                          ? halt_branch           : //branch
                    (opcode == 7'b1100111)                          ? halt_jump             : //JAL, JALR
                    (opcode == 7'b0110111 || opcode == 7'b0010111)  ? 0                     : //LUI, AUIPC (can't halt)
                    1;                                                                        //unrecognized opcode


    always @ (negedge clk) begin

        if (halt) begin //breaks computer if halt asserted
        end

        else if (opcode == 7'b0110011) begin // Arithmetic
            ex_out <= out_arithmetic;
            Rd <= Rd_decode;
            RWEN <= 0; // neg assert
            DWEN <= 1;
            branch_flag <= 0;
            branch_or_jump_target_addr <= 32'b0;
            load_store_effective_addr <= 32'b0;
        end
        else if (opcode == 7'b0010011) begin // Immediate Arithmetic
            ex_out <= out_ari_imm;
            Rd <= Rd_decode;
            RWEN <= 0;
            DWEN <= 1;
            branch_flag <= 0;
            branch_or_jump_target_addr <= 32'b0;
            load_store_effective_addr <= 32'b0;
        end
        else if (opcode == 7'b0000011) begin // Load
            ex_out <= 32'b0;
            Rd <= Rd_decode;
            RWEN <= 1; // This will be neg asserted in the next cycle (mem stage)
            DWEN <= 1;
            branch_flag <= 0;
            branch_or_jump_target_addr <= 32'b0;
            load_store_effective_addr <= load_store_effective_addr_internal;
        end
        else if (opcode == 7'b0100011) begin // Store
            ex_out <= 32'b0;
            Rd <= Rd_decode;
            RWEN <= 1;
            DWEN <= 0; // Enable data memory write
            branch_flag <= 0;
            branch_or_jump_target_addr <= 32'b0;
            load_store_effective_addr <= load_store_effective_addr_internal;
        end
        else if (opcode == 7'b1100011) begin // Branch
            ex_out <= 32'b0;
            Rd <= Rd_decode;
            RWEN <= 1;
            DWEN <= 1;
            branch_flag <= branch_flag_internal;
            branch_or_jump_target_addr <= branch_target_addr;
            load_store_effective_addr <= 32'b0;
        end
        else if (opcode == 7'b1101111) begin // JAL
            ex_out <= out_jump;
            Rd <= Rd_decode;
            RWEN <= 0;
            DWEN <= 1;
            branch_flag <= 0;
            branch_or_jump_target_addr <= 32'b0;
            load_store_effective_addr <= 32'b0;
        end
        else if (opcode == 7'b1100111) begin // JALR
            ex_out <= out_jump;
            Rd <= Rd_decode;
            RWEN <= 0;
            DWEN <= 1;
            branch_flag <= 1;
            branch_or_jump_target_addr <= jalr_PC_next;
            load_store_effective_addr <= 32'b0;
        end
        else if (opcode == 7'b0110111 || opcode == 7'b0010111) begin // LUI or AUIPC
            ex_out <= out_upper_imm;
            Rd <= Rd_decode;
            RWEN <= 0;
            DWEN <= 1;
            branch_flag <= 0;
            branch_or_jump_target_addr <= 32'b0;
            load_store_effective_addr <= 32'b0;
        end
        else begin // Invalid opcode
            ex_out <= 32'b0;
            Rd <= 0;
            RWEN <= 1;
            DWEN <= 1;
            branch_flag <= 0;
            branch_or_jump_target_addr <= 32'b0;
            load_store_effective_addr <= 32'b0;
        end
    end

    upper_imm       ui0 (.out(out_upper_imm), .imm_U(imm_U), .PC(PC), .opcode(opcode)); //upper immediate
    ari_imm         ai0 (.out(out_ari_imm), .halt(halt_ari_imm), .imm_I(imm_I), .DataRS1(DataRS1), .funct3(funct3)); //immediate arithmetic
    arithmetic      ar0 (.out(out_arithmetic), .halt(halt_arithmetic), .DataRS1(DataRS1), .DataRS2(DataRS2), .funct3(funct3), .funct7(funct7)); //arithmetic
    branch          br0 (.branch_target_addr(branch_target_addr), .branch_flag(branch_flag_internal), .halt(halt_branch), .funct3(funct3), .opA(DataRS1), .opB(DataRS2)); //branching
    jump            jp0 (.out(out_jump), .halt(halt_jump), .PC(PC), .PC_next(jalr_PC_next), .DataRS1(DataRS1), .imm_I(imm_I), .imm_UJ(imm_UJ), .func3(funct3), .opcode(opcode)); //jumping
    effective_addr  ea0 (.EffectiveDataAddr(load_store_effective_addr_internal), .halt(halt_effective_addr), .DataRS1(DataRS1), .imm_S(imm_S), .imm_I(imm_I), .opcode(opcode), .func3(funct3)); //effective address calculation

endmodule

module memory_access(mem_out, RWEN, PC, Rd, halt, clk, ex_out, Rd_ex, RWEN_ex, DWEN, effective_addr, DataRS2, func3, func7, opcode, );

    output reg [31:0] mem_out;
    output reg RWEN;
    // output reg [31:0] PC;
    output reg [4:0] Rd;
    output halt;

    input clk;
    input [31:0] ex_out;
    input [4:0] Rd_ex;
    input RWEN_ex, DWEN;
    input [31:0] effective_addr, DataRS2;
    input [2:0] func3;
    input [6:0] func7;
    input [6:0] opcode;
    
    wire [31:0] DataOutM;
    wire [31:0] out_load, out_store;

    wire halt_load, halt_store;
    wire DWEN_store;

    wire [1:0] DataSize;
    assign DataSize = funct3[1:0];

    assign DWEN_store = DWEN || halt_store;

    assign halt = halt_load || halt_store;

    always @ (negedge clk) begin
        if (halt) begin //breaks computer if halt asserted
        end
        else begin
            // PC <= branch_flag ? branch_or_jump_target_addr : PC + 4; // PC is incremented by 4 unless branch or jump -> implement this in instruction fetch stage
            Rd <= Rd_ex; // Rd is passed through from execute stage

            if (opcode == 7'b0000011) begin // Load
                mem_out <= halt_load ? 32'b0 : out_load;
                RWEN <= halt_load ? 1 : 0; // neg assert
            end
            else if (opcode == 7'b0100011) begin // Store
                mem_out <= 32'b0;
                RWEN <= 1; // no write back for stores
            end
            else begin // Other instructions (no memory access). Just pass the data through from execute stage
                mem_out <= ex_out;
                RWEN <= RWEN_ex;
            end
        end
        else begin // Invalid opcode
            out <= 32'b0;
        end
    end

    DataMem         DMEM    (.Addr(effective_addr), .Size(DataSize), .DataIn(out_store), .DataOut(DataOutM), .WEN(DWEN_store), .CLK(clk)); //data memory
    load_extend     le0     (.out(out_load), .halt(halt_load), .mem_val(DataOutM), .funct3(func3)); //load extension
    store_extend    se0     (.out(out_store), .halt(halt_store), .DataRS2(DataRS2), .funct3(func3)); //store extension

endmodule

module reg_write_back(DataInRd, RWEN, Rd, mem_out, RWEN_ex, Rd_mem);

    // write back to register file
    // all outputs are from the instruction decode stage
    output [31:0] DataInRd;
    output RWEN;     
    output [4:0] Rd; 

    input [31:0] mem_out;
    input RWEN_ex;
    input [4:0] Rd_mem;

    assign DataInRd = (RWEN_ex == 0) ? mem_out : 32'b0;
    assign RWEN = RWEN_ex;
    assign Rd = Rd_mem;

endmodule

module PipelinedCPU(halt, clk, rst);
    output halt;
    input clk, rst;
endmodule