// Forwarding implemented in InstructionDecode and Execute stages - check comments in those modules for more details

`include "lib_lab2.v"

`define SIZE_WORD  2'b10

module Parse (  inst_data,
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
                imm_UJ,
                RWEN,
                DWEN,
                MEMREAD,
                BRANCH_OR_JUMP
                );

    input [31:0] inst_data;

    output [6:0] funct7;
    output [4:0] rs1, rs2, rd;
    output [2:0] funct3;
    output [6:0] opcode;

    output [31:0] imm_I, imm_S;
    output [31:0] imm_SB;
    output [31:0] imm_U;
    output [31:0] imm_UJ;

    output RWEN, DWEN, MEMREAD, BRANCH_OR_JUMP;

    assign funct7 = inst_data[31:25];
    assign funct3 = inst_data[14:12];

    assign rs1 = inst_data[19:15];
    assign rs2 = inst_data[24:20];
    assign rd  = inst_data[11:7];

    assign opcode = inst_data[6:0];

    assign imm_I[11:0] = inst_data[31:20];
    assign imm_I[31:12] = inst_data[31] ? 20'hFFFFF : 20'h00000;

    assign imm_S[11:0] = {inst_data[31:25],inst_data[11:7]};
    assign imm_S[31:12] = inst_data[31] ? 20'hFFFFF : 20'h00000;

    assign imm_SB = {{19{inst_data[31]}}, inst_data[31], inst_data[7], inst_data[30:25], inst_data[11:8], 1'b0};
    

    assign imm_U = {inst_data[31:12], 12'b0};   

    assign imm_UJ[20:0] = {inst_data[31], inst_data[19:12], inst_data[20], inst_data[30:21], 1'b0};
    assign imm_UJ[31:21] = inst_data[31] ? 11'b11111111111 : 11'b00000000000;

    assign RWEN =     !((opcode == 7'b0110111) || //LUI
                        (opcode == 7'b0010111) || //AUIPC
                        (opcode == 7'b1101111) || //JAL
                        (opcode == 7'b1100111) || //JALR
                        (opcode == 7'b0000011) || //LOAD
                        (opcode == 7'b0010011) || //arith imm
                        (opcode == 7'b0110011)); //arith
                        
    assign DWEN =      !(opcode == 7'b0100011); // STORE
    assign MEMREAD =    (opcode == 7'b0000011); //LOAD

    assign BRANCH_OR_JUMP =  (opcode == 7'b1101111) || //JAL
                            (opcode == 7'b1100111) || //JALR
                            (opcode == 7'b1100011); //BRANCH

endmodule

module branch(branch_target_addr, branch_flag, halt, funct3, opA, opB, PC, imm_SB);

    // checks for branching conditions and calculates branch target address, asserts halt if unrecognized funct3
    output branch_flag, halt;
    output [31:0] branch_target_addr;

    input [2:0] funct3;
    input [31:0] opA, opB;
    input [31:0] PC, imm_SB;

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

module jump(out, halt, PC_next, PC, DataRS1, imm_I, imm_UJ, funct3, opcode);
    //supports JAL and JALR

    output [31:0] out;
    output halt;
    output [31:0] PC_next;

    input [31:0] PC, DataRS1, imm_I, imm_UJ;
    input [2:0] funct3;
    input [6:0] opcode;

    // For JAL, PC is incemented by imm_I and for JALR, PC is set to DataRS1 + imm_I
    assign PC_next =    (opcode == 7'b1101111) ? 
                            PC + imm_UJ 
                    :
                        (opcode == 7'b1100111) ?
                            DataRS1 + imm_I
                    :
                            32'b0;

    assign out =  PC + 4;

    assign halt =  !(((opcode == 7'b1100111) && (funct3 == 3'b000)) ||
                    (opcode == 7'b1101111));

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

module effective_addr(EffectiveDataAddr, halt, DataRS1, imm_S, imm_I, opcode, funct3);
    // calculate effective address for loads and stores
    // asserts halt if address is not appropriately aligned for loads and stores

    output [31:0] EffectiveDataAddr;
    output halt;

    input [31:0] DataRS1, imm_S, imm_I;
    input [6:0] opcode;
    input [2:0] funct3;

    wire halt_alignment;

    assign EffectiveDataAddr =   (opcode == 7'b0100011) ? DataRS1 + imm_S : //stores
                                 (opcode == 7'b0000011) ? DataRS1 + imm_I: //loads
                                 32'b0;
    
    
    assign halt_alignment = (funct3[1:0] == 2'b00) ? 1'b0 : // byte aligned
                            (funct3[1:0] == 2'b01) ? (EffectiveDataAddr[0] != 1'b0) : // half word aligned
                            (funct3[1:0] == 2'b10) ? (EffectiveDataAddr[1:0] != 2'b00) : // word aligned
                            1'b0; // other cases, don't halt

    assign halt =   (opcode == 7'b0100011 || opcode == 7'b0000011) ? halt_alignment : //stores and loads
                    1'b0; //other cases, don't halt

endmodule

module InstructionFetch(IF_InstData, PC, IF_PC, InstWord, clk, rst, halt_EX, halt_MEM, branch_flag, branch_or_jump_target_addr, IF_stall);
    // fetches instruction from instruction MEMory

    output [31:0] IF_InstData;
    output [31:0] PC; 
    output [31:0] IF_PC;

    // Internal Registers
    reg [31:0] IF_InstData_reg;
    reg [31:0] PC_reg;
    reg [31:0] IF_PC_reg;

    input [31:0] InstWord;
    input clk;
    input rst;
    input halt_EX, halt_MEM;
    input branch_flag;
    input [31:0] branch_or_jump_target_addr;

    input IF_stall; // load stall signal from ID stage

    // Register assignments to output ports
    assign IF_InstData = IF_InstData_reg;
    assign PC = PC_reg;
    assign IF_PC = IF_PC_reg;

    always @ (negedge clk or negedge rst) begin
        if (!rst) begin
            PC_reg <= 0;
        end

        else if (halt_EX || halt_MEM) begin //breaks computer if halt asserted
        end

        else begin
            if (IF_stall) begin // stall IF stage if load stall to keep on fetching same instruction i.e don't update any registers
            end
            else begin
                IF_InstData_reg <= InstWord;
                IF_PC_reg <= PC;
                if (branch_flag) begin // if branch flag asserted, branch to branch target address
                    PC_reg <= branch_or_jump_target_addr;
                end
                else begin 
                    if (InstWord != 32'h0) begin // if instruction is not 0, increment PC by 4
                        PC_reg <= PC_reg + 4;
                    end
                    else begin
                        PC_reg <= PC_reg; // If instruction is 0, keep PC same because it is end of program and previous instruction will halt the computer
                    end
                end
            end
            end
    end
endmodule

module InstructionDecode(
    IF_InstData, IF_PC, clk, halt_EX, halt_MEM, DataRS1, DataRS2, RF_DataInRd, RF_Rd, RF_WEN, EX_BRANCH_OR_JUMP, MEM_BRANCH_OR_JUMP, RF_rs1, RF_rs2, ID_DataRS1, ID_DataRS2,
    ID_funct7, ID_rs1, ID_rs2, ID_rd, ID_funct3, ID_opcode, ID_imm_I, ID_imm_S, ID_imm_SB, ID_imm_U, ID_imm_UJ, ID_RWEN, ID_DWEN, ID_MEMREAD, ID_BRANCH_OR_JUMP, ID_PC, IF_stall, EX_stall);
    // decodes instruction and calculates control signals

    input [31:0] IF_InstData, IF_PC;
    input clk;
    input halt_EX;
    input halt_MEM;
    input [31:0] DataRS1, DataRS2;
    input [31:0] RF_DataInRd;
    input [4:0] RF_Rd;
    input RF_WEN;
    input EX_BRANCH_OR_JUMP, MEM_BRANCH_OR_JUMP;


    output [4:0] RF_rs1, RF_rs2;
    output [31:0] ID_DataRS1, ID_DataRS2;
    output [6:0] ID_funct7;
    output [4:0] ID_rs1, ID_rs2, ID_rd;
    output [2:0] ID_funct3;
    output [6:0] ID_opcode;
    output [31:0] ID_imm_I, ID_imm_S;
    output [31:0] ID_imm_SB;
    output [31:0] ID_imm_U;
    output [31:0] ID_imm_UJ;
    output ID_RWEN, ID_DWEN, ID_MEMREAD, ID_BRANCH_OR_JUMP;
    output [31:0] ID_PC;
    output IF_stall, EX_stall;

    // Internal Registers
    reg [31:0] ID_DataRS1_reg, ID_DataRS2_reg;
    reg [6:0] ID_funct7_reg;
    reg [4:0] ID_rs1_reg, ID_rs2_reg, ID_rd_reg;
    reg [2:0] ID_funct3_reg;
    reg [6:0] ID_opcode_reg;
    reg [31:0] ID_imm_I_reg, ID_imm_S_reg;
    reg [31:0] ID_imm_SB_reg;
    reg [31:0] ID_imm_U_reg;
    reg [31:0] ID_imm_UJ_reg;
    reg ID_RWEN_reg, ID_DWEN_reg, ID_MEMREAD_reg, ID_BRANCH_OR_JUMP_reg;
    reg [31:0] ID_PC_reg;
    reg EX_stall_reg;

    wire [6:0] funct7;
    wire [4:0] rs1, rs2, rd;
    wire [2:0] funct3;
    wire [6:0] opcode;
    wire [31:0] imm_I, imm_S;
    wire [31:0] imm_SB;
    wire [31:0] imm_U;
    wire [31:0] imm_UJ;
    wire RWEN, DWEN, MEMREAD, BRANCH_OR_JUMP;
    wire load_stall;
    wire first_branch_or_jump_stall;
    wire second_branch_or_jump_stall;
    wire third_branch_or_jump_stall;
    wire branch_or_jump_stall;

    // Register file read logic
    assign RF_rs1 = rs1;
    assign RF_rs2 = rs2; 

    // Forwarding logic
    // DataRS1 and DataRS2 are forwarded from RWB stage as ID and RWB are in the same clock cycle
    wire [31:0] DataRS1_forwarded, DataRS2_forwarded;
    assign DataRS1_forwarded = (!RF_WEN && (RF_Rd != 0) && (RF_Rd == rs1)) ? RF_DataInRd : DataRS1;
    assign DataRS2_forwarded = (!RF_WEN && (RF_Rd != 0) && (RF_Rd == rs2)) ? RF_DataInRd : DataRS2;

    // stall if ID_MEMREAD and ID_rd is not 0 and ID_rd is equal to rs1 or rs2
    assign load_stall = ID_MEMREAD_reg && (ID_rd_reg != 0) && (ID_rd_reg == rs1 || ID_rd_reg == rs2); // stall if EX_MEMREAD and EX_rd is not 0 and EX_rd is equal to rs1 or rs2

    assign first_branch_or_jump_stall = ID_BRANCH_OR_JUMP_reg == 1 ? 1 : 0; // stall if previous instruction is branch or jump
    assign IF_stall = load_stall || first_branch_or_jump_stall; // keep fetching same instruction if load stall or the first stall of branch or jump

    assign second_branch_or_jump_stall = EX_BRANCH_OR_JUMP == 1 ? 1 : 0; // stall if previous instruction is branch or jump
    assign third_branch_or_jump_stall = MEM_BRANCH_OR_JUMP == 1 ? 1 : 0; // stall if previous instruction is branch or jump

    assign branch_or_jump_stall = first_branch_or_jump_stall || second_branch_or_jump_stall || third_branch_or_jump_stall; // stall if previous instruction is branch or jump

    // Register assignments to output ports
    assign ID_DataRS1 = ID_DataRS1_reg;
    assign ID_DataRS2 = ID_DataRS2_reg;
    assign ID_funct7 = ID_funct7_reg;
    assign ID_rs1 = ID_rs1_reg;
    assign ID_rs2 = ID_rs2_reg;
    assign ID_rd = ID_rd_reg;
    assign ID_funct3 = ID_funct3_reg;
    assign ID_opcode = ID_opcode_reg;
    assign ID_imm_I = ID_imm_I_reg;
    assign ID_imm_S = ID_imm_S_reg;
    assign ID_imm_SB = ID_imm_SB_reg;
    assign ID_imm_U = ID_imm_U_reg;
    assign ID_imm_UJ = ID_imm_UJ_reg;
    assign ID_RWEN = ID_RWEN_reg;
    assign ID_DWEN = ID_DWEN_reg;
    assign ID_MEMREAD = ID_MEMREAD_reg;
    assign ID_BRANCH_OR_JUMP = ID_BRANCH_OR_JUMP_reg;
    assign ID_PC = ID_PC_reg;
    assign EX_stall = EX_stall_reg;

    always @(negedge clk) begin
        if (halt_EX || halt_MEM) begin //breaks computer if halt asserted
            ID_RWEN_reg <= 1; // assert RWEN and DWEN to prevent register file and data memory write
            ID_DWEN_reg <= 1;
        end
        else begin 
            if (load_stall || branch_or_jump_stall) begin
                // Make instruction decode stage a bubble
                ID_RWEN_reg <= 1;
                ID_DWEN_reg <= 1;
                ID_MEMREAD_reg <= 0;
                ID_BRANCH_OR_JUMP_reg <= 0;
                ID_DataRS1_reg <= 32'b0;
                ID_DataRS2_reg <= 32'b0;
                ID_funct7_reg <= 7'b0;
                ID_rs1_reg <= 5'b0;
                ID_rs2_reg <= 5'b0;
                ID_rd_reg <= 5'b0;
                ID_funct3_reg <= 3'b0;
                ID_opcode_reg <= 7'b0;
                ID_imm_I_reg <= 32'b0;
                ID_imm_S_reg <= 32'b0;
                ID_imm_SB_reg <= 32'b0;
                ID_imm_U_reg <= 32'b0;
                ID_imm_UJ_reg <= 32'b0;
                ID_PC_reg <= 32'b0;
                EX_stall_reg <= 1;
            end

            else begin
                ID_RWEN_reg <= RWEN;
                ID_DWEN_reg <= DWEN;
                ID_MEMREAD_reg <= MEMREAD;
                ID_BRANCH_OR_JUMP_reg <= BRANCH_OR_JUMP;
                ID_DataRS1_reg <= DataRS1_forwarded;
                ID_DataRS2_reg <= DataRS2_forwarded;
                ID_funct7_reg <= funct7;
                ID_rs1_reg <= rs1;
                ID_rs2_reg <= rs2;
                ID_rd_reg <= rd;
                ID_funct3_reg <= funct3;
                ID_opcode_reg <= opcode;
                ID_imm_I_reg <= imm_I;
                ID_imm_S_reg <= imm_S;
                ID_imm_SB_reg <= imm_SB;
                ID_imm_U_reg <= imm_U;
                ID_imm_UJ_reg <= imm_UJ;
                ID_PC_reg <= IF_PC;
                EX_stall_reg <= 0;
            end
        end
    end

    Parse p0 (      .inst_data(IF_InstData),
                    .funct7(funct7),
                    .rs2(rs2), 
                    .rs1(rs1), 
                    .funct3(funct3), 
                    .rd(rd), 
                    .opcode(opcode), 
                    .imm_I(imm_I), 
                    .imm_S(imm_S), 
                    .imm_SB(imm_SB), 
                    .imm_U(imm_U), 
                    .imm_UJ(imm_UJ),
                    .RWEN(RWEN),
                    .DWEN(DWEN),
                    .MEMREAD(MEMREAD),
                    .BRANCH_OR_JUMP(BRANCH_OR_JUMP)
                    );

endmodule


module Execute(EX_out, EX_RWEN, EX_rd, EX_DWEN, branch_flag, branch_or_jump_target_addr, load_store_effective_addr, EX_DataRS2, EX_funct3, EX_opcode, EX_MEMREAD, EX_BRANCH_OR_JUMP,
                halt_EX_backward, halt_EX_forward, halt_MEM_backward, clk, MEM_out, MEM_RWEN, MEM_rd, 
                ID_DataRS1, ID_DataRS2,
                ID_funct7, ID_rs1, ID_rs2, ID_rd, ID_funct3, ID_opcode, ID_imm_I, ID_imm_S, ID_imm_SB, ID_imm_U, ID_imm_UJ, ID_RWEN, ID_DWEN, ID_MEMREAD, ID_BRANCH_OR_JUMP, ID_PC, EX_stall, MEM_stall);
    
    output [31:0] EX_out;
    output EX_RWEN;
    output [4:0] EX_rd;
    output EX_DWEN;
    output branch_flag; // this is from the instruction fetch stage
    output [31:0] branch_or_jump_target_addr; // this is from the instruction fetch stage
    output [31:0] load_store_effective_addr;
    output [31:0] EX_DataRS2;
    output [2:0] EX_funct3;
    output [6:0] EX_opcode;
    output EX_MEMREAD;
    output EX_BRANCH_OR_JUMP;
    output MEM_stall;

    reg [31:0] EX_out_reg;
    reg EX_RWEN_reg;
    reg [4:0] EX_rd_reg;
    reg EX_DWEN_reg;
    reg branch_flag_reg; 
    reg [31:0] branch_or_jump_target_addr_reg; 
    reg [31:0] load_store_effective_addr_reg;
    reg [31:0] EX_DataRS2_reg;
    reg [2:0] EX_funct3_reg;
    reg [6:0] EX_opcode_reg;
    reg EX_MEMREAD_reg;
    reg EX_BRANCH_OR_JUMP_reg;
    reg MEM_stall_reg;
    reg halt_EX_forward_reg;

    output halt_EX_backward;
    output halt_EX_forward;

    input clk;
    input halt_MEM_backward;

    input [31:0] MEM_out;
    input MEM_RWEN;
    input [4:0] MEM_rd;
    input [31:0] ID_DataRS1, ID_DataRS2;
    input [6:0] ID_funct7;
    input [4:0] ID_rs1, ID_rs2, ID_rd;
    input [2:0] ID_funct3;
    input [6:0] ID_opcode;
    input [31:0] ID_imm_I, ID_imm_S;
    input [31:0] ID_imm_SB;
    input [31:0] ID_imm_U;
    input [31:0] ID_imm_UJ;
    input ID_RWEN, ID_DWEN, ID_MEMREAD, ID_BRANCH_OR_JUMP;
    input [31:0] ID_PC;
    input EX_stall;

    wire [31:0] out_upper_imm, out_ari_imm, out_arithmetic, out_jump, branch_target_addr;
    wire halt_ari_imm, halt_arithmetic, halt_branch, halt_jump, halt_effective_addr;
    wire branch_flag_internal;
    wire [31:0] jump_target_addr;
    wire [31:0] load_store_effective_addr_internal;

    wire halt_EX_internal;

    // Forwarding logic
    // DataRS1 and DataRS2 are forwarded from EX stage and MEM stage
    wire [31:0] DataRS1_forwarded, DataRS2_forwarded;

    assign DataRS1_forwarded = (!EX_RWEN_reg && (EX_rd_reg != 0) && (EX_rd_reg == ID_rs1)) ? EX_out : // Execute stage takes precedence over memory in case of forwarding
                                (!MEM_RWEN && (MEM_rd != 0) && (MEM_rd == ID_rs1)) ? MEM_out :
                                ID_DataRS1;
    
    assign DataRS2_forwarded = (!EX_RWEN_reg && (EX_rd_reg != 0) && (EX_rd_reg == ID_rs2)) ? EX_out : // EXecute stage takes precedence over memory in case of forwarding
                                (!MEM_RWEN && (MEM_rd != 0) && (MEM_rd == ID_rs2)) ? MEM_out :
                                ID_DataRS2;

    assign halt_EX_internal =   (ID_opcode == 7'b0110011)                               ? halt_arithmetic       : //arithmetic
                                (ID_opcode == 7'b0010011)                               ? halt_ari_imm          : //immediate arithmetic
                                (ID_opcode == 7'b0000011 || ID_opcode == 7'b0100011)    ? halt_effective_addr   : //loads, stores effective address
                                (ID_opcode == 7'b1100011)                               ? halt_branch           : //branch
                                (ID_opcode == 7'b1101111 || ID_opcode == 7'b1100111)    ? halt_jump             : //JAL, JALR    
                                (ID_opcode == 7'b0110111 || ID_opcode == 7'b0010111)    ? 0                     : //LUI, AUIPC (can't halt)
                                1;                                                                        //unrecognized opcode

    // Halts IF, ID and EX stages as soon as halt asserted and this instruction is being executed i.e it is not a bubble
    assign halt_EX_backward = halt_EX_internal && !EX_stall;

    // Register assignments to output ports
    assign EX_out = EX_out_reg;
    assign EX_RWEN = EX_RWEN_reg;
    assign EX_rd = EX_rd_reg;
    assign EX_DWEN = EX_DWEN_reg;
    assign branch_flag = branch_flag_reg;
    assign branch_or_jump_target_addr = branch_or_jump_target_addr_reg;
    assign load_store_effective_addr = load_store_effective_addr_reg;
    assign EX_DataRS2 = EX_DataRS2_reg;
    assign EX_funct3 = EX_funct3_reg;
    assign EX_opcode = EX_opcode_reg;
    assign EX_MEMREAD = EX_MEMREAD_reg;
    assign EX_BRANCH_OR_JUMP = EX_BRANCH_OR_JUMP_reg;
    assign MEM_stall = MEM_stall_reg;

    // This halt is passed down the pipeline to ensure MA and RWB of previous instruction is completed before halting
    assign halt_EX_forward = halt_EX_forward_reg;

    always @ (negedge clk) begin
        if (EX_stall) begin // stall if EX_stall asserted (load stall) - we check before halt as this instruction should not be executed
            EX_RWEN_reg <= 1; // assert RWEN and DWEN to prevent register file and data memory write
            EX_DWEN_reg <= 1;
            branch_flag_reg <= 0;
            EX_BRANCH_OR_JUMP_reg <= 0;
            MEM_stall_reg <= 1;
        end
        else if (halt_EX_internal || halt_MEM_backward) begin //breaks computer if halt asserted
            EX_RWEN_reg <= 1; // assert RWEN and DWEN to prevent register file and data memory write
            EX_DWEN_reg <= 1;
            EX_BRANCH_OR_JUMP_reg <= 0;
            halt_EX_forward_reg <= 1;
            MEM_stall_reg <= 0;
        end
        else if (ID_opcode == 7'b0110011) begin // Arithmetic
            EX_out_reg <= out_arithmetic;
            EX_rd_reg <= ID_rd;
            EX_RWEN_reg <= ID_RWEN;
            EX_DWEN_reg <= ID_DWEN;
            branch_flag_reg <= 0;
            branch_or_jump_target_addr_reg <= 32'b0;
            load_store_effective_addr_reg <= 32'b0;
            EX_DataRS2_reg <= DataRS2_forwarded;
            EX_funct3_reg <= ID_funct3;
            EX_opcode_reg <= ID_opcode;
            EX_MEMREAD_reg <= ID_MEMREAD;
            EX_BRANCH_OR_JUMP_reg <= ID_BRANCH_OR_JUMP;
            MEM_stall_reg <= 0;
        end
        else if (ID_opcode == 7'b0010011) begin // Immediate Arithmetic
            EX_out_reg <= out_ari_imm;
            EX_rd_reg <= ID_rd;
            EX_RWEN_reg <= ID_RWEN;
            EX_DWEN_reg <= ID_DWEN;
            branch_flag_reg <= 0;
            branch_or_jump_target_addr_reg <= 32'b0;
            load_store_effective_addr_reg <= 32'b0;
            EX_DataRS2_reg <= DataRS2_forwarded;
            EX_funct3_reg <= ID_funct3;
            EX_opcode_reg <= ID_opcode;
            EX_MEMREAD_reg <= ID_MEMREAD;
            EX_BRANCH_OR_JUMP_reg <= ID_BRANCH_OR_JUMP;
            MEM_stall_reg <= 0;
        end
        else if (ID_opcode == 7'b0000011) begin // Load
            EX_out_reg <= 32'b0;
            EX_rd_reg <= ID_rd;
            EX_RWEN_reg <= ID_RWEN;
            EX_DWEN_reg <= ID_DWEN;
            branch_flag_reg <= 0;
            branch_or_jump_target_addr_reg <= 32'b0;
            load_store_effective_addr_reg <= load_store_effective_addr_internal;
            EX_DataRS2_reg <= DataRS2_forwarded;
            EX_funct3_reg <= ID_funct3;
            EX_opcode_reg <= ID_opcode;
            EX_MEMREAD_reg <= ID_MEMREAD;
            EX_BRANCH_OR_JUMP_reg <= ID_BRANCH_OR_JUMP;
            MEM_stall_reg <= 0;
        end
        else if (ID_opcode == 7'b0100011) begin // Store
            EX_out_reg <= 32'b0;
            EX_rd_reg <= ID_rd;
            EX_RWEN_reg <= ID_RWEN;
            EX_DWEN_reg <= ID_DWEN;
            branch_flag_reg <= 0;
            branch_or_jump_target_addr_reg <= 32'b0;
            load_store_effective_addr_reg <= load_store_effective_addr_internal;
            EX_DataRS2_reg <= DataRS2_forwarded;
            EX_funct3_reg <= ID_funct3;
            EX_opcode_reg <= ID_opcode;
            EX_MEMREAD_reg <= ID_MEMREAD;
            EX_BRANCH_OR_JUMP_reg <= ID_BRANCH_OR_JUMP;
            MEM_stall_reg <= 0;
        end
        else if (ID_opcode == 7'b1100011) begin // Branch
            EX_out_reg <= 32'b0;
            EX_rd_reg <= ID_rd;
            EX_RWEN_reg <= ID_RWEN;
            EX_DWEN_reg <= ID_DWEN;
            branch_flag_reg <= 1; // branch flag is always asserted for branch instructions as the branch target address is calculated here
            if (branch_flag_internal) begin // if branch flag asserted, branch to branch target address
                branch_or_jump_target_addr_reg <= branch_target_addr;
            end
            else begin // else branch to next instruction
                branch_or_jump_target_addr_reg <= ID_PC + 4;
            end
            load_store_effective_addr_reg <= 32'b0;
            EX_DataRS2_reg <= DataRS2_forwarded;
            EX_funct3_reg <= ID_funct3;
            EX_opcode_reg <= ID_opcode;
            EX_MEMREAD_reg <= ID_MEMREAD;
            EX_BRANCH_OR_JUMP_reg <= ID_BRANCH_OR_JUMP;
            MEM_stall_reg <= 0;
        end
        else if (ID_opcode == 7'b1101111 || ID_opcode == 7'b1100111) begin // JAL or JALR
            EX_out_reg <= out_jump;
            EX_rd_reg <= ID_rd;
            EX_RWEN_reg <= ID_RWEN;
            EX_DWEN_reg <= ID_DWEN;
            branch_flag_reg <= 1;
            branch_or_jump_target_addr_reg <= jump_target_addr;
            load_store_effective_addr_reg <= 32'b0;
            EX_DataRS2_reg <= DataRS2_forwarded;
            EX_funct3_reg <= ID_funct3;
            EX_opcode_reg <= ID_opcode;
            EX_MEMREAD_reg <= ID_MEMREAD;
            EX_BRANCH_OR_JUMP_reg <= ID_BRANCH_OR_JUMP;
            MEM_stall_reg <= 0;
        end
        else if (ID_opcode == 7'b0110111 || ID_opcode == 7'b0010111) begin // LUI or AUIPC
            EX_out_reg <= out_upper_imm;
            EX_rd_reg <= ID_rd;
            EX_RWEN_reg <= ID_RWEN;
            EX_DWEN_reg <= ID_DWEN;
            branch_flag_reg <= 0;
            branch_or_jump_target_addr_reg <= 32'b0;
            load_store_effective_addr_reg <= 32'b0;
            EX_DataRS2_reg <= DataRS2_forwarded;
            EX_funct3_reg <= ID_funct3;
            EX_opcode_reg <= ID_opcode;
            EX_MEMREAD_reg <= ID_MEMREAD;
            EX_BRANCH_OR_JUMP_reg <= ID_BRANCH_OR_JUMP;
            MEM_stall_reg <= 0;
        end
        else begin // Invalid opcode
            EX_RWEN_reg <= 1; // assert RWEN and DWEN to prevent register file and data memory write
            EX_DWEN_reg <= 1; 
            EX_MEMREAD_reg <= 0;
            EX_BRANCH_OR_JUMP_reg <= 0;
        end
    end

    upper_imm       ui0 (.out(out_upper_imm), .imm_U(ID_imm_U), .PC(ID_PC), .opcode(ID_opcode)); //upper immediate
    ari_imm         ai0 (.out(out_ari_imm), .halt(halt_ari_imm), .imm_I(ID_imm_I), .DataRS1(DataRS1_forwarded), .funct3(ID_funct3)); //immediate arithmetic
    arithmetic      ar0 (.out(out_arithmetic), .halt(halt_arithmetic), .DataRS1(DataRS1_forwarded), .DataRS2(DataRS2_forwarded), .funct3(ID_funct3), .funct7(ID_funct7)); //arithmetic
    branch          br0 (.branch_target_addr(branch_target_addr), .branch_flag(branch_flag_internal), .halt(halt_branch), .funct3(ID_funct3), .opA(DataRS1_forwarded), .opB(DataRS2_forwarded), .PC(ID_PC), .imm_SB(ID_imm_SB)); //branching
    jump            ju0 (.out(out_jump), .halt(halt_jump), .PC_next(jump_target_addr), .PC(ID_PC), .DataRS1(DataRS1_forwarded), .imm_I(ID_imm_I), .imm_UJ(ID_imm_UJ), .funct3(ID_funct3), .opcode(ID_opcode)); //jumping
    effective_addr  ea0 (.EffectiveDataAddr(load_store_effective_addr_internal), .halt(halt_effective_addr), .DataRS1(DataRS1_forwarded), .imm_S(ID_imm_S), .imm_I(ID_imm_I), .opcode(ID_opcode), .funct3(ID_funct3)); //effective address

endmodule

module MemoryAccess(MEM_out, MEM_RWEN, MEM_rd, MEM_BRANCH_OR_JUMP, DataAddr, DataSize, DWEN, DataInM, DataOutM, halt_MEM_backward, halt_MEM_forward, clk, EX_out, EX_rd, EX_RWEN, EX_DWEN, load_store_effective_addr, EX_DataRS2, EX_funct3, EX_opcode, EX_BRANCH_OR_JUMP, halt_EX_forward, MEM_stall);

    output [31:0] MEM_out;
    output MEM_RWEN;
    output [4:0] MEM_rd;
    output MEM_BRANCH_OR_JUMP;

    reg [31:0] MEM_out_reg;
    reg MEM_RWEN_reg;
    reg [4:0] MEM_rd_reg;
    reg MEM_BRANCH_OR_JUMP_reg;

    output [31:0] DataAddr;
    output [1:0] DataSize;
    output DWEN;
    output [31:0] DataInM;
    
    input [31:0] DataOutM;

    output halt_MEM_backward;
    output halt_MEM_forward;

    reg halt_MEM_forward_reg;

    input clk;

    input [31:0] EX_out;
    input [4:0] EX_rd;
    input EX_RWEN, EX_DWEN;
    input [31:0] load_store_effective_addr, EX_DataRS2;
    input [2:0] EX_funct3;
    input [6:0] EX_opcode;
    input EX_BRANCH_OR_JUMP;
    input halt_EX_forward;
    input MEM_stall;
    
    wire [31:0] out_load, out_store;

    wire halt_load, halt_store;
    wire DWEN_store;

    wire halt_MEM_opcode, halt_MEM_internal;

    assign halt_MEM_opcode = EX_opcode == 7'b0000011 ? halt_load : //load
                             EX_opcode == 7'b0100011 ? halt_store : //store
                             0; //other cases, don't halt

    assign halt_MEM_internal = halt_MEM_opcode || halt_EX_forward; // halt asserted in this stage or in the previous stage

    assign halt_MEM_backward = halt_MEM_internal && !MEM_stall; // halt asserted in this stage and this instruction is being executed i.e it is not a bubble

    // Assignments to output ports
    assign MEM_out = MEM_out_reg;
    assign MEM_RWEN = MEM_RWEN_reg;
    assign MEM_rd = MEM_rd_reg;

    assign MEM_BRANCH_OR_JUMP = MEM_BRANCH_OR_JUMP_reg;

    assign DataAddr = load_store_effective_addr;
    assign DataSize = EX_funct3[1:0];
    assign DWEN = EX_DWEN || halt_store;
    assign halt_MEM_forward = halt_MEM_forward_reg; // halt is passed down the pipeline to ensure RWB of previous instruction is completed before halting
    assign DataInM = out_store;

    always @ (negedge clk) begin
        if (MEM_stall) begin // stall if MEM_stall asserted (load stall) - we check before halt as this instruction should not be executed
            MEM_RWEN_reg <= 1; // assert RWEN to prevent register file write
            MEM_BRANCH_OR_JUMP_reg <= 0;
        end
        else if (halt_MEM_internal) begin //breaks computer if halt asserted
            MEM_RWEN_reg <= 1; // assert RWEN to prevent register file write
            halt_MEM_forward_reg <= 1; // pass halt forward
            MEM_BRANCH_OR_JUMP_reg <= 0;
        end
        else begin
            MEM_rd_reg <= EX_rd; // rd is passed through from Execute stage
            MEM_BRANCH_OR_JUMP_reg <= EX_BRANCH_OR_JUMP; // branch or jump flag is passed through from Execute stage

            if (EX_opcode == 7'b0000011) begin // Load
                MEM_out_reg <= halt_load ? 32'b0 : out_load;
                MEM_RWEN_reg <= halt_load ? 1 : 0; // neg assert
            end
            else if (EX_opcode == 7'b0100011) begin // Store
                MEM_out_reg <= 32'b0;
                MEM_RWEN_reg <= 1; // no write back for stores
            end
            else begin // Other instructions (no memory access). Just pass the data through from EXecute stage
                MEM_out_reg <= EX_out;
                MEM_RWEN_reg <= EX_RWEN;
            end
        end
    end

    load_extend     le0     (.out(out_load), .halt(halt_load), .mem_val(DataOutM), .funct3(EX_funct3)); //load extension
    store_extend    se0     (.out(out_store), .halt(halt_store), .DataRS2(EX_DataRS2), .funct3(EX_funct3)); //store extension

endmodule

module RegisterWriteBack(DataInRd, RF_WEN, rd, MEM_out, MEM_RWEN, MEM_rd, halt_MEM_forward, halt_overall, clk);
    // write back to register file

    output [31:0] DataInRd;
    output RF_WEN;     
    output [4:0] rd; 
    output halt_overall;

    reg halt_overall_reg;


    input [31:0] MEM_out;
    input MEM_RWEN;
    input [4:0] MEM_rd;
    input halt_MEM_forward;
    input clk;

    assign DataInRd = (MEM_RWEN == 0) ? MEM_out : 32'b0;
    assign RF_WEN = MEM_RWEN;
    assign rd = MEM_rd;

    assign halt_overall = halt_overall_reg; // if there was a halt in any of the previous stages, halt the computer now for real

    always @(negedge clk) begin
        if (halt_MEM_forward) begin //breaks computer if halt asserted
            halt_overall_reg <= 1;
        end
        else begin
            halt_overall_reg <= 0;
        end
    end

endmodule

module PipelinedCPU(halt, clk, rst);
    output halt;
    input clk, rst;

    wire [31:0] PC;

    wire[4:0] rs1, rs2, rd;
    
    //data wires
    wire [31:0] InstWord;
    wire [31:0] DataAddr, DataInM, DataOutM;
    wire [31:0] DataRS1, DataRS2, DataInRd;
    wire [1:0] DataSize;

    wire DWEN, RF_WEN;

    // Instruction fetch wires
    wire [31:0] IF_InstData;
    wire [31:0] IF_PC;
    wire load_stall;

    // Instruction decode wires
    wire [6:0] ID_funct7;
    wire [4:0] ID_rs1, ID_rs2, ID_rd;
    wire [2:0] ID_funct3;
    wire [6:0] ID_opcode;
    wire [31:0] ID_imm_I, ID_imm_S;
    wire [31:0] ID_imm_SB;
    wire [31:0] ID_imm_U;
    wire [31:0] ID_imm_UJ;
    wire ID_RWEN, ID_DWEN, ID_MEMREAD, ID_BRANCH_OR_JUMP;
    wire [31:0] ID_PC;
    wire [31:0] ID_DataRS1, ID_DataRS2;

    // Execute wires
    wire [31:0] EX_out;
    wire EX_RWEN;
    wire [4:0] EX_rd;
    wire EX_DWEN;
    wire branch_flag;
    wire [31:0] branch_or_jump_target_addr;
    wire [31:0] load_store_effective_addr;
    wire [31:0] EX_DataRS2;
    wire [2:0] EX_funct3;
    wire [6:0] EX_opcode;
    wire EX_MEMREAD;
    wire EX_BRANCH_OR_JUMP;

    // Memory access wires
    wire [31:0] MEM_out;
    wire MEM_RWEN;
    wire [4:0] MEM_rd;
    wire MEM_BRANCH_OR_JUMP;

    // stall wires
    wire IF_stall, EX_stall, MEM_stall;

    // halt wires
    wire halt_EX_backward, halt_EX_forward;
    wire halt_MEM_backward, halt_MEM_forward;
    wire halt_overall;

    assign halt = halt_overall;


    InstMem         IMEM    (.Addr(PC), .Size(`SIZE_WORD), .DataOut(InstWord), .CLK(clk)); //instruction memory
    DataMem         DMEM    (.Addr(DataAddr), .Size(DataSize), .DataIn(DataInM), .DataOut(DataOutM), .WEN(DWEN), .CLK(clk)); //data memory
    RegFile         RF      (.AddrA(rs1), .DataOutA(DataRS1), .AddrB(rs2), .DataOutB(DataRS2), .AddrW(rd), .DataInW(DataInRd), .WenW(RF_WEN), .CLK(clk)); //register file


    // Pipeline stages

    // Instruction fetch
    InstructionFetch  IF     (.IF_InstData(IF_InstData), .PC(PC), .IF_PC(IF_PC), .InstWord(InstWord), .clk(clk), .rst(rst), 
                             .halt_EX(halt_EX_backward), .halt_MEM(halt_MEM_backward), .branch_flag(branch_flag), .branch_or_jump_target_addr(branch_or_jump_target_addr), 
                             .IF_stall(IF_stall)); //instruction fetch

    // Instruction decode/ register read
    InstructionDecode ID    (.IF_InstData(IF_InstData), .IF_PC(IF_PC), .clk(clk), .halt_EX(halt_EX_backward), .halt_MEM(halt_MEM_backward), .DataRS1(DataRS1), .DataRS2(DataRS2), .RF_DataInRd(DataInRd), .RF_Rd(rd), .RF_WEN(RF_WEN), .EX_BRANCH_OR_JUMP(EX_BRANCH_OR_JUMP), 
                            .MEM_BRANCH_OR_JUMP(MEM_BRANCH_OR_JUMP), .RF_rs1(rs1), .RF_rs2(rs2), .ID_DataRS1(ID_DataRS1), .ID_DataRS2(ID_DataRS2),
                            .ID_funct7(ID_funct7), .ID_rs1(ID_rs1), .ID_rs2(ID_rs2), .ID_rd(ID_rd), .ID_funct3(ID_funct3), .ID_opcode(ID_opcode), .ID_imm_I(ID_imm_I), 
                            .ID_imm_S(ID_imm_S), .ID_imm_SB(ID_imm_SB), .ID_imm_U(ID_imm_U), .ID_imm_UJ(ID_imm_UJ), .ID_RWEN(ID_RWEN), .ID_DWEN(ID_DWEN), .ID_MEMREAD(ID_MEMREAD), .ID_BRANCH_OR_JUMP(ID_BRANCH_OR_JUMP), .ID_PC(ID_PC), .IF_stall(IF_stall), .EX_stall(EX_stall)); //instruction decode
    // Execute
    Execute           EX    (.EX_out(EX_out), .EX_RWEN(EX_RWEN), .EX_rd(EX_rd), .EX_DWEN(EX_DWEN), .branch_flag(branch_flag), .branch_or_jump_target_addr(branch_or_jump_target_addr), .load_store_effective_addr(load_store_effective_addr), .EX_DataRS2(EX_DataRS2), .EX_funct3(EX_funct3), .EX_opcode(EX_opcode), 
                            .EX_MEMREAD(EX_MEMREAD), .EX_BRANCH_OR_JUMP(EX_BRANCH_OR_JUMP),
                             .halt_EX_backward(halt_EX_backward), .halt_EX_forward(halt_EX_forward), .halt_MEM_backward(halt_MEM_backward), .clk(clk), .MEM_out(MEM_out), .MEM_RWEN(MEM_RWEN), .MEM_rd(MEM_rd), .ID_DataRS1(ID_DataRS1), .ID_DataRS2(ID_DataRS2),
                             .ID_funct7(ID_funct7), .ID_rs1(ID_rs1), .ID_rs2(ID_rs2), .ID_rd(ID_rd), .ID_funct3(ID_funct3), .ID_opcode(ID_opcode), .ID_imm_I(ID_imm_I), 
                             .ID_imm_S(ID_imm_S), .ID_imm_SB(ID_imm_SB), .ID_imm_U(ID_imm_U), .ID_imm_UJ(ID_imm_UJ), .ID_RWEN(ID_RWEN), .ID_DWEN(ID_DWEN), .ID_MEMREAD(ID_MEMREAD), .ID_BRANCH_OR_JUMP(ID_BRANCH_OR_JUMP), .ID_PC(ID_PC), .EX_stall(EX_stall), .MEM_stall(MEM_stall)); //execute
    // memory access
    MemoryAccess      MA    (.MEM_out(MEM_out), .MEM_RWEN(MEM_RWEN), .MEM_rd(MEM_rd), .MEM_BRANCH_OR_JUMP(MEM_BRANCH_OR_JUMP), .DataAddr(DataAddr), .DataSize(DataSize), .DWEN(DWEN), .DataInM(DataInM), 
                            .DataOutM(DataOutM), .halt_MEM_backward(halt_MEM_backward), .halt_MEM_forward(halt_MEM_forward), .clk(clk), .EX_out(EX_out), .EX_rd(EX_rd), .EX_RWEN(EX_RWEN), .EX_DWEN(EX_DWEN),
                             .load_store_effective_addr(load_store_effective_addr), .EX_DataRS2(EX_DataRS2), .EX_funct3(EX_funct3), .EX_opcode(EX_opcode), .EX_BRANCH_OR_JUMP(EX_BRANCH_OR_JUMP), .halt_EX_forward(halt_EX_forward), .MEM_stall(MEM_stall)); //memory access

    // Register write back
    RegisterWriteBack  RWB    (.DataInRd(DataInRd), .RF_WEN(RF_WEN), .rd(rd), .MEM_out(MEM_out), .MEM_RWEN(MEM_RWEN), .MEM_rd(MEM_rd), .halt_MEM_forward(halt_MEM_forward), .halt_overall(halt_overall), .clk(clk)); //register write back
    
endmodule