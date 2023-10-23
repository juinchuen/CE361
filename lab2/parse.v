// defines 6 modules that can be instantiated to parse instruction fields of RISC-V instructions.

module Parse (  ins,
                funct7,
                rs2, 
                rs1, 
                funct3, 
                rd, 
                opcode, 
                imm12_I, 
                imm12_S, 
                imm12_SB, 
                imm20_U, 
                imm20_UJ);

    input [31:0] ins;

    output [6:0] funct7;
    output [4:0] rs1, rs2, rd;
    output [2:0] funct3;
    output [6:0] opcode;

    output [11:0] imm12_I, imm12_S;
    output [12:0] imm13_SB;
    output [31:0] imm32_U;
    output [19:0] imm21_UJ;

    assign funct7 = ins[31:25];
    assign funct3 = ins[14:12];

    assign rs1 = ins[19:15];
    assign rs2 = ins[24:20];
    assign rd  = ins[11:7];

    assign opcode = ins[6:0];

    assign imm12_I = ins[31:20];

    assign imm12_S[ 4:0] = ins[11:7];
    assign imm12_S[11:5] = ins[31:25];

    assign imm13_SB[12]   = ins[31];
    assign imm13_SB[10:5] = ins[30:25];
    assign imm13_SB[ 4:1] = ins[11: 8];
    assign imm13_SB[11]   = ins[7];
    assign imm13_SB[0]    = 1'b0;

    assign imm32_U[31:12] = ins[31:12];
    assign imm32_U[11: 0] = 12'b0;

    assign imm21_UJ[20]    = ins[31];
    assign imm21_UJ[10:1]  = ins[30:21];
    assign imm21_UJ[11]    = ins[20];
    assign imm21_UJ[19:12] = ins[19:12];
    assign imm21_UJ[0]     = 1'b0;

endmodule