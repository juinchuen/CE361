// defines 6 modules that can be instantiated to parse instruction fields of RISC-V instructions.

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

    assign imm_SB[12:0]   = {ins[31], ins[7], ins[30:25], ins[11:8], 1'b0;};
    assign imm_SB[31:13]  = ins[31] ? 19'h7FFFF : 19'h00000;

    assign imm_U = {ins[31:12], 12'b0};   

    assign imm_UJ[20:0] = {ins[31], ins[19:12], ins[20], ins[30:21], 1'b0};
    assign imm_UJ[31:21] = ins[31] ? 11'b11111111111 : 11b'00000000000;

endmodule