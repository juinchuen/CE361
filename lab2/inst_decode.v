module inst_decode(
    // input from fetch
    ins_i,
    funct7_i,
    funct3_i,
    rs1_i,
    rs2_i,
    rd_i,
    opcode_i,
    imm_I_i,
    imm_S_i,
    imm_SB_i,
    imm_U_i,
    imm_UJ_i,

    // write enable signals
    wr_en_reg_i,
    wr_en_mem_i,

    // register file signals
    rs1_addr,
    rs2_addr,
    rs1_data,
    rs2_data,

    //outputs
    ins_o,
    funct7_o,
    funct3_o,
    rs1_o,
    rs2_o,
    rd_o,
    opcode_o,
    imm_I_o,
    imm_S_o,
    imm_SB_o,
    imm_U_o,
    imm_UJ_o,
    wr_en_reg_o,
    wr_en_mem_o,
    rs1_data,
    rs2_data,    

    // control signals
    clk,
    rstb,
    halt
);

    input [31:0] ins_i;
    input [6:0]  funct7_i;
    input [2:0]  funct3_i;
    input [4:0]  rs1_i, rs2_i, rd_i;
    input [6:0]  opcode_i;
    input [31:0] imm_I_i;
    input [31:0] imm_S_i;
    input [31:0] imm_SB_i;
    input [31:0] imm_U_i;
    input [31:0] imm_UJ_i;

    // write enable signals
    input wr_en_reg_i,
    input wr_en_mem_i,

    // register file signals
    output [4:0] rs1_addr,
    output [4:0] rs2_addr,
    input [31:0] rs1_data,
    input [31:0] rs2_data,

    //outputs
    output [31:0] ins_o             ;
    output [6:0]  funct7_o          ;
    output [2:0]  funct3_o          ;
    output [4:0]  rs1_o, rs2_o, rd_o;
    output [6:0]  opcode_o          ;
    output [31:0] imm_I_o           ;
    output [31:0] imm_S_o           ;
    output [31:0] imm_SB_o          ;
    output [31:0] imm_U_o           ;
    output [31:0] imm_UJ_o          ;
    output        wr_en_reg_o       ;
    output        wr_en_mem_o       ;
    output [31:0] rs1_data_o        ;
    output [31:0] rs2_data_o        ;    

    // control signals
    input clk;
    input rstb;
    input halt;

    assign rs1_addr = rs1_i;
    assign rs2_addr = rs2_i;

    always @ (negedge clk or negedge rst) begin

        if (!rst) begin

            ins_o       <= 32'h0;
            funct7_o    <= 7'h0;
            funct3_o    <= 3'h0;
            rs1_o       <= 5'h0;
            rs2_o       <= 5'h0;
            rd_o        <= 5'h0;
            opcode_o    <= 7'h0;
            imm_I_o     <= 32'h0;
            imm_S_o     <= 32'h0;
            imm_SB_o    <= 32'h0;
            imm_U_o     <= 32'h0;
            imm_UJ_o    <= 32'h0;
            wr_en_reg_o <= 0;
            wr_en_mem_o <= 0;
            rs1_data_o  <= 32'h0;
            rs2_data_o  <= 32'h0;

        end else begin

            ins_o       <= ins_i;
            funct7_o    <= funct7_i;
            funct3_o    <= funct3_i;
            rs1_o       <= rs1_i;
            rs2_o       <= rs2_i;
            rd_o        <= rd_i;
            opcode_o    <= opcode_i;
            imm_I_o     <= imm_I_i;
            imm_S_o     <= imm_S_i;
            imm_SB_o    <= imm_SB_i;
            imm_U_o     <= imm_U_i;
            imm_UJ_o    <= imm_UJ_i;
            wr_en_reg_o <= wr_en_reg_i;
            wr_en_mem_o <= wr_en_mem_i;
            rs1_data_o  <= rs1_data;
            rs2_data_o  <= rs2_data;

        end

    end


endmodule

