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
                imm_UJ,
                wr_en_reg,
                wr_en_mem,
                read_rs1,
                read_rs2);

    input [31:0] ins;

    output [6:0] funct7;
    output [4:0] rs1, rs2, rd;
    output [2:0] funct3;
    output [6:0] opcode;

    output [31:0] imm_I, imm_S;
    output [31:0] imm_SB;
    output [31:0] imm_U;
    output [31:0] imm_UJ;

    output wr_en_reg, wr_en_mem;
    output read_rs1, read_rs2;

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

    assign wr_en_reg =  (opcode == 7'b0110111) || //LUI
                        (opcode == 7'b0010111) || //AUIPC
                        (opcode == 7'b1101111) || //JAL
                        (opcode == 7'b1100111) || //JALR
                        (opcode == 7'b0000011) || //LOAD
                        (opcode == 7'b0010011) || //arith imm
                        (opcode == 7'b0110011); //arith
                        
    assign wr_en_mem =  (opcode == 7'b0100011);

    assign read_rs1 =   (opcode == 7'b1100111) || //JALR
                        (opcode == 7'b1100011) || //BRANCH
                        (opcode == 7'b0000011) || //LOAD
                        (opcode == 7'b0100011) || //STORE
                        (opcode == 7'b0010011) || //ARI IMM
                        (opcode == 7'b0110011);   //ARI

    assign read_rs2 =   (opcode == 7'b1100011) || //BRANCH
                        (opcode == 7'b0110011);   //ARI

endmodule

module reg_lock(rs1_safe, rs2_safe, wr_en, rd, rs1, rs2, opcode, clk, rst);

    output rs1_safe, rs2_safe;

    input wr_en;
    input [4:0] rs1, rs2, rd;
    input [6:0] opcode;

    input clk, rst;

    // locks for each register
    reg [1:0] locks [31:0];

    assign rs1_safe = locks[rs1] == 2'h0;
    assign rs2_safe = locks[rs2] == 2'h0;

    always @ (negedge clock or negedge rst) begin

        if (!rst) begin

            for (int i = 0; i < 32; i++) locks[i] <= 2'h0;

        end else begin

            for (int i = 0; i < 32; i++) begin
                
                locks[i] <= (locks[i] == 2'h0)      ? 2'h0 :
                            ((i == rd) && wr_en)    ? 2'h3 :
                            locks[i] - 1;

            end

        end

    end

endmodule

module inst_fetch(

    // instruction memory ports
    inst_addr,
    inst_data,

    // instruction parse ports
    ins,
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

    // ex write back ports
    result_ex,
    branch_ex,

    // register and memory writes
    wr_en_reg,
    wr_en_mem,

    // control signals
    clk, 
    rstb,
    halt

);

    // instruction memory ports
    output [31:0] inst_addr, PC_pass;
    input  [31:0] inst_data;

    // instruction parse ports and wires
    output [ 6:0] funct7;
    output [ 4:0] rs1, rs2, rd;
    output [ 2:0] funct3;
    output [ 6:0] opcode;
    output [31:0] imm_I, imm_S, imm_SB, imm_U, imm_UJ;

    wire [31:0] ins_c;
    wire [ 6:0] funct7_c;
    wire [ 4:0] rs1_c, rs2_c, rd_c;
    wire [ 2:0] funct3_c;
    wire [ 6:0] opcode_c;
    wire [31:0] imm_I_c, imm_S_c, imm_SB_c, imm_U_c, imm_UJ_c;

    // ex write back ports
    input [31:0] result_ex;
    input branch_ex;

    // register and memory writes
    output wr_en_reg;
    output wr_en_mem;

    // control signals
    input clk;
    input rstb;
    input halt;

    // reg lock wires
    wire rs1_safe
    wire rs2_safe
    wire wr_mem_op, wr_reg_op;
    wire read_rs1, read_rs2;

    // FSM variables
    reg [1:0] state
    reg [1:0] stall_counter;

    always @ (negedge clk or negedge rst) begin

        if (!rst) begin

            inst_addr   <= 32'h0;
            ins         <= 32'h0;
            funct7      <= 7'h0;
            rs1         <= 5'h0;
            rs2         <= 5'h0;
            rd          <= 5'h0;
            funct3      <= 3'h0;
            opcode      <= 7'h0;
            imm_I       <= 32'h0;
            imm_S       <= 32'h0;
            imm_SB      <= 32'h0;
            imm_U       <= 32'h0;
            imm_UJ      <= 32'h0;
            wr_en_reg   <= 0;
            wr_en_mem   <= 0;

            state         <= 0;
            stall_counter <= 0;

        end else begin

            PC_pass     <= inst_addr;
            ins         <= inst_data;
            funct7      <= funct7_c ;
            rs1         <= rs1_c    ;
            rs2         <= rs2_c    ;
            rd          <= rd_c     ;
            funct3      <= funct3_c ;
            opcode      <= opcode_c ;
            imm_I       <= imm_I_c  ;
            imm_S       <= imm_S_c  ;
            imm_SB      <= imm_SB_c ;
            imm_U       <= imm_U_c  ;
            imm_UJ      <= imm_UJ_c ;

            case (state)

                2'h0 :  begin
                            inst_addr <=    (read_rs1 && !rs1_safe) ? inst_addr : //reg locks
                                            (read_rs2 && !rs2_safe) ? inst_addr : 
                                            (opcode == 7'b1100111)  ? inst_addr : //JALR
                                            (opcode == 7'b1100011)  ? inst_addr : //BRANCH
                                            (opcode == 7'b1101111)  ? inst_addr + imm_UJ_c : //JAL
                                            inst_addr + 4; //ELSE

                            state <=    (read_rs1 && !rs1_safe) ? 0 : //reg locks
                                        (read_rs2 && !rs2_safe) ? 0 :
                                        (opcode == 7'b1100111)  ? 1 : //JALR
                                        (opcode == 7'b1100011)  ? 2 : //BRANCH
                                        0;

                            stall_counter <=    (opcode == 7'b1100111) ? 2 :
                                                (opcode == 7'b1100011) ? 2 :
                                                0;

                            wr_en_reg   <=  (read_rs1 && !rs1_safe) ? 0 : //reg locks
                                            (read_rs2 && !rs2_safe) ? 0 :
                                            wr_reg_op;
                            
                            wr_en_mem   <=  (read_rs1 && !rs1_safe) ? 0 : //reg locks
                                            (read_rs2 && !rs2_safe) ? 0 :
                                            wr_mem_op;
                            
                end

                2'h1 :  begin //JALR

                            if (stall_counter == 0) begin

                                state <= 0;

                                inst_addr <= result_ex;

                            end else begin

                                stall_counter <= stall_counter - 1;

                            end

                end

                2'h2 :  begin //BRANCH

                            if (stall_counter == 0) begin

                                state <= 0;

                                inst_addr <=    branch_ex ?
                                                inst_addr + imm_SB_c :
                                                inst_addr + 4;

                            end else begin

                                stall_counter <= stall_counter - 1;

                            end

                end

                default :   begin
                                inst_addr   <= 32'h0;
                                funct7      <= 7'h0;
                                rs1         <= 5'h0;
                                rs2         <= 5'h0;
                                rd          <= 5'h0;
                                funct3      <= 3'h0;
                                opcode      <= 7'h0;
                                imm_I       <= 32'h0;
                                imm_S       <= 32'h0;
                                imm_SB      <= 32'h0;
                                imm_U       <= 32'h0;
                                imm_UJ      <= 32'h0;
                                wr_en_reg   <= 0;
                                wr_en_mem   <= 0;

                                state         <= 0;
                                stall_counter <= 0;
                end

            endcase

        end

    end


    Parse p0 (      
        .ins        (inst_data),
        .funct7     (funct7_c ),
        .rs2        (rs2_c    ), 
        .rs1        (rs1_c    ), 
        .funct3     (funct3_c ), 
        .rd         (rd_c     ), 
        .opcode     (opcode_c ), 
        .imm_I      (imm_I_c  ), 
        .imm_S      (imm_S_c  ), 
        .imm_SB     (imm_SB_c ), 
        .imm_U      (imm_U_c  ), 
        .imm_UJ     (imm_UJ_c ),
        .wr_en_reg  (wr_reg_op),
        .wr_en_mem  (wr_mem_op),
        .read_rs1   (read_rs1 ),
        .read_rs2   (read_rs2 ));

    reg_lock rl0 (
        .rs1_safe   (rs1_safe ),
        .rs2_safe   (rs2_safe ),
        .wr_en      (wr_reg_op),
        .rd         (rd       ),
        .rs1        (rs1      ),
        .rs2        (rs2      ),
        .opcode     (opcode   ),
        .clk        (clk      ),
        .rst        (rst      ));    


endmodule
