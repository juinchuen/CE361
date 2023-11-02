// Testbench for Northwestern - CompEng 361 - Lab2
`include "sc_cpu.v"

module tb;
   reg clk, rst;
   wire halt;

   // Single Cycle CPU instantiation
   SingleCycleCPU CPU (halt, clk, rst);

   // Clock Period = 10 time units
   //  (stops when halt is asserted)  
   always
     #5 clk = ~clk & !halt;

   initial begin
      // Clock and reset steup
      #0 rst = 1; clk = 0;
      #0 rst = 0;
      #0 rst = 1;

      // Load program
      #0 $readmemh("mem_in.hex", CPU.IMEM.Mem);
      #0 $readmemh("mem_in.hex", CPU.DMEM.Mem);
      #0 $readmemh("regs_in.hex", CPU.RF.Mem);

      // Feel free to modify to inspect whatever you want
      #0 $monitor($time,, "PC=%08x IR=%08x rs1 = %d DRS1=%08X rs2 = %d DRS2=%08X  rd = %d DRD=%08X HALT=%b halt_load=%b halt_effective_addr=%b halt_branch=%b effective_add=%08x opcode=%7b func3=%3b DataOutM=%32b LoadReg=%32b", CPU.PC, CPU.InstWord, CPU.rs1, CPU.DataRS1, CPU.rs2, CPU.DataRS2, CPU.rd, CPU.DataInRd, halt, CPU.rw0.halt_load, CPU.rw0.halt_effective_addr, CPU.rw0.halt_branch, CPU.rw0.EffectiveDataAddr, CPU.opcode, CPU.funct3, CPU.DataOutM, CPU.DataInRd);

      // Exits when halt is asserted
      wait(halt);

      // Dump registers
      #0 $writememh("regs_out.hex", CPU.RF.Mem);

      // Dump memory
      #0 $writememh("mem_out.hex", CPU.DMEM.Mem);

      $finish;      
   end
   

endmodule // tb

