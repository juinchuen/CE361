setenv LMC_TIMEUNIT -9
vlib work
vmap work work

# compile
vlog -work work "pipelined_cpu.v"
vlog -work work "tb_lab3.v"
vlog -work work "lib_lab2.v"


vsim -classdebug -voptargs=+acc +notimingchecks -L work work.tb -wlf tb.wlf

# wave
add wave -noupdate -group TOP
add wave -noupdate -group TOP -radix decimal /tb/*

add wave -noupdate -group REGM -radix hexadecimal /tb/CPU/RF/Mem

add wave -noupdate -group IF -radix decimal /tb/CPU/IF/*
add wave -noupdate -group ID -radix decimal /tb/CPU/ID/*
add wave -noupdate -group EX -radix binary /tb/CPU/EX/*
add wave -noupdate -group MA -radix binary /tb/CPU/MA/*
add wave -noupdate -group RWB -radix binary /tb/CPU/RWB/*

run -all