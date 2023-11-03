setenv LMC_TIMEUNIT -9
vlib work
vmap work work

# compile
vlog -work work "sc_cpu.v"
vlog -work work "tb_lab2.v"
vlog -work work "lib_lab2.v"


vsim -classdebug -voptargs=+acc +notimingchecks -L work work.tb -wlf tb.wlf

# wave
add wave -noupdate -group TOP
add wave -noupdate -group TOP -radix decimal /tb/*
add wave -noupdate -group HALT -radix decimal /tb/CPU/rw0/halt_effective_addr
add wave -noupdate -group REGW -radix decimal /tb/CPU/rw0/*
add wave -noupdate -group CPU -radix decimal /tb/CPU/RF/*

add wave -noupdate -group INST -radix binary /tb/CPU/p0/*

add wave -noupdate -group BRANCH -radix hexadecimal /tb/CPU/rw0/pc_up0/*

run -all