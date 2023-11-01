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

run -all