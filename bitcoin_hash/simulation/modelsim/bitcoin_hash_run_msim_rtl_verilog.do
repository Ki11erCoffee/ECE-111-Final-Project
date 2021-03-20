transcript on
if {[file exists rtl_work]} {
	vdel -lib rtl_work -all
}
vlib rtl_work
vmap work rtl_work

vlog -sv -work work +incdir+C:/Users/Potato/Documents/School/ECE\ 111/me/bitcoin_hash {C:/Users/Potato/Documents/School/ECE 111/me/bitcoin_hash/bitcoin_hash.sv}

