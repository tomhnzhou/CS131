#!/bin/bash

model=$1
NRUN=10
echo "Run:		|	avg time/transition"
echo "-----------------------------------------"
for ((run_num=1; run_num<=$NRUN; run_num++))
do
	n_threads=16
	n_swap=1000000
	array_size=100
	array_max=20
	array_str=""
	maxval=20

	for ((i=1; i<=$array_size; i++))
	do
		rand=$(( (RANDOM % $array_max) + 1))
		array_str="$array_str $rand"

	done

	cmd_str="java UnsafeMemory $model $n_threads $n_swap $maxval$array_str"
	retstr=$($cmd_str)
	avgt=$(echo $retstr | sed 's/[^0-9.]//g')
	echo "$run_num		|	$avgt"
			
done
