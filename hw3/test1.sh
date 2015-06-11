#!/bin/bash

model=$1
echo "nThreads:	|	nSwap:		|	array_size:	| array_sum:	|	avg time/transition"
echo "------------------------------------------------------------------------------------------------------"
for n_threads in 8 16 32
do
	for n_swap in 10 100
	do
		for array_size in 20 50
		do
			for array_max in 10 20 
			do
				array_str=""
				maxval=127
				array_sum=0
				for ((i=1; i<=$array_size; i++))
				do
					rand=$(( (RANDOM % $array_max) + 1))
					array_str="$array_str $rand"
					if [ $rand -gt $maxval ]
					then
							maxval=$rand
					fi

					array_sum=$((array_sum + rand))
				done

				cmd_str="java UnsafeMemory $model $n_threads $n_swap $maxval$array_str"
				retstr=$($cmd_str)
				avgt=$(echo $retstr | sed 's/\([0-9]+\).*/\1/')
				echo "$n_threads |	$n_swap		|	$array_size	|	$array_sum	|	$avgt"
			
			done
		done
	done
done
