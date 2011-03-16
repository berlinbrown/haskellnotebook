#!/bin/sh

# Output these commands to a temp file and then run gnu plot 
echo "set terminal png
set output 'LIN_bench_.png'
set title 'Benchmark results'
set size 1,1
set key left top
set xlabel 'request'
set ylabel 'ms'
plot 'LIN_bench__gnu.txt' using 10 with lines title 'Benchmark 1' 
" > gnuplot_tmp_cmd.tmp 

# Run the gnu plot command
gnuplot gnuplot_tmp_cmd.tmp > /dev/null


# For future reference, add a command and title to compare results 
# E.g.
# '/tmp/data2' using 10 with lines title 'Benchmark 2', 
