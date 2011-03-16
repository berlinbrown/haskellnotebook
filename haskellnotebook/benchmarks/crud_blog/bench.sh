#!/bin/sh

ab -c 20 -n 5000 -g "LIN_bench__gnu.txt" -e "LIN_bench__default.csv" http://localhost:4242/

