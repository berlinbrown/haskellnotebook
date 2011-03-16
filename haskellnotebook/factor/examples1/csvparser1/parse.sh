#!/bin/sh

## Edit the vocab name accordingly for your project
VOCAB_NAME=client.parse

TOP_DIR=`pwd`

## Include the binary configuration script
. $TOP_DIR/factor_bin_conf.sh

## Execute factor with system time information
time $FACTOR_BIN -i=$FACTOR_IMG -e="\"$VOCAB_NAME\" run" -quiet -run=none

# End of Script
