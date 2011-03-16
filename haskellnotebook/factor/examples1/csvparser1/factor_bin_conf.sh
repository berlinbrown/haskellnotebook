#!/bin/sh

## (Replace the FACTOR_BIN executable with the proper 
## path of your factor executable, and IMAGE location)

## Unix path
FACTOR_BIN=/factorbuild/factor/factor
FACTOR_IMG=/factorbuild/factor/factor.image

FACTOR_BIN_WIN=/cygdrive/c/projects/tools/home/projects/aaageneralprojects/factorbuild/factor/factor-nt.exe
FACTOR_IMG_WIN=/cygdrive/c/projects/tools/home/projects/aaageneralprojects/factorbuild/factor/factor-nt.image

## This will overwrite, existing values if working with cygwin.
if [ $(uname -s | grep -c CYGWIN) -gt 0 ] ; then
	## We need the correct windows image path, executable path is ok
	FACTOR_IMG=`cygpath -w $FACTOR_IMG_WIN`
	FACTOR_BIN=$FACTOR_BIN_WIN
fi

