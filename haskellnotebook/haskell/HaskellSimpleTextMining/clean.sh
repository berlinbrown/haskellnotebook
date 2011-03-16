#!/bin/sh
#----------------------------
# Clean garbage files
# *.pyc
#----------------------------

find . \( -name "*.pyc" -o -name "*.hi" -o -name "*.o" \) -exec rm -vf {} \;
