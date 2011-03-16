#!/bin/sh
# Execute the simple obfuscation program
# Note: run the script from some sub directory.
# ../all.sh
# OK, not the best script

LIST=`find -name '*.*'`

for i in $LIST ; do
	echo $i	
	../o.py $i > $i.tmp && mv $i.tmp $i
done

find -name '*.tmp' -exec rm -vf {} \;
