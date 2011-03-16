#!/usr/bin/python
##
## Simple obfuscation program
## And a simple startup script.
## (Note: not the best script in the world)
"""
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

"""
import sys

replace_chars = [
	('a', 'h'),
	('b', 'q'),
	('8', '3'),
	('9', '4')
	]

def obfuscate_line(line):
	new_line = line
	for tupl in replace_chars:
		f, s = tuple(tupl)
		f2 = f.upper()
		# replace f and f2 with s
		new_line = new_line.replace(f, s)
		new_line = new_line.replace(f2, s)
	return new_line
		

def read_file(name):
	f = open(name)
	all_data = f.readlines()
	for n in all_data:
		new_line = obfuscate_line(n)
		print new_line,

if __name__ == '__main__':
	if (len(sys.argv) != 2):
		print "Invalid Args"
		sys.exit(1)
	read_file(sys.argv[1])
