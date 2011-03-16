#!/bin/sh

find -size -200c -type f | gawk '{p = ""; s = split($0, sp, "/"); for (i = 1; i < s; i++) p = p (p?"/":"") sp[i]; b = sp[s]; getline a < $0; if (match(a, /^[a-zA-Z0-9_\-\.]*$/)) if(system("if [ -e " p "/" a " ]; then exit 66; fi")==66) system("cd " p "; rm " b "; ln -s " a " " b)}'
