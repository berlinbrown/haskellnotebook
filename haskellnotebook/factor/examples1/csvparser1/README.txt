###****************************
### README Document
###
### Berlin Brown
### Date: 11/18/2007
### Set: Examples1
###
### myhelloworld module
###
### THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
### "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
### LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
### A PARTICULAR PURPOSE ARE DISCLAIMED.
###****************************

For a more complicated example (csv file), download the
following.  Botlist URL data drop. It contains over 100,000 URLs 
and associated titles and keywords.  Would be an interesting project
for map / reduce functions.
wget http://openbotlist.googlecode.com/files/botlist_datadump.tar.gz

Running the factor version, against 174,000 lines
$ ./parse.sh
Compiling 10 words...
Running simple csv parser example
Opening file, testcsv.txt
Number of lines in file:
*** Data heap resized to 117440512 bytes
*** Data GC (15 minor, 263 cards)
*** Data heap resized to 234881024 bytes
*** Data GC (14 minor, 207 cards)
*** Data heap resized to 469762048 bytes
*** Data GC (14 minor, 202 cards)
85364
Done.

real    0m9.885s
user    0m0.015s
sys     0m0.000s

(see the java directory)
Running the java Application and 174,000 lines of URL data:

$ time java AnalyzeURLFiles botlist_datadump.dat
Analyzing Files
Reading file=botlist_datadump.dat
Total Lines:174008
Completed in=1859 ms

real    0m1.936s
user    0m0.031s
sys     0m0.000s
