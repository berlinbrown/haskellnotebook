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

-------------------------------
 * Overview of hello.sh 
-------------------------------
 hello.sh is a simple (a lazy/easy approach) bash script
 that invokes the factor binary and image and also launches
 the myhelloworld vocab.  The vocab must have a MAIN: definition.

-------------------------------
 * Startup times, compared with Java Example
-------------------------------

I included a simple Java source example, just to see
how long it took java to startup and run a similar application.

javac HelloWorld.java

$ time java HelloWorld
Hello World

real    0m0.108s
user    0m0.015s
sys     0m0.000s


Factor version, with compilation:

$ ./hello.sh
Compiling 1 words...
Hello world

real    0m0.145s
user    0m0.015s
sys     0m0.015s

-------------------------------
 * Directory Structure for Example
-------------------------------

 Note: the directory and file structure is important.
 
 hello.sh - shell script that invokes the factor executable
  to load the myhelloworld module.
 factor_bin_conf.sh - simple include script with factor binary paths

 myhelloworld/ - myhelloworld module and subdirectory
  * authors.txt  - simple text file, list of author names 
  * myhelloworld.factor - source code with hello word definition
  * tags.txt - whitespace separated list of tags to classify the vocabulary
  * summary.txt - one line description of the module

