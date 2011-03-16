###****************************
### README Document
###
### Berlin Brown (berlin.brown at gmail.com)
### Date: 11/18/2007
### Set: Examples1
###
###
### THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
### "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
### LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
### A PARTICULAR PURPOSE ARE DISCLAIMED.
###****************************

-------------------------------
 * Description 
-------------------------------
 Simple factor examples and startup/vocab scripts.

 Factor version as of git pull: 11/10/2007
 
 Factor 0.91 x86/64 image 
 git clone http://factorcode.org/git/factor.git

 On Win32, I was able to build the source, make sure that
 you add to the CFLAGS
 -mno-cygwin

 Per:
 http://code-factor.blogspot.com/2007/04/building-factor-in-cygwin-now-supported.html

-------------------------------
 * Overview of the scripts 
-------------------------------
 some_script_name.sh will contain a simple (a lazy/easy approach) bash script
 that invokes the factor binary and image and also launches
 the myhelloworld vocab.  The vocab must have a MAIN: definition.

 factor_bin_conf.sh - simple include script with factor binary paths

 You will need to edit the factor_bin_conf.sh script with your factor
 binary and image path locations.  Also, you will need edit the
 vocab_name variable in the some_script_name.sh script.

-------------------------------
 * Resources 
-------------------------------

http://factorcode.org/

http://factorlang-fornovices.blogspot.com/

-------------------------------
 * Startup times, compared with Java Example
-------------------------------

I included a simple Java source example and will include
similar imperative style examples, just to see
how long it took java to startup and run a similar application.

javac HelloWorld.java

$ time java HelloWorld
Hello World

real    0m0.108s
user    0m0.015s
sys     0m0.000s

-------------------------------
 * Directory Structure for Example
-------------------------------

 Note: the directory and file structure is important.
 
 some_script_name.sh - shell script that invokes the factor executable
  to load the vocab.
 factor_bin_conf.sh - simple include script with factor binary paths

 vocab_name/ - subdirectory
  * authors.txt  - simple text file, list of author names 
  * myhelloworld.factor - source code with hello word definition
  * tags.txt - whitespace separated list of tags to classify the vocabulary
  * summary.txt - one line description of the module

