###----------------------------------------------
### Makefile
### Berlin Brown
### 9/27/2007
###----------------------------------------------

APP_PREFIX=problem

APP_1=problem1
APP_SRC_1 = Reader.hs

all: ${APP_1}

###
### Target Build
###
${APP_1}: ${APP_SRC_1}
	ghc --make -fglasgow-exts ${APP_SRC_1} -o $@

###
### Target Clean
### 
clean:
	rm -vf *.o *.hi *.prof *.manifest ${APP_PREFIX}*

###----------------------------------------------
### End of Makefile
###----------------------------------------------