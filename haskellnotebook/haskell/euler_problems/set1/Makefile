###----------------------------------------------
### Makefile
### Berlin Brown
### 9/27/2007
###----------------------------------------------

APP_PREFIX=problem

APP_1=problem1
APP_SRC_1 = Problem1.hs

APP_2=problem2
APP_SRC_2 = Problem2.hs

APP_3=problem3
APP_SRC_3 = Problem3.hs

all: ${APP_1} ${APP_2} ${APP_3} euler

###
### Target Build
###
${APP_1}: ${APP_SRC_1}
	ghc --make -fglasgow-exts ${APP_SRC_1} -o $@

${APP_2}: ${APP_SRC_2}
	ghc --make -fglasgow-exts ${APP_SRC_2} -o $@

${APP_3}: ${APP_SRC_3}
	ghc --make -fglasgow-exts ${APP_SRC_3} -o $@

euler: Euler25.hs 
	ghc --make -fglasgow-exts Euler25.hs -o $@


###
### Target Clean
### 
clean:
	rm -vf *.o *.hi *.prof *.manifest ${APP_PREFIX}*
	rm -vf euler*

###----------------------------------------------
### End of Makefile
###----------------------------------------------
