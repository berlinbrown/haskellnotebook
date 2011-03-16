###----------------------------------------------
### Makefile
### Berlin Brown
### 9/27/2007
###----------------------------------------------

APP=wordproc

APP_SRC = BotWords.hs	\

all: ${APP}

###
### Target Build
###
${APP}: ${APP_SRC}
	ghc --make -fglasgow-exts BotWords.hs -o $@

###
### Target Clean
### 
clean:
	rm -vf *.o *.hi *.prof *.manifest ${APP} ${APP}.exe 

###----------------------------------------------
### End of Makefile
###----------------------------------------------