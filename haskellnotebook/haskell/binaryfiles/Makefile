###----------------------------------------------
### Makefile
### Berlin Brown
### 9/27/2007
###----------------------------------------------

APP=dbreader

APP_SRC = DbReader.hs	\

all: ${APP}

###
### Target Build
### binary package is required.
### http://hackage.haskell.org/cgi-bin/hackage-scripts/package/binary-0.4.1
${APP}: ${APP_SRC}
	ghc --make -fglasgow-exts -package binary DbReader.hs -o $@

###
### Target Clean
### 
clean:
	rm -vf *.o *.hi *.prof *.manifest ${APP} ${APP}.exe 

###----------------------------------------------
### End of Makefile
###----------------------------------------------