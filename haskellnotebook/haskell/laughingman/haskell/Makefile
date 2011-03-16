###----------------------------------------------
### Makefile
### Berlin Brown
### 9/27/2007
###----------------------------------------------

LMAN_APP=lman

LMAN_SRC =	ManClient.hs		\
			NewHttpBrowser.hs	\

all: ${LMAN_APP}

###
### Target Build Octane Mech
###
${LMAN_APP}: ${LMAN_SRC}
	ghc --make -fglasgow-exts -package HTTP ManClient.hs -o $@

###
### Target Clean
### 
clean:
	rm -vf *.o *.hi *.prof *.manifest ${LMAN_APP} ${LMAN_APP}.exe 

###----------------------------------------------
### End of Makefile
###----------------------------------------------