##----------------------------------------------
## Makefile
## Berlin Brown
## 9/27/2008
##----------------------------------------------

TOPDIR := $(shell pwd)
APP=cannongl
APP2=cannontext

all: ${APP} $(APP2)

# Target Build
${APP}: Cannon.hs SimUtils.hs
	ghc --make -fglasgow-exts Cannon.hs -lglut -o $@

${APP2}: CannonText.hs SimUtils.hs
	ghc --make -fglasgow-exts CannonText.hs -lglut -o $@

# Target Clean
clean:
	rm -vf *.o *.hi *.prof *.manifest ${APP}* ${APP2}

run: ${APP}
	exec ${TOPDIR}/cannongl
text: ${APP2}
	exec ${TOPDIR}/cannontext

win: ${APP}
	exec ${TOPDIR}/cannongl.exe

#----------------------------------------------
# End of Makefile
#----------------------------------------------
