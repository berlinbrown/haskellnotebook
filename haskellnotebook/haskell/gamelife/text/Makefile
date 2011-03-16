##----------------------------------------------
## Makefile
## Berlin Brown
## 9/27/2008
##----------------------------------------------

TOPDIR := $(shell pwd)
APP=gamelife
APP2=gamelifegl

all: ${APP} ${APP2}

# Target Build
${APP}: GameText.hs 
	ghc --make -fglasgow-exts $< -o $@

${APP2}: GameGLRender.hs 
	ghc --make -fglasgow-exts $< -lglut -o $@

# Target Clean
clean:
	rm -vf *.o *.hi *.prof *.manifest ${APP}* ${APP2}* *.class

run: ${APP2}
	exec ${TOPDIR}/gamelifegl

win: ${APP}
	exec ${TOPDIR}/gamelife.exe

## Java targets
java: JGameLife.class JGameGLLife.class
#java JGameLife
	java -classpath '.:jogl.jar:gluegen-rt.jar' -Djava.library.path=. GLLifeMain

JGameLife.class: JGameLife.java
	javac -classpath '.:jogl.jar' *.java

## Java OpenGL Targets
## Note: java and opengl are required to run this particular demo
JGameGLLife.class: JGameGLLife.java JRenderCanvas.java
	echo 'jogl.jar is required to run this demo, see jogl.dev.java.net'
	javac -classpath '.:jogl.jar' *.java

#----------------------------------------------
# End of Makefile
#----------------------------------------------
