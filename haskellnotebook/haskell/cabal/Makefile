###----------------------------------------------
### Makefile
### 9/20/2007
###----------------------------------------------

TOP_DIR := $(shell pwd)

all: build

build-all:
	runhaskell Setup.hs configure
	runhaskell Setup.hs build --verbose

build:
	runhaskell Setup.hs build --verbose
	./dist/build/test1/test1

clean:
	rm -Rf dist

###----------------------------------------------
### End of the Script
###----------------------------------------------