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

client1: build
	./dist/build/client1/client1

client2: build
	./dist/build/client2/client2

clean:
	rm -Rf dist

###----------------------------------------------
### End of the Script
###----------------------------------------------