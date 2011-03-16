###----------------------------------------------
### Makefile
### 9/20/2007
###----------------------------------------------

TOP_DIR := $(shell pwd)

all: build

build-all:
	runhaskell Setup.hs configure
	runhaskell Setup.hs build --verbose

tests:
	runhaskell Setup.hs build --verbose
	./dist/build/functional_tests/functional_tests

unit-tests:
	./dist/build/spider_unit_tests/spider_unit_tests

clean:
	rm -Rf dist

###----------------------------------------------
### End of the Script
###----------------------------------------------