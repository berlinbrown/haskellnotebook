###----------------------------------------------
### Makefile
### 9/20/2007
###----------------------------------------------

TOP_DIR := $(shell pwd)

all: queuedump

build-all:
	runhaskell Setup.hs configure
	runhaskell Setup.hs build --verbose

trainreport:
	runhaskell Setup.hs build --verbose
	./dist/build/trainreport/trainreport

queuedump:
	runhaskell Setup.hs build --verbose
	./dist/build/queuedump/queuedump

tokenize:
	runhaskell Setup.hs build --verbose
	./dist/build/tokenize/tokenize -i ../../var/lib/spiderdb/lexicon/stopwords/stopwords.tdb \
		-o ../../var/lib/spiderdb/lexicon/stopwords/stopwords.tdb.tmp

clean:
	rm -Rf dist

###----------------------------------------------
### End of the Script
###----------------------------------------------
