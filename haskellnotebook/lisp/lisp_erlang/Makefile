#**********************************************************
# Build the .beam erlang VM files
# Makefile for irc bot (based on orbitz bot)
# Laughing man IRC library uses orbitz
#
# Date: 3/1/2008
# Author: Berlin Brown
#
#**********************************************************

TOPDIR := $(shell pwd)
DATE   = $(shell date +%Y%m%d)
WIN_TOPDIR := $(shell cygpath -w `pwd`)

APPLICATION = riki_server
VERSION     = 0.0.1

ESRC  = ./src
EBIN  = ./ebin

TEST_DIR     = ./test/erlang
TEST_DIR_SRC = $(TEST_DIR)/src

ERLC  = erlc
ERL   = erl

OPT   = -W
INC   = $(ESRC)/inc

SED   = $(shell which sed)

TMP         = $(wildcard *~) $(wildcard src/*~) $(wildcard inc/*~)
INC_FILES   = $(wildcard $(INC)/*.hrl)
SRC         = $(wildcard $(ESRC)/*.erl)
CONFFILES   = conf/config.xml $(wildcard conf/*fortune)

TARGET      = $(addsuffix .beam, $(basename \
                 $(addprefix $(EBIN)/, $(notdir $(SRC)))))
LIB_TARGET_OBJS = $(EBIN)/client_handler.beam    \
	$(EBIN)/server_lib.beam        \
	$(EBIN)/riki_server.beam       \
	$(EBIN)/lisp_parse_std.beam    \
	$(EBIN)/lisp_parse.beam        \

${APPLICATION}: $(TARGET) $(LIB_TARGET_OBJS) $(TEST_OBJS)

all: clean ${APPLICATION}

clean:
	-rm -vf $(TARGET) $(TMP)
	-rm -vf erl_crash.dump
	-rm -vf ebin/*.beam

ebin/%.beam: $(TEST_DIR_SRC)/%.erl
	$(ERLC) $(OPT) -I $(INC) -o ebin $<

ebin/%.beam: $(ESRC)/%.erl
	$(ERLC) $(OPT) -I $(INC) -o ebin $<

# Start the erl emulator process with the social stats module
# Also -s erlang halt
run: $(APPLICATION)
	$(ERL) -noshell -pa $(TOPDIR)/ebin -s riki_server start_server -s erlang halt

# Note: error with running from win32/cygwin, see tests.sh
winrun: $(APPLICATION)
	$(ERL) -noshell -pa $(WIN_TOPDIR)/ebin/ -s riki_server start_server -s erlang halt

# Run the test suite.
# Start the test IRC server, delay for 5 seconds and start the client.
tests: $(APPLICATION)
# Run the basic tests and then the server test.
# Ideally, test modules will have a 'run_tests' function.
	$(ERL) -noshell -pa $(TOPDIR)/ebin -s simple_server_example simple_server_lib -s erlang halt &
	-sleep 20

test2: $(APPLICATION)	
	$(ERL) -noshell -pa $(TOPDIR)/ebin -s lisp_parse_std test -s erlang halt

# End of the file 
