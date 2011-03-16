############################################################
#  Copyright (C) 2003-2007  Berlin Brown (Botnode.com).  All rights reserved.
#
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions are met:
#
#  1. Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer.
#  2. Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions and the following disclaimer in the
#     documentation and/or other materials provided with the distribution.
#  3. The name of the author may not be used to endorse or promote products
#     derived from this software without specific prior written permission.
#
#  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
#  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
#  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
#  ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
#  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
#  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
#  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
#  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
#  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
#  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
#  SUCH DAMAGE.
############################################################

CC = gcc
LD = ld

APP_NAME = octanec
APP_TEST = octane_test
VERSION  = 0.0

DEBUG   = true
CFLAGS  = -Wall

ifdef DEBUG
	CFLAGS += -g
else
	CFLAGS += -O3 $(SITE_CFLAGS)
endif

OBJS_MAIN = octane/octanec_main.o

OBJS =                          \
	octane/list_lib.o       \
	octane/octanec_lib.o    \
	octane/stack_lib.o      \
	octane/hashtable.o

TEST_OBJS =                          \
	octane/test/test_framework.o \
	octane/test/test_octane.o    \
	octane/test/test_pair.o      \
	octane/test/test_hashtable.o \

default: $(APP_NAME) $(APP_TEST)

help:
	@echo "Run '$(MAKE)' with one of the following parameters:"

run: $(APP_TEST)
	./$(APP_TEST)

$(APP_NAME): $(OBJS) $(OBJS_MAIN)
	$(CC) $(LIBS) $(LIBPATH) -L. $(CFLAGS) -o $@ $(OBJS) $(OBJS_MAIN)

$(APP_TEST): $(TEST_OBJS) $(OBJS)
	$(CC) $(LIBS) $(LIBPATH) -L. $(CFLAGS) -o $@ $(OBJS) $(TEST_OBJS)

clean:
	rm -vf octane/*.o octane/test/*.o	
	rm -vf $(APP_NAME) $(APP_NAME).exe
	rm -vf $(APP_TEST) $(APP_TEST).exe
	rm -vf *.stackdump
	rm -Rvf api/html api/latex

doc:
	doxygen api_doc_config.d 

.c.o:
	$(CC) -c $(CFLAGS) -o $@ $<

.PHONY: $(APP_NAME) $(APP_TEST)

############################################################
# End of Makefile
############################################################
