## Makefile
TOPDIR := $(shell pwd)
DATE   = $(shell date +%Y%m%d)

APPLICATION = compile1 

ESRC  = ./src
EBIN  = ./ebin

ERLC  = erlc
ERL   = erl

OPT   = -W

SRC         = $(wildcard $(ESRC)/*.erl)
TARGET      = $(addsuffix .beam, $(basename \
                 $(addprefix $(EBIN)/, $(notdir $(SRC)))))

all: clean ${APPLICATION}

LIB_TARGET_OBJS = $(EBIN)/compile1.beam

${APPLICATION}: $(TARGET) $(LIB_TARGET_OBJS) 

ebin/%.beam: $(ESRC)/%.erl
	$(ERLC) $(OPT) -o ebin $<

run: $(APPLICATION)
	$(ERL) -noshell -pa $(TOPDIR)/ebin/ -s $(APPLICATION) main -s erlang halt

clean:
	-rm -vf $(TARGET)
	-rm -vf erl_crash.dump
	-rm -vf ebin/*.beam
