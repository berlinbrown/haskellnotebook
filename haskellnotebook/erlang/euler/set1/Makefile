## Makefile
TOPDIR := $(shell pwd)
DATE   = $(shell date +%Y%m%d)

APPLICATION = set1

ESRC  = ./src
EBIN  = ./ebin

ERLC  = erlc
ERL   = erl

OPT   = -W

SRC         = $(wildcard $(ESRC)/*.erl)
TARGET      = $(addsuffix .beam, $(basename \
                 $(addprefix $(EBIN)/, $(notdir $(SRC)))))

all: clean ${APPLICATION}

LIB_TARGET_OBJS = $(EBIN)/euler14.beam \
	$(EBIN)/euler25.beam \

${APPLICATION}: $(TARGET) $(LIB_TARGET_OBJS) 

ebin/%.beam: $(ESRC)/%.erl
	$(ERLC) $(OPT) -o ebin $<

run: $(APPLICATION)
	$(ERL) -noshell -pa $(TOPDIR)/ebin/ -s euler25 main -s erlang halt

clean:
	-rm -vf $(TARGET)
	-rm -vf erl_crash.dump
	-rm -vf ebin/*.beam
