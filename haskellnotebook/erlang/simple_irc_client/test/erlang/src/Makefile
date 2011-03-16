#**********************************************************
# Build the .beam erlang VM files
# Makefile for irc bot (based on manderlbrot)
# Based on manderlbot - 0.9.2
#**********************************************************

OPT   = -W
INC   = .
ERLC  = erlc
ERL   = erl
SED   = $(shell which sed)

ESRC  = .
EBIN  = ../../ebin

SRC         = $(wildcard $(ESRC)/*.erl)
TARGET      = $(addsuffix .beam, $(basename \
                 $(addprefix $(EBIN)/, $(notdir $(SRC)))))
EMAKE       = $(addsuffix \'., $(addprefix \'../, $(SRC)))

${APPLICATION}: $(TARGET)

all: ${APPLICATION}

OBJECTS=$(SOURCES:%.erl=$(EBIN)/%.$(EMULATOR))

ebin/%.beam: src/%.erl 
	$(ERLC) $(OPT) -I $(INC) -o ebin $<
