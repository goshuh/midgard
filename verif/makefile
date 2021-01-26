export VERIF = $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))


SHELL   := /usr/bin/zsh
NULL    := >/dev/null 2&>/dev/null

CMP_DIR := $(VERIF)/cmp
RUN_DIR := $(VERIF)/run

FLIST   := $(VERIF)/flist.vc

EXE     := $(CMP_DIR)/simv
LOG     := $(RUN_DIR)/sim.log
FSDB    := $(RUN_DIR)/dump.fsdb
SEED    := $(shell echo $${RANDOM})
RUNARGS :=


.PHONY: all cmp run dbg clean


all: cmp run


cmp:
	mkdir -p $(CMP_DIR) && cd $(CMP_DIR) && vcs -full64 -sverilog -f $(FLIST) -debug_pp -LDFLAGS -rdynamic -LDFLAGS -Wl,--no-as-needed -P $(VERDI_HOME)/share/PLI/VCS/LINUX64/novas.tab $(VERDI_HOME)/share/PLI/VCS/LINUX64/pli.a

run:
	mkdir -p $(RUN_DIR) && cd $(RUN_DIR) && $(CMP_DIR)/simv -l $(LOG) +seed $(SEED) $(RUNARGS)

dbg:
	cd $(RUN_DIR) && verdi -nologo -sv -ssv -ssy -f $(FLIST) -ssf $(FSDB) &!

clean:
	rm -rf $(CMP_DIR) $(RUN_DIR)
