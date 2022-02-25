export VERIF = $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))


SHELL   := /usr/bin/zsh
NULL    := >/dev/null 2&>/dev/null

CMP_DIR := $(VERIF)/cmp
RUN_DIR := $(VERIF)/run

FLIST   := $(VERIF)/flist.vc

LOG     := $(RUN_DIR)/sim.log
FSDB    := $(RUN_DIR)/dump.fsdb
SEED    ?= $(shell echo $${RANDOM})
RUNARGS ?= +err.log=0


.PHONY: all cmp run dbg clean


all: cmp run


cmp:
	mkdir -p $(CMP_DIR) && cd $(CMP_DIR) && vcs -full64 -sverilog -override_timescale=1ns/1ns +define+DUMPFSDB -f $(FLIST) -kdb -debug_access+all -lca -LDFLAGS -rdynamic -LDFLAGS -Wl,--no-as-needed -P $(VERDI_HOME)/share/PLI/VCS/LINUX64/novas.tab $(VERDI_HOME)/share/PLI/VCS/LINUX64/pli.a

run:
	mkdir -p $(RUN_DIR) && cd $(RUN_DIR) && $(CMP_DIR)/simv -l $(LOG) $(RUNARGS)

dbg:
	cd $(RUN_DIR) && verdi -nologo -sv -ssv -ssy -f $(FLIST) -ssf $(FSDB) $(NULL) &!

clean:
	rm -rf $(CMP_DIR) $(RUN_DIR)
