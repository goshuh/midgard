export VERIF = $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))


SHELL   := /usr/bin/zsh
NULL    := >/dev/null 2&>/dev/null

OUT_DIR := $(VERIF)/out
SIM_DIR := $(VERIF)/sim

FLIST   := $(VERIF)/flist.vc

LOG     := $(SIM_DIR)/sim.log
FSDB    := $(SIM_DIR)/dump.fsdb
SEED    ?= $(shell echo $${RANDOM})
RUNARGS ?= +err.log=-2 +llc.req_max=0 +llc.acc_max=0 +mem.req_max=0 +mem.acc_max=0 +l1d.req_max=0 +l1d.acc_max=0


.PHONY: all out sim dbg clean


all: out sim


out:
	mkdir -p $(OUT_DIR) && cd $(OUT_DIR) && vcs -full64 -sverilog -override_timescale=1ns/1ns +define+DUMPFSDB -f $(FLIST) -LDFLAGS -rdynamic -LDFLAGS -Wl,--no-as-needed -debug_pp -P $(VERDI_HOME)/share/PLI/VCS/LINUX64/novas.tab $(VERDI_HOME)/share/PLI/VCS/LINUX64/pli.a

sim:
	mkdir -p $(SIM_DIR) && cd $(SIM_DIR) && $(OUT_DIR)/simv -l $(LOG) $(RUNARGS)

dbg:
	cd $(SIM_DIR) && verdi -nologo -sv -ssv -ssy -f $(FLIST) -ssf $(FSDB) $(NULL) &!

clean:
	rm -rf $(OUT_DIR) $(SIM_DIR)
