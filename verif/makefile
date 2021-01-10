.PHONY: all cmp run dbg clean


SHELL   := /usr/bin/zsh
RUNARGS :=


export VERIF=$(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))


all: cmp run


cmp:
	@mkdir -p cmp && cd cmp && vcs -full64 -sverilog -F ../flist.vc -debug_pp -LDFLAGS -rdynamic -LDFLAGS -Wl,--no-as-needed -P $(VERDI_HOME)/share/PLI/VCS/LINUX64/novas.tab $(VERDI_HOME)/share/PLI/VCS/LINUX64/pli.a

run:
	@mkdir -p run && cd run && ../cmp/simv -l sim.log $(RUNARGS)

dbg:
	@cd run && verdi -nologo -sv -ssv -ssy -F ../flist.vc -ssf dump.fsdb &!

clean:
	@rm -rf cmp run
