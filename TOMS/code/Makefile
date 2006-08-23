# name:          Makefile

########################################################################
#
# Variables
#
########################################################################

SYNTAX_EXTENSION := ../camlp4/pa_monad.cmo

PP := camlp4o -I . $(SYNTAX_EXTENSION)

INFRA := abstractrep.cmi coderep.cmi 
BASE := stateCPSMonad.cmo infra.cmo code.cmo infra_code.cmo
OBJ  := ge.cmo rkf45.cmo fft.cmo
TEST := ge.out rkf45.out fft.out
RES  := ge.res rkf45.res fft.res

VERSION := 1.0

########################################################################
#
# Special Targets
#
########################################################################

.PHONY: all
all: clean-test test $(TEST)

.PHONY: test 
test: $(SYNTAX_EXTENSION) $(OBJ) $(RES) $(TEST)
	diff ge.out ge.res
	diff rkf45.out rkf45.res
	diff fft.out fft.res

mytop: 
	ocamlmktop -o mytop nums.cma

.PHONY: clean-test
clean-test:
	rm -f rkf45.out

.PHONY: clean
clean:
	rm -f $(TEST) *.cm[io]

$(OBJ): $(INFRA) $(BASE)

########################################################################
#
# Implicit Rules
#
########################################################################

.SUFFIXES: .ml .mli .cmo .cmi

.ml.cmo:
	metaocamlc -pp '$(PP)' -w A -g -c $<

.mli.cmi:
	metaocamlc -pp '$(PP)' -c $<

%.out: test-%.ml %.cmo mytop
	./mytop $(BASE) ./$*.cmo < $< > $@

########################################################################
#
# Explicit Rules
#
########################################################################

../camlp4/pa_monad.cmo:
	(cd ../camlp4; make all)
