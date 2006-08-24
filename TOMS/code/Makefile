# name:          Makefile

########################################################################
#
# Variables
#
########################################################################

SYNTAX_EXTENSION := ../camlp4/pa_monad.cmo

PP := camlp4o -I . $(SYNTAX_EXTENSION)

INFRA := abstractrep.cmi coderep.cmi 
BASE := stateCPSMonad.cmo infra.cmo code.cmo infra_code.cmo ge.cmo
BASE2 := direct.cmo infra_direct.cmo
OBJ  := rkf45.cmo fft.cmo
OBJ2  := 
TEST := ge.out rkf45.out fft.out ge_direct.out
RES  := ge.res rkf45.res fft.res ge_direct.res

VERSION := 1.0

########################################################################
#
# Special Targets
#
########################################################################

.PHONY: all
all: test $(TEST)

.PHONY: test 
test: $(SYNTAX_EXTENSION) $(OBJ) $(RES) $(TEST)
	diff ge.out ge.res
	diff ge_direct.out ge_direct.res
	diff rkf45.out rkf45.res
	diff fft.out fft.res

mytop: 
	ocamlmktop -o mytop nums.cma

.PHONY: clean
clean:
	rm -f $(TEST) *.cm[io]

$(OBJ): $(INFRA) $(BASE)
$(OBJ2): $(INFRA) $(BASE) $(BASE2)

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

ge_direct.out: test-ge_direct.ml $(BASE2) mytop
	./mytop $(BASE) $(BASE2) < $< > $@

../camlp4/pa_monad.cmo:
	(cd ../camlp4; make all)
