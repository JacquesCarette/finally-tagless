# name:          Makefile

########################################################################
#
# Variables
#
########################################################################

INFRA := prelude.cmo coderep.cmi
BASE := stateCPSMonad.cmo domains_sig.cmo domains_common.cmo \
	domains_code.cmo \
	code.cmo ge.cmo
BASE2 := direct.cmo domains_direct.cmo
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
test: $(SYNTAX_EXTENSION) $(OBJ) $(BASE) $(BASE2) $(RES) $(TEST)
	diff -B -b ge_direct.out ge_direct.res
	diff -B -b rkf45.out rkf45.res
	diff -B -b fft.out fft.res
	diff -B -b ge.out ge.res

mytop: 
	metaocamlmktop -o mytop nums.cma $(BASE)

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
	metaocamlc -w Ax -g -c $<

.mli.cmi:
	metaocamlc -c $<

%.out: test-%.ml %.cmo mytop
	./mytop $(BASE) ./$*.cmo < $< > $@

########################################################################
#
# Explicit Rules
#
########################################################################

ge.out: test-ge.ml $(BASE) ge.cmo mytop test-ge-common.ml
	./mytop < $< > $@

ge-small.out: test-ge-small.ml $(BASE) ge.cmo mytop test-ge-common-small.ml
	./mytop < $< > $@

ge_direct.out: test-ge_direct.ml $(BASE2) ge.cmo mytop test-ge-common.ml
	./mytop $(BASE2) < $< > $@

rkf45.out: test-rkf45.ml rkf45.cmo mytop
	./mytop rkf45.cmo < $< > $@

fft.out: test-fft.ml fft.cmo mytop
	./mytop fft.cmo < $< > $@

../camlp4/pa_monad.cmo:
	(cd ../camlp4; make all)

# Dependencies. Needed! Otherwise, old versions of files are compiled.

ge.cmo :  stateCPSMonad.cmo prelude.cmo coderep.cmi domains_sig.cmo
domains_common.cmo : domains_sig.cmo
openRecord.cmo : openRecord.cmi
domains_code.cmo : domains_sig.cmo domains_common.cmo
test-ge.ml : ge.cmo code.cmo domains_code.cmo test-ge-common.ml
test-ge-small.ml : ge.cmo code.cmo domains_code.cmo test-ge-common-small.ml
test-ge_direct.ml : ge.cmo code.cmo domains_direct.cmo domains_direct.cmo \
	test-ge-common.ml
