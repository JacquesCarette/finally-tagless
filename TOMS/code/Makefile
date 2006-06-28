# name:          Makefile

########################################################################
#
# Variables
#
########################################################################

SYNTAX_EXTENSION := ../camlp4/pa_monad.cmo

PP := camlp4o -I . $(SYNTAX_EXTENSION)

BASE := infra.cmo
OBJ  := ge.cmo rkf45.cmo
TEST := ge.out rkf45.out
RES  := ge.res rkf45.res

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
	diff rkf45.out rkf45.res

mytop: 
	ocamlmktop -o mytop nums.cma

.PHONY: clean-test
clean-test:
	rm -f rkf45.out

.PHONY: clean
clean:
	rm --force $(TEST) *.cm[io]

$(OBJ): $(BASE)

########################################################################
#
# Implicit Rules
#
########################################################################

%.cmo: %.ml
	metaocamlc -pp '$(PP)' -g -c $<

%.out: test-%.ml %.cmo mytop
	./mytop ./infra.cmo ./$*.cmo < $< > $@

########################################################################
#
# Explicit Rules
#
########################################################################

../camlp4/pa_monad.cmo:
	(cd ../camlp4; make all)
