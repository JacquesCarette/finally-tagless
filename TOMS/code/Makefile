# name:          Makefile

########################################################################
#
# Variables
#
########################################################################

SYNTAX_EXTENSION := ../camlp4/pa_monad.cmo

PP := camlp4o -I . $(SYNTAX_EXTENSION)

OBJ  := infra.cmo ge.cmo
TEST := ge.out
RES  := ge.res

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

.PHONY: clean-test
clean-test:
	rm -f *.out

mytop: 
	ocamlmktop -o mytop nums.cma

.PHONY: clean
clean:
	rm --force $(TEST) *.cm[io]

# Special case - make sure this still works 'raw'
#

########################################################################
#
# Implicit Rules
#
########################################################################

%.cmo: %.ml
	metaocamlc -pp '$(PP)' -g -c $<

%.out: test-%.ml %.cmo mytop
	./mytop ./$*.cmo < $< > $@

########################################################################
#
# Explicit Rules
#
########################################################################

ge.out: test-ge.ml ge.cmo infra.cmo mytop
	./mytop ./infra.cmo ./ge.cmo < test-ge.ml > ge.out 

../camlp4/pa_monad.cmo:
	(cd ../camlp4; make all)
