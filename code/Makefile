# name:          Makefile

########################################################################
#
# Variables
#
########################################################################

SYNTAX_EXTENSION := ../camlp4/pa_monad2.cmo

PP := camlp4o -I . $(SYNTAX_EXTENSION)

OBJ  := funct1.cmo funct2.cmo
TEST := funct1.out funct2.out
RES  := funct1.res funct2.res

VERSION := 1.0


########################################################################
#
# Special Targets
#
########################################################################

.PHONY: all
all: clean-test test $(TEST)

.PHONY: test 
test: $(OBJ) $(RES) $(TEST)
	diff funct1.out funct1.res
	diff funct2.out funct2.res

.PHONY: clean-test
clean-test:
	rm -f *.out

.PHONY: top-level
top-level: $(SYNTAX_EXTENSION)
	ocaml camlp4o.cma pa_extend.cmo $(SYNTAX_EXTENSION)

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
	metaocamlc -pp '$(PP)' -c $<

%.out: test-%.ml %.cmo
	metaocaml ./$*.cmo < $< > $@

########################################################################
#
# Explicit Rules
#
########################################################################

funct1.cmo: funct1.ml
	metaocamlc -c funct1.ml

../camlp4/pa_monad2.cmo:
	(chdir ../camlp4; make all)
