# name:          Makefile

########################################################################
#
# Variables
#
########################################################################

SYNTAX_EXTENSION := ../camlp4/pa_monad2.cmo

PP := camlp4o -I . $(SYNTAX_EXTENSION)

OBJ  := funct1.cmo funct2.cmo funct3.cmo funct4.cmo
TEST := funct1.out funct2.out funct3.out funct4.out
RES  := funct1.res funct2.res funct3.res funct4.res
#funct3.res 

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
	#diff funct1.out funct1.res
	#diff funct2.out funct2.res
	#diff funct3.out funct3.res
	diff funct4.out funct4.res

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
	metaocamlc -pp '$(PP)' -g -c $<

%.out: test-%.ml %.cmo
	metaocaml ./$*.cmo < $< > $@

########################################################################
#
# Explicit Rules
#
########################################################################

funct1.cmo: funct1.ml
	metaocamlc -c funct1.ml

funct3.cmo: funct3.ml $(SYNTAX_EXTENSION)
	metaocamlc -c -pp "camlp4o pa_extend.cmo q_MLast.cmo $(SYNTAX_EXTENSION)" -I +camlp4 $<

../camlp4/pa_monad2.cmo:
	(chdir ../camlp4; make all)
