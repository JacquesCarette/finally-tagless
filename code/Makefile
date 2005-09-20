# name:          Makefile

########################################################################
#
# Variables
#
########################################################################

SYNTAX_EXTENSION := ../camlp4/pa_monad.cmo

PP := camlp4o -I . $(SYNTAX_EXTENSION)

OBJ  := funct1.cmo funct2.cmo funct3.cmo funct4.cmo direct2.cmo infra.cmo ge.cmo
TEST := funct1.out funct2.out funct3.out funct4.out direct2.out ge.out
RES  := funct1.res funct2.res funct3.res funct4.res direct2.res ge.res
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
	diff direct2.out direct2.res
	diff ge.out ge.res

.PHONY: timings
timings: $(OBJ)
	./mytop ./funct4.cmo ./direct2.cmo exper.ml
	./mytop ./funct4.cmo ./direct2.cmo exper2.ml

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

funct1.cmo: funct1.ml
	metaocamlc -c funct1.ml

funct3.cmo: funct3.ml $(SYNTAX_EXTENSION)
	metaocamlc -c -pp "camlp4o pa_extend.cmo q_MLast.cmo $(SYNTAX_EXTENSION)" -I +camlp4 $<

ge.out: test-ge.ml ge.cmo infra.cmo mytop
	./mytop ./infra.cmo ./ge.cmo < test-ge.ml > ge.out 

../camlp4/pa_monad.cmo:
	(cd ../camlp4; make all)
