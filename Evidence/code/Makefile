# name:          Makefile

########################################################################
#
# Variables
#
########################################################################

# No syntax extension needed (yet)
# SYNTAX_EXTENSION := pa_monad.cmo

# PP := camlp4o -I . $(SYNTAX_EXTENSION)

OBJ  := examples.cmo
TEST := examples.out
RES  := examples.res


########################################################################
#
# Special Targets
#
########################################################################

.PHONY: all
all: test $(TEST)

.PHONY: test 
test: $(OBJ) $(RES) $(TEST)
	diff examples.out examples.res

.PHONY: clean-test
clean-test:
	rm -f *.out

# mytop: 
# 	ocamlmktop -o mytop nums.cma

.PHONY: clean
clean:
	rm --force $(TEST) *.cm[io]

########################################################################
#
# Explicit and Implicit Rules
#
########################################################################

$(TEST): $(OBJ) 

%.cmo: %.ml
	metaocamlc -c $<

%.out: test-%.ml %.cmo
	metaocaml ./$*.cmo < $< > $@

examples.cmo: examples.ml 
	metaocamlc -c $<
