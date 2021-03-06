# name:          Makefile
# synopsis:      Construction rules for monadic "do" syntax extension
# author:        Lydia E. van Dijk
# adapted:       Jacques Carette
# last revision: Wed Feb 2 2005
# ocaml version: 3.08.0


########################################################################
#
# Variables
#
########################################################################

SYNTAX_EXTENSION := pa_monad.cmo

PP := camlp4o -I . $(SYNTAX_EXTENSION)

TEST := test-monad3 tt

DISTNAME := monad-function

VERSION := 1.0


########################################################################
#
# Special Targets
#
########################################################################

.PHONY: all
all: $(TEST)


.PHONY: test
test: all
	./test-monad3
	./tt


.PHONY: pretty-print
pretty-print: $(TEST)-pretty-print.ml
	cat $^


.PHONY: top-level
top-level: $(SYNTAX_EXTENSION)
	ocaml camlp4o.cma pa_extend.cmo $(SYNTAX_EXTENSION)


.PHONY: clean
clean:
	rm --force $(TEST) *.cm[io]


########################################################################
#
# Explicit and Implicit Rules
#
########################################################################

pa_%.cmo: pa_%.ml
	ocamlc -c -pp 'camlp4o pa_extend.cmo q_MLast.cmo' -I +camlp4 $<

pa-%.cmo: pa-%.ml
	metaocamlc -verbose -c -pp 'camlp4o pa_extend.cmo q_MLast.cmo' -I +camlp4 $<

test-lift%.txt: test-lift%.ml
	camlp4o -I . pa-lift.cmo pr_o.cmo $<

test-lift%: test-lift%.ml
	metaocaml camlp4o.cma pa_extend.cmo pa-lift.cmo < $<

test3.cmo: test3.ml pa_monad.cmo
	metaocamlc -c -pp 'camlp4o pa_extend.cmo q_MLast.cmo ./pa_monad.cmo' -I +camlp4 $<

%-pretty-print.ml: %.ml
	$(PP) pr_o.cmo $< > $@

tt: tt.ml test3.cmo
	metaocamlc ./test3.cmo -o $@ $<

#%: %.ml
#	metaocamlc -pp '$(PP)' -o $@ $<

########################################################################
#
# Explicit Rules
#
########################################################################

$(TEST): $(SYNTAX_EXTENSION) $(TEST).ml
