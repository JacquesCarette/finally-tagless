# name:          Makefile
# synopsis:      Construction rules for monadic "do" syntax extension
# author:        Lydia E. van Dijk
# last revision: Sat Dec 10 08:56:57 UTC 2005
# ocaml version: 3.09.0


########################################################################
#
# Variables
#
########################################################################

SYNTAX_EXTENSION := pa_monad.cmo

PP := camlp4o -I . $(SYNTAX_EXTENSION)
PP-EXT := camlp4o -I . pa_extend.cmo q_MLast.cmo

TEST := test-monad

DISTNAME := monad-syntax-extension

VERSION := 1.0


########################################################################
#
# Special Targets
#
########################################################################

.PHONY: all
all: $(SYNTAX_EXTENSION)


.PHONY: test
test: pythagorean-triples test-monad exception test-syntax
	for x in $^; do $$x; done


.PHONY: doc
doc: html-doc/pa_monad.html


.PHONY: dist
dist: distcheck
	darcs dist --dist-name $(DISTNAME)-$(VERSION)


.PHONY: distcheck
distcheck: distclean test


.PHONY: top-level
top-level: $(SYNTAX_EXTENSION)
	ocaml camlp4o.cma pa_extend.cmo $(SYNTAX_EXTENSION)


.PHONY: clean
clean:
	rm --force $(TEST) *.cm[io] *.annot


.PHONY: distclean
distclean: clean
	rm --force *.ml-pp $(DISTNAME)-*.tar.gz *~


########################################################################
#
# Implicit Rules
#
########################################################################

# Compile syntax extension
pa_%.cmo: pa_%.ml
	ocamlc -c -dtypes -pp '$(PP-EXT)' -I +camlp4 $<


# Pritty-print a given OCaml file
%.ml-pp: %.ml pa_monad.cmo
	$(PP) pr_o.cmo $< > $@


# Compile an OCaml file to an executable using the syntax extension
%: %.ml $(SYNTAX_EXTENSION)
	ocamlc -warn-error A -pp '$(PP)' -o $@ $<


########################################################################
#
# Explicit Rules
#
########################################################################

html-doc/pa_monad.html: pa_monad.ml
	test -d html-doc || mkdir html-doc
	ocamldoc -html -d html-doc -pp '$(PP-EXT)' -I +camlp4 -o $@  $<
