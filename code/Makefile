# name:          Makefile

########################################################################
#
# Variables
#
########################################################################

SYNTAX_EXTENSION := ../camlp4/pa_monad2.cmo

PP := camlp4o -I . $(SYNTAX_EXTENSION)

PROG := funct2

VERSION := 1.0


########################################################################
#
# Special Targets
#
########################################################################

.PHONY: all
all: $(PROG)


.PHONY: test
test: all
	./$(PROG)

.PHONY: top-level
top-level: $(SYNTAX_EXTENSION)
	ocaml camlp4o.cma pa_extend.cmo $(SYNTAX_EXTENSION)

.PHONY: clean
clean:
	rm --force $(PROG) *.cm[io]

########################################################################
#
# Implicit Rules
#
########################################################################

%: %.ml
	metaocamlc -pp '$(PP)' -o $@ $<


########################################################################
#
# Explicit Rules
#
########################################################################

$(PROG): $(SYNTAX_EXTENSION) $(PROG).ml

../camlp4/pa_monad2.cmo:
	(chdir ../camlp4; make)
