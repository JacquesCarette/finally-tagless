# name:          Makefile
# synopsis:      Construction rules for monadic "do" syntax extension
# authors:       Chris L. Spiel (nifty stuff), Lydia E. van Dijk (boring rest)
# last revision: Fri Jan 13 08:53:36 UTC 2006
# ocaml version: 3.09.0


########################################################################
#
# Macros
#
########################################################################

# [$(call exists, HAYSTACK, NEEDLES)] answers all [NEEDLES] in
# [HAYSTACK], thus lending itself to an existence test.
define exists
$(filter $(1), $(2))
endef


# [$(call join-by LIST, SEPARATOR)] answers the (space-separated) LIST
# catenated with SEPARATOR in between.
#
# The definition of [SPACE] -- a variable that holds a single space--
# involves some very deep Make knowledge/magic.
#
# We cannot use a literal space in the [$(subst ...)] function,
# because the parser throws it away.  After a variable expansion
# whitespace survives.  The remaining task is to define a variable
# with a single space.  The documentation says: "Whitespace around the
# variable name and immediately after the '=' is ignored."  Make keeps
# whitespace before a comment, unless the '=' rule is in effect.
# Thus, we define [SPACE] to be the result of applying the built-in
# function [$(strip)] to the empty string and a space, which is
# protected by the following comment.
SPACE := $(strip) # required comment; do _not_ remove or even move
define join-by
$(subst $(SPACE),$(2),$(strip $(1)))
endef


# [$(call rotate-left, LIST)] answers LIST rotated to the left by one
# position
define rotate-left
$(wordlist 2, 999, $(1)) $(word 1, $(1))
endef


########################################################################
#
# Variables
#
########################################################################

# Name of compiled syntax extension
SYNTAX_EXTENSION := pa_monad.cmo


# Names of all sources that define tests
TESTS := exception.ml pythagorean-triples.ml \
         test-syntax.ml test-monad.ml test-rec.ml


# Pre-Precessor-Pretty-Printer for OCaml
CAMLP4 := camlp4o


# Option to feed OCaml sources through the pre-processor. This
# particular incantation is used to compile the test of our syntax
# extensions.
PP := -pp '$(CAMLP4) -I . $(SYNTAX_EXTENSION)'


# Option to feed OCaml sources through the pre-processor.  This
# particular incantation is used to compile a syntax extension for the
# OCaml language.
PP-EXT := -pp '$(CAMLP4) -I . pa_extend.cmo q_MLast.cmo'


# Directory for the html documentation
HTML-DOCUMENTATION := html-doc


# OCaml interpreter
OCAML := ocaml


# OCaml byte-code compiler
OCAMLC := ocamlc


# OCaml native code compiler
OCAMLOPT := ocamlopt


# OCaml's documentation generator
OCAMLDOC := ocamldoc


# Flags for both OCaml compilers
OCAMLFLAGS := -warn-error AX -g -dtypes


# Flags to compile a syntax extension
OCAMLCFLAGS-PA := -I +camlp4 $(PP-EXT)


# Flags only for the byte-code compiler
OCAMLCFLAGS := $(OCAMLFLAGS)


# Flags only for the native compiler
OCAMLOPTFLAGS := $(OCAMLFLAGS)


# Flags to use when linking a byte-code or native executable
OCAMLLINKFLAGS := -g


# Flags for the documentation generator
OCAMLDOCFLAGS := $(OCAMLCFLAGS-PA)


# Name of the tarball in which we package all files
DISTNAME := monad-syntax-extension


# Version number of the tarball.  (See DISTNAME.)
VERSION := 4.0


########################################################################
#
# Special Targets
#
########################################################################

# Build all syntax extensions.
.PHONY: all
all: $(SYNTAX_EXTENSION)


# Run all binaries with tests.  Stop if any test fails.
.PHONY: test
test: $(foreach file,$(TESTS:.ml=),run-$(file))


# Run a selected test only
run-%: %
	./$^


# Generate the documentation.
.PHONY: doc
doc: $(HTML-DOCUMENTATION)/$(SYNTAX_EXTENSION:.cmo=).html


# Create a distributable archive with the help of the revision
# management system.
.PHONY: dist
dist:
	darcs dist --dist-name $(DISTNAME)-$(VERSION)


# Check whether the project is ready for distribution:
# (1) Remove all files that can be remade.
# (2) Rebuild everything and run all tests.
# (3) Generate documentation
.PHONY: distcheck
distcheck: distclean test doc


# Launch an OCaml interpreter enriched with all of our syntax
# extensions.
.PHONY: top-level
top-level: $(SYNTAX_EXTENSION)
	$(OCAML) camlp4o.cma pa_extend.cmo $(SYNTAX_EXTENSION)


# Remove most files that we can remake
.PHONY: clean
clean:
	rm --force *.cm[iox] *.annot $(TESTS:.ml=) $(TESTS:.ml=.opt)


# Remove all files that we can remake and all uninteresting ones, too.
.PHONY: distclean
distclean: clean
	rm --force *.ml-pp *.mli-gen $(DISTNAME)-*.tar.gz *~
	rm --force --recursive $(HTML-DOCUMENTATION)


# Target for forced rule application
.PHONY: FORCE


########################################################################
#
# Implicit Rules
#
########################################################################

# Compile a syntax extension.
pa_%.cmo: pa_%.ml
	$(OCAMLC) $(OCAMLCFLAGS-PA) -c $<


# Pretty-print an OCaml file.
%.ml-pp: %.ml $(SYNTAX_EXTENSION) FORCE
	$(CAMLP4) -I . $(SYNTAX_EXTENSION) pr_o.cmo $< > $@


# Extract the interface definition from an OCaml implementation
# module.
%.mli-gen: %.ml FORCE
	$(OCAMLC) $(OCAMLCFLAGS) -i $< > $@


# Compile an OCaml interface definition.
%.cmi: %.mli
	$(OCAMLC) $(OCAMLCFLAGS) -c $<


# Compile a test suite that requires our syntax extension to
# native code; currently these are all test files.
$(TESTS:.ml=.cmx): %.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(PP) -c $<


# Compile an OCaml implementation module to native code.
%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<


# Compile a test suite that requires our syntax extension to
# byte-code; currently these are all test files.
$(TESTS:.ml=.cmo): %.cmo: %.ml
	$(OCAMLC) $(OCAMLCFLAGS) $(PP) -c $<


# Compile an OCaml implementation module to byte-code.
%.cmo: %.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c $<


# Link byte-code executable.
#
# We cannot link the object files in the order make(1) puts them into
# [$^] because the main module always comes first.  (It is exactly
# this implicit rule that causes this behavior.)  So, either we use
# ocamldsort(1), which is a bitch, or we simply move the main module
# to the last position and hope for the best.
%: %.cmo
	$(OCAMLC) $(OCAMLLINKFLAGS) -o $@ $(call rotate-left, $^)


# Link native code executable.
#
# For the call to the [rotate-left]-macro see [%: %.cmo].
%.opt: %.cmx
	$(OCAMLOPT) $(OCAMLLINKFLAGS) -o $@ $(call rotate-left, $^)


########################################################################
#
# Explicit Rules
#
########################################################################

# Generate the documentation for all of our syntax extensions.
$(HTML-DOCUMENTATION)/$(SYNTAX_EXTENSION:.cmo=).html: $(SYNTAX_EXTENSION:.cmo=.ml)
	test -d $(HTML-DOCUMENTATION) || mkdir $(HTML-DOCUMENTATION)
	$(OCAMLDOC) $(OCAMLDOCFLAGS) -html -d $(HTML-DOCUMENTATION) -o $@ $^


# Additional dependencies of the "Exception Monad" example
exception.cmo: utest.cmi $(SYNTAX_EXTENSION)
exception: utest.cmo


# Additional dependencies of the "Pythogorean-Tiples" example
pythagorean-triples.cmo: utest.cmi $(SYNTAX_EXTENSION)
pythagorean-triples: utest.cmo


# Additional dependencies of the syntax tests
test-syntax.cmo: utest.cmi $(SYNTAX_EXTENSION)
test-syntax: utest.cmo


# Additional dependencies of the application tests
test-monad.cmo: utest.cmi $(SYNTAX_EXTENSION)
test-monad: utest.cmo


# Additional dependencies of the recursive-binding tests
test-rec.cmo: utest.cmi $(SYNTAX_EXTENSION)
test-rec: utest.cmo
