# name:          Makefile
# synopsis:      Construction rules for monadic "do" syntax extension
# authors:       Chris L. Spiel (nifty stuff), Lydia E. van Dijk (boring rest)
# last revision: Mon Jan 16 14:30:44 UTC 2006
# ocaml version: 3.09.0


########################################################################
#
# Macros
#
########################################################################

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
SYNTAX-EXTENSION := pa_monad.cmo


# Names of all sources that define non-interactive tests.  These files
# will be compiled and run.
TESTS := pythagorean-triples.ml \
         test-exception.ml test-syntax.ml test-monad.ml test-rec.ml


# Sources of interactive tests.  These files will only be compiled,
# but not run by any make(1) command.
INTERACTIVE-TESTS := monadic-io.ml


# Pre-Precessor-Pretty-Printer for OCaml
CAMLP4 := camlp4o


# Option to feed OCaml sources through the pre-processor. This
# particular incantation is used to compile the test of our syntax
# extensions.
PP := -pp '$(CAMLP4) -I . $(SYNTAX-EXTENSION)'


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
all: $(SYNTAX-EXTENSION)


# Run all binaries with tests.  Stop if any test fails.
.PHONY: test
test: $(foreach file,$(TESTS:.ml=),run-$(file)) $(INTERACTIVE-TESTS:.ml=)


# Run a selected test only
run-%: %
	./$^


# Generate the documentation.
.PHONY: doc
doc: $(HTML-DOCUMENTATION)/$(SYNTAX-EXTENSION:.cmo=).html


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
top-level: $(SYNTAX-EXTENSION)
	$(OCAML) camlp4o.cma pa_extend.cmo $(SYNTAX-EXTENSION)


# Remove most files that we can remake
.PHONY: clean
clean:
	rm --force *.cm[iox] *.annot
	rm --force $(TESTS:.ml=) $(TESTS:.ml=.opt)
	rm --force $(INTERACTIVE-TESTS:.ml=) $(INTERACTIVE-TESTS:.ml=.opt)


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
%.ml-pp: %.ml $(SYNTAX-EXTENSION) FORCE
	$(CAMLP4) -I . $(SYNTAX-EXTENSION) pr_o.cmo $< > $@


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
$(HTML-DOCUMENTATION)/$(SYNTAX-EXTENSION:.cmo=).html: $(SYNTAX-EXTENSION:.cmo=.ml)
	test -d $(HTML-DOCUMENTATION) || mkdir $(HTML-DOCUMENTATION)
	$(OCAMLDOC) $(OCAMLDOCFLAGS) -html -d $(HTML-DOCUMENTATION) -o $@ $^


# Additional dependencies of the exception monad
exception.cmo: exception.cmi


# Additional dependencies of the IO-monad
io.cmo: exception.cmi io.cmi


# Additional dependencies of the "Exception Monad" example
test-exception.cmo: exception.cmi utest.cmi $(SYNTAX-EXTENSION)
test-exception: exception.cmo utest.cmo


# Additional dependencies of the "Pythogorean-Tiples" example
pythagorean-triples.cmo: utest.cmi $(SYNTAX-EXTENSION)
pythagorean-triples: utest.cmo


# Additional dependencies of the syntax tests
test-syntax.cmo: utest.cmi $(SYNTAX-EXTENSION)
test-syntax: utest.cmo


# Additional dependencies of the application tests
test-monad.cmo: utest.cmi $(SYNTAX-EXTENSION)
test-monad: utest.cmo


# Additional dependencies of the recursive-binding tests
test-rec.cmo: utest.cmi $(SYNTAX-EXTENSION)
test-rec: utest.cmo


# Dependencies of "Monadic IO" example.  In contrary to the other
# tests this one is interactive.  Thus we do not want it to run with
# the non-interactive tests.
monadic-io.cmo: monadic-io.ml exception.cmi io.cmi $(SYNTAX-EXTENSION)
	$(OCAMLC) $(OCAMLCFLAGS) $(PP) -c $(@:cmo=ml)
monadic-io: exception.cmo io.cmo monadic-io.cmo

