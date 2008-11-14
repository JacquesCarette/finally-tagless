# name:          Makefile
# synopsis:      Construction rules for monadic "do" syntax extension
# authors:       Chris L. Spiel (nifty stuff), Lydia E. van Dijk (boring rest)
# last revision: Thu Nov 13 08:28:15 UTC 2008
# make version:  3.81


########################################################################
#
# Macros
#
########################################################################

# [$(call rotate-left, LIST)] answers LIST rotated to the left by one
# position.
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
TESTS := pythagorean_triples.ml \
         test_exception.ml test_syntax.ml test_monad.ml test_rec.ml \
         test_cc.ml


# Sources of interactive tests.  These files will only be compiled,
# but not run by any make(1) command.
INTERACTIVE-TESTS := monadic_io.ml


# These are the libraries necessary to run the OCaml interpreter with
# out syntax extension.  For older OCaml version use "camlp4o.cma
# pa_extend.cmo".
INTERACTIVE-LIBRARIES := dynlink.cma camlp4o.cma


# Names of all modules in addition to the syntax-extension that are
# documented.
ADDITIONAL-DOCUMENTED-MODULES := cc.ml exception.ml io.ml utest.ml


# Pre-Precessor-Pretty-Printer for OCaml
CAMLP4 := camlp4orf


# Option to feed OCaml sources through the pre-processor. This
# particular incantation is used to compile the test of our syntax
# extensions.
PP := -pp '$(CAMLP4) -I . $(SYNTAX-EXTENSION)'


# Option to feed OCaml sources through the pre-processor.  This
# particular incantation is used to compile a syntax extension for the
# OCaml language.
PP-EXT := -pp $(CAMLP4)


# Directory for the HTML documentation
HTML-DOCUMENTATION := html-doc


# OCaml interpreter
#
# Use for example [rlwrap ocaml] to get convenient command-line
# editing within the interpreter.  RLWrap can be found at
# http://utopia.knoware.nl/~hlub/uck/rlwrap/.
OCAML := ocaml


# OCaml byte-code compiler
OCAMLC := ocamlc


# OCaml native code compiler
OCAMLOPT := ocamlopt


# OCaml's documentation generator
OCAMLDOC := ocamldoc


# Ocaml's universal package manager
OCAMLFIND := ocamlfind


# Flags for both OCaml compilers
OCAMLFLAGS := -warn-error AX -g #-dtypes


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


# Flags for all findlib commands
OCAMLFINDFLAGS :=


# Name of the tarball in which we package all files
DISTNAME := monad-syntax-extension


# Get the version number of the packages from file "VERSION".
VERSION := $(shell cat VERSION)


# Name of the package for findlib
FINDLIB-NAME := monad


# Non Ocaml Tools

# Name of the program to erase files.
RM := rm -f


# Name of the program to erase directories.
RMDIR := rmdir


# Name of the program to replace strings in files.
#
# If you have no working sed(1) on your machine or your are not into
# Latin, use [perl -Wp] instead.
SED := sed


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


# Run a selected test only.
run-%: %
	./$^


# Generate the documentation.
.PHONY: doc
doc: $(HTML-DOCUMENTATION)/$(SYNTAX-EXTENSION:.cmo=)


# Let findlib install the syntax extension.
.PHONY: findlib-install
findlib-install: META $(SYNTAX-EXTENSION)
	$(OCAMLFIND) install $(OCAMLFINDFLAGS) $(FINDLIB-NAME) $^


# Let findlib un-install the syntax extension.
.PHONY: findlib-uninstall
findlib-uninstall:
	$(OCAMLFIND) remove $(OCAMLFINDFLAGS) $(FINDLIB-NAME)


# Create a distributable archive with the help of the revision
# management system.
.PHONY: dist
dist:
	svn export --quiet . ../$(DISTNAME)-$(VERSION)
	tar --create \
            --gzip \
            --file=$(DISTNAME)-$(VERSION).tar.gz \
            --directory=.. \
            $(DISTNAME)-$(VERSION)
	$(RM) ../$(DISTNAME)-$(VERSION)/*
	$(RMDIR) ../$(DISTNAME)-$(VERSION)


# Check whether the project is ready for distribution:
# (1) Remove all files that can be remade.
# (2) Rebuild everything and run all tests.
# (3) Generate documentation.
.PHONY: distcheck
distcheck: distclean test doc


# Launch an OCaml interpreter enriched with all of our syntax
# extensions.
.PHONY: top-level
top-level: $(SYNTAX-EXTENSION)
	$(OCAML) $(INTERACTIVE-LIBRARIES) $(SYNTAX-EXTENSION)


# Remove most files that we can remake.
.PHONY: clean
clean:
	$(RM) *.cm[iox] *.annot
	$(RM) $(TESTS:.ml=) $(TESTS:.ml=.opt)
	$(RM) $(INTERACTIVE-TESTS:.ml=) $(INTERACTIVE-TESTS:.ml=.opt)


# Remove all files that we can remake and all uninteresting ones, too.
.PHONY: distclean
distclean: clean
	$(RM) *.ml-pp *.mli-gen $(DISTNAME)-*.tar.gz *~
	$(RM) -r $(HTML-DOCUMENTATION)
	$(RM) META transcript.log


# Canonical target for forced rule application
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


# Instantiate generic files with the values of this Makefile.
%: %.in
	$(SED) \
	    -e 's|\@VERSION@|$(VERSION)|g;' \
	    -e 's|\@NAME@|$(FINDLIB-NAME)|g;' \
	    -e 's|\@EXTENSION@|$(SYNTAX-EXTENSION)|g;' \
	    < $< > $@


########################################################################
#
# Explicit Rules
#
########################################################################

# Generate the documentation for our syntax extension.
$(HTML-DOCUMENTATION)/$(SYNTAX-EXTENSION:.cmo=): \
  $(SYNTAX-EXTENSION:.cmo=.ml) $(ADDITIONAL-DOCUMENTED-MODULES:.ml=.cmi)
	test -d $(HTML-DOCUMENTATION) || mkdir $(HTML-DOCUMENTATION)
	$(OCAMLDOC) $(OCAMLDOCFLAGS) -html -d $(HTML-DOCUMENTATION) \
	    -o $@ $< $(ADDITIONAL-DOCUMENTED-MODULES:.ml=.mli) $(ADDITIONAL-DOCUMENTED-MODULES)


# Additional dependencies of the exception monad
exception.cmo: exception.cmi


# Additional dependencies of the IO-monad
io.cmo: exception.cmi io.cmi


# Additional dependencies of the CC-monad
cc.cmo: cc.cmi


# Additional dependencies of the "CC Monad" example
test_cc.cmo: cc.cmi utest.cmi $(SYNTAX-EXTENSION)
test_cc: cc.cmo utest.cmo


# Additional dependencies of the "Exception Monad" example
test_exception.cmo: exception.cmi utest.cmi $(SYNTAX-EXTENSION)
test_exception: exception.cmo utest.cmo


# Additional dependencies of the "Pythogorean-Tiples" example
pythagorean_triples.cmo: utest.cmi $(SYNTAX-EXTENSION)
pythagorean_triples: utest.cmo


# Additional dependencies of the syntax tests
test_syntax.cmo: utest.cmi $(SYNTAX-EXTENSION)
test_syntax: utest.cmo


# Additional dependencies of the application tests
test_monad.cmo: utest.cmi $(SYNTAX-EXTENSION)
test_monad: utest.cmo


# Additional dependencies of the recursive-binding tests
test_rec.cmo: utest.cmi $(SYNTAX-EXTENSION)
test_rec: utest.cmo


# Dependencies of "Monadic IO" example.  In contrary to the other
# tests this one is interactive.  Thus we do not want it to run with
# the non-interactive tests.
monadic_io.cmo: monadic_io.ml exception.cmi io.cmi $(SYNTAX-EXTENSION)
	$(OCAMLC) $(OCAMLCFLAGS) $(PP) -c $(@:cmo=ml)
monadic_io: exception.cmo io.cmo monadic_io.cmo
