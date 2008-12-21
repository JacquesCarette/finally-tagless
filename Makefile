# name:          Makefile
# synopsis:      Construction rules for monadic "do" syntax extension
# authors:       Chris L. Spiel (nifty stuff), Lydia E. van Dijk (boring rest)
# last revision: Fri Dec 19 08:49:10 UTC 2008
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

# Name of our compiled syntax extension.
MONAD-EXTENSION := pa_monad.cmo


# Directory where the sources for "optcomp" are
OPTCOMP-DIRECTORY := optcomp


# Name the syntax extension to perform conditional compilation.  We
# depend on this one!
OPTCOMP-EXTENSION := $(OPTCOMP-DIRECTORY)/pa_optcomp.cmo


# List of all sub-directories we must consider when building the
# syntax extension or the tests.
SUBDIRECTORIES := $(OPTCOMP-DIRECTORY)


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


# Plain vanilla pre-precessor-pretty-printer for OCaml.
CAMLP4 := camlp4


# The pre-precessor-pretty-printer we need for out syntax extension.
#
# "o": Host language (outside of quotations) is original OCaml.
# "r": Embedded language (inside of quotations) follows the Revised
#      Syntax.
# "f": Fully loaded preprocessor with parsers, grammars, quotations,
#      macros, and list comprehensions.
CAMLP4-FULL := $(CAMLP4)orf


# Option to feed OCaml sources through the pre-processor. This
# particular incantation is used to compile the tests of our syntax
# extension.
PP := -pp '$(CAMLP4-FULL) $(OPTCOMP-EXTENSION) $(MONAD-EXTENSION)'


# Option to feed OCaml sources through the pre-processor.  This
# particular incantation is used to compile a syntax extension for the
# OCaml language.
PP-EXT := -pp '$(CAMLP4-FULL) $(OPTCOMP-EXTENSION)'


# Directory for the HTML documentation
HTML-DIRECTORY := html-doc


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


#
# Non Ocaml Tools
#


# Name of the program to erase files.
RM := rm -f


# Name of the program to erase directories.
RMDIR := rmdir


# Name of the program to replace strings in files.
#
# If you have no working sed(1) on your machine and you are not into
# Latin, use [perl -Wp] instead.
SED := sed


# The text explains the most important features of this Makefile.  It
# is used by the phony target "help".
#
# IMPLEMENTATION NOTE
#     We use a single string constructed by Make(1), because this is
#     much faster than writing each line with a separate "echo", which
#     implicitly spawns a new shell.
HELP-TEXT := "\
This is the Makefile for the OCaml monadic syntax extension.\n\
\n\
* Phony Targets\n\
    help:: Display this help message\n\
    all:: Build the syntax extension '$(MONAD-EXTENSION)'\n\
    doc:: Generate HTML-documentation in directory '$(HTML-DIRECTORY)'\n\
    clean:: Delete most generated files\n\
    distclean:: Delete all files not belonging to the distribution\n\
    top-level:: Launch a toplevel interpreter, pre-load '$(MONAD-EXTENSION)'\n\
\n\
* Implicit Rules\n\
    %.ml-pp:%.ml:: Preprocess %.ml with the syntax extension\n\
\n\
* Influential Variables\n\
    OCAMLC:: Name of the Ocaml byte-code compiler '$(OCAMLC)'\n\
    CAMLP4:: Name of the plain-vanilla OCaml preprocessor '$(CAMLP4)'\n\
    OCAMLFLAGS:: Flags for the byte-code and the native compiler '$(OCAMLFLAGS)'\n\
\n\
The text inside single quotes are the current values.\n\
"


########################################################################
#
# Special Targets
#
########################################################################

# Build all syntax extensions.
.PHONY: all
all: $(MONAD-EXTENSION)


# Build "optcomp", a parser module that enables C-like conditional
# compilation.  It is third-party software and thus resides in a
# separate directory.
.PHONY: optcomp
optcomp:
	$(MAKE) --directory=$(OPTCOMP-DIRECTORY) \
                OCAMLC=$(OCAMLC) CAMLP4=$(CAMLP4) \
                pa_optcomp.cmo


# Run all binaries with tests.  Stop if any test fails.
.PHONY: test
test: $(foreach file,$(TESTS:.ml=),run-$(file)) $(INTERACTIVE-TESTS:.ml=)


# Run a selected test only.
run-%: %
	./$^


# Generate the documentation.
.PHONY: doc
doc: $(HTML-DIRECTORY)/$(MONAD-EXTENSION:.cmo=)


# Let findlib install the syntax extension.
.PHONY: findlib-install
findlib-install: META $(MONAD-EXTENSION)
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
top-level: $(MONAD-EXTENSION)
	$(OCAML) $(INTERACTIVE-LIBRARIES) $(MONAD-EXTENSION)


# Remove most files that we can remake.
.PHONY: clean
clean:
	for dir in $(SUBDIRECTORIES); do $(MAKE) --directory=$$dir clean; done
	$(RM) *.cm[iox] *.annot
	$(RM) $(TESTS:.ml=) $(TESTS:.ml=.opt)
	$(RM) $(INTERACTIVE-TESTS:.ml=) $(INTERACTIVE-TESTS:.ml=.opt)
	$(RM) test_syntax-camlp4-3.09*


# Remove all files that we can remake and all uninteresting ones, too.
.PHONY: distclean
distclean: clean
	for dir in $(SUBDIRECTORIES); do $(MAKE) --directory=$$dir distclean; done
	$(RM) *.ml-pp *.mli-gen $(DISTNAME)-*.tar.gz *~
	$(RM) META transcript.log
	$(RM) $(HTML-DIRECTORY)


# Print help on this Makefile.
.PHONY: help
help:
	@echo -e $(HELP-TEXT) | \
	    $(SED) -e 's/^ //' -e 's/^ *\(.*\):: */    \1: /'


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
%.ml-pp: %.ml $(MONAD-EXTENSION) FORCE
	$(CAMLP4-FULL) -I . $(MONAD-EXTENSION) pr_o.cmo $< > $@


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
	    -e 's|\@EXTENSION@|$(MONAD-EXTENSION)|g;' \
	    < $< > $@


########################################################################
#
# Explicit Rules
#
########################################################################

# The syntax extension for conditional compilation lives in its own
# subdirectory.  We use its own Makefile to build it.
$(OPTCOMP-EXTENSION): $(OPTCOMP-DIRECTORY)/pa_optcomp.ml
	$(MAKE) --directory=$(OPTCOMP-DIRECTORY) pa_optcomp.cmo


# Our syntax extension uses conditional compilation.  It therefore
# depends on the parser "pa_optcomp.cmo".  Some of the unit tests
# depend on it too, but these -- of course -- need MONAD-EXTENSION,
# so we do not need to add the dependency everywhere.
$(MONAD-EXTENSION): $(OPTCOMP-EXTENSION)


# Generate the documentation for our syntax extension.
$(HTML-DIRECTORY)/$(MONAD-EXTENSION:.cmo=): \
  $(OPTCOMP-EXTENSION) \
  $(MONAD-EXTENSION:.cmo=.ml) \
  $(ADDITIONAL-DOCUMENTED-MODULES:.ml=.cmi)
	test -d $(HTML-DIRECTORY) || mkdir $(HTML-DIRECTORY)
	$(OCAMLDOC) $(OCAMLDOCFLAGS) -html -d $(HTML-DIRECTORY) \
	    -o $@ \
            $(filter-out %.cmo,$<) \
            $(ADDITIONAL-DOCUMENTED-MODULES:.ml=.mli) \
            $(ADDITIONAL-DOCUMENTED-MODULES)


# Additional dependencies of the exception monad
exception.cmo: exception.cmi


# Additional dependencies of the IO-monad
io.cmo: exception.cmi io.cmi


# Additional dependencies of the CC-monad
cc.cmo: cc.cmi


# Additional dependencies of the "CC Monad" example
test_cc.cmo: cc.cmi utest.cmi $(MONAD-EXTENSION)
test_cc: cc.cmo utest.cmo


# Additional dependencies of the "Exception Monad" example
test_exception.cmo: exception.cmi utest.cmi $(MONAD-EXTENSION)
test_exception: exception.cmo utest.cmo


# Additional dependencies of the "Pythogorean-Tiples" example
pythagorean_triples.cmo: utest.cmi $(MONAD-EXTENSION)
pythagorean_triples: utest.cmo


# Additional dependencies of the syntax tests for OCaml version 3.10
# and later versions
test_syntax.cmo: utest.cmi $(MONAD-EXTENSION)
test_syntax: utest.cmo


# Strip conditional compilation directives for OCaml-3.09.
test_syntax-camlp4-3.09.ml: test_syntax.ml
	$(SED) -e '/^#if/,/^#endif/d' < $< > $@


# Additional dependencies of the syntax tests for OCaml version 3.09
test_syntax-camlp4-3.09.cmo: utest.cmi $(MONAD-EXTENSION)
test_syntax-camlp4-3.09: utest.cmo


# Additional dependencies of the application tests
test_monad.cmo: utest.cmi $(MONAD-EXTENSION)
test_monad: utest.cmo


# Additional dependencies of the recursive-binding tests
test_rec.cmo: utest.cmi $(MONAD-EXTENSION)
test_rec: utest.cmo


# Dependencies of "Monadic IO" example.  In contrary to the other
# tests this one is interactive.  Thus we do not want it to run with
# the non-interactive tests.
monadic_io.cmo: monadic_io.ml exception.cmi io.cmi $(MONAD-EXTENSION)
	$(OCAMLC) $(OCAMLCFLAGS) $(PP) -c $(@:cmo=ml)
monadic_io: exception.cmo io.cmo monadic_io.cmo
