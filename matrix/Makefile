# name:          Makefile
# Stolen from other places, this thing is an awful mess!

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

# Name of code
CODE := matrix.cmo

TEST := matrix.out

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

# OCaml interpreter
OCAML := metaocaml

# OCaml byte-code compiler
OCAMLC := metaocamlc

# OCaml native code compiler
OCAMLOPT := metaocamlopt

# Flags for both OCaml compilers
OCAMLFLAGS := -warn-error AX -g

# Flags to compile a syntax extension
OCAMLCFLAGS-PA := -I +camlp4 $(PP-EXT)

# Flags only for the byte-code compiler
OCAMLCFLAGS := $(OCAMLFLAGS)

# Flags only for the native compiler
OCAMLOPTFLAGS := $(OCAMLFLAGS)

# Flags to use when linking a byte-code or native executable
OCAMLLINKFLAGS := -g

########################################################################
#
# Special Targets
#
########################################################################

# Build all syntax extensions.
.PHONY: all
all: test

.PHONE: test
test: $(SYNTAX-EXTENSION) $(TEST)
	diff -B -b matrix.out matrix.res 

# Launch an OCaml interpreter enriched with all of our syntax
# extensions.
.PHONY: top-level
top-level: $(SYNTAX-EXTENSION)
	$(OCAML) camlp4o.cma pa_extend.cmo $(SYNTAX-EXTENSION)


# Remove most files that we can remake
.PHONY: clean
clean:
	rm -f *.cm[iox] *.annot
	rm -f $(TESTS:.ml=) $(TESTS:.ml=.opt)

# Remove all files that we can remake and all uninteresting ones, too.
.PHONY: distclean
distclean: clean
	rm --force *.ml-pp *.mli-gen $(DISTNAME)-*.tar.gz *~

# Target for forced rule application
.PHONY: FORCE

mytop:
	ocamlmktop -o mytop bigarray.cma

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

# Compile an OCaml implementation module to native code.
%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

# Compile an OCaml implementation module to byte-code.
%.cmo: %.ml $(SYNTAX-EXTENSION)
	$(OCAMLC) $(PP) $(OCAMLCFLAGS) -c $<

%.out: test-%.ml %.cmo mytop
	./mytop ./$*.cmo < $< > $@

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

