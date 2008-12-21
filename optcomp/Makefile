# Makefile
# --------
# Copyright : (c) 2008, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of optcomp.


OPTCOMP-EXTENSION := pa_optcomp.cmo

OCAMLC := ocamlc

OCAMLFIND := ocamlfind

CAMLP4 := camlp4

CAMLP4-FULL := $(CAMLP4)of

PP-EXT := -pp $(CAMLP4-FULL)

OCAMLCFLAGS-PA := -I +camlp4 $(PP-EXT)

RM := rm -f

SED := sed

NAME := optcomp

VERSION := $(shell head -n 1 VERSION)


.PHONY: all
all: META $(OPTCOMP-EXTENSION)


.PHONY: install
install:
	$(OCAMLFIND) install $(NAME) META $(OPTCOMP-EXTENSION) sample.ml


.PHONY: uninstall
uninstall:
	$(OCAMLFIND) remove $(NAME)


.PHONY: dist
dist:
	DARCS_REPO=$(PWD) darcs dist --dist-name $(NAME)-$(VERSION)


.PHONY: clean
clean:
	$(RM) *.cm* META $(NAME)-*.tar.gz


.PHONY: distclean
distclean: clean
	$(RM) *~


pa_%.cmo: pa_%.ml
	$(OCAMLC) $(OCAMLCFLAGS-PA) -c $<


META: VERSION META.in
	$(SED) -e 's/@VERSION@/$(VERSION)/' META.in > META
