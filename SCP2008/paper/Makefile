.SUFFIXES:
.SUFFIXES: .pdf .ps .dvi .tex .lhs

all: metamonads.dvi

metamonads.dvi metamonads.pdf: metamonads.bib

.lhs.tex:
	lhs2TeX --poly $< > $@

.tex.dvi:
	if texi2dvi $<; then killall -USR1 xdvi.bin; true; else rm $@; false; fi

.pdf.ps:
	pdftops $< $@

.tex.pdf:
	texi2dvi --pdf $<
