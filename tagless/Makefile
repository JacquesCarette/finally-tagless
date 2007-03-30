.SUFFIXES:
.SUFFIXES: .dvi .pdf .ps .tex .eps .jpg .png .bbl .aux .ppm .pnm .png .gif .jpg

export BIBTEX := bibtex -min-crossrefs=9999

all: tagless-final.dvi

tagless-final.dvi tagless-final.pdf: tagless.bib

xdvi: tagless-final.dvi
	xdvi -paper us $<

.tex.dvi:
	if texi2dvi $<; then killall -USR1 xdvi.bin; true; fi

.tex.pdf:
	texi2dvi --pdf $<

.pdf.ps:
	pdftops $< $@
