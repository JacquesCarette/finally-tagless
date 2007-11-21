.SUFFIXES:
.SUFFIXES: .dvi .pdf .ps .tex .eps .jpg .png .bbl .aux .ppm .pnm .png .gif .jpg

export BIBTEX := bibtex -min-crossrefs=9999

all: jfp.dvi jfp.pdf aplas.dvi aplas.pdf talk.pdf talk.ps

jfp.dvi jfp.pdf aplas.dvi aplas.pdf: tagless-final.tex preamble.tex fancyvrb1.sty natbib1.sty

talk.pdf: White_pearl_necklace.jpg

White_pearl_necklace.jpg:
	wget http://upload.wikimedia.org/wikipedia/commons/a/af/$@

.tex.dvi:
	if texi2dvi $<; then killall -USR1 xdvi.bin; true; fi

.tex.pdf:
	texi2dvi --pdf $<

.pdf.ps:
	pdftops -nocrop $< $@
