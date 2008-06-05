.SUFFIXES:
.SUFFIXES: .dvi .pdf .ps .tex .eps .jpg .png .bbl .aux .ppm .pnm .png .gif .jpg

export BIBTEX := bibtex -min-crossrefs=9999

all: jfp.dvi jfp.pdf # aplas.dvi aplas.pdf talk.pdf talk.ps

jfp.dvi jfp.pdf aplas.dvi aplas.pdf: tagless-final.tex preamble.tex fancyvrb1.sty natbib1.sty
jfp.dvi jfp.pdf: tagless.bib
aplas.dvi aplas.pdf: aplas.bib

talk.pdf: White_pearl_necklace.jpg FC0836213122.jpg

White_pearl_necklace.jpg:
	wget http://upload.wikimedia.org/wikipedia/commons/a/af/$@

FC0836213122.jpg: FC0836213122.JPG
	convert -threshold 58880 $< threshold.ppm
	convert -separate -channel Red threshold.ppm red.pbm
	convert -separate -channel Green threshold.ppm green.pbm
	convert -separate -channel Blue threshold.ppm blue.pbm
	composite -compose multiply red.pbm green.pbm threshold.pbm
	composite -compose multiply blue.pbm threshold.pbm threshold.pbm
	composite -compose screen $< threshold.pbm $@
	rm threshold.ppm red.pbm green.pbm blue.pbm threshold.pbm

FC0836213122.JPG:
	wget http://images.overstock.com/f/102/3117/8h/www.overstock.com/images/products/bnt/$@

.tex.dvi:
	if texi2dvi --batch $<; then killall -USR1 xdvi.bin; true; fi

.tex.pdf:
	texi2dvi --pdf --batch $<

.pdf.ps:
	pdftops -nocrop $< $@
