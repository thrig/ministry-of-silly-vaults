CFLAGS+=-std=c99 -lm
#CFLAGS+=-g

crawlpaper: crawlpaper.go
	go build crawlpaper.go

# this in turn requires perl and App::cpanminus installed. And, make.
depend:
	@cpanm --installdeps .
	@go get github.com/fogleman/gg

# KLUGE requires multiple runs to fill in the references
noise.pdf: noise.tex
	xelatex -halt-on-error -interaction=batchmode noise.tex
	bibtex noise.aux

termlbrot: termlbrot.c

clean:
	@-rm -f termlbrot noise.bbl noise.blg noise.log noise.out noise.tex.bak >/dev/null 2>&1

.PHONY: clean depend
