CFLAGS+=-std=c99 -lm
#CFLAGS+=-g

%.fasl: %.lisp
	sbcl --noinform --non-interactive --eval '(compile-file (second *posix-argv*))' $<

crawlpaper: crawlpaper.go
	go build crawlpaper.go

# this in turn requires perl and App::cpanminus installed. And, make.
depend:
	cpanm --installdeps .
	go get github.com/fogleman/gg

# KLUGE requires multiple runs to fill in the references
dijkstramap.pdf: dijkstramap.tex
	xelatex -halt-on-error -interaction=batchmode dijkstramap.tex
	bibtex dijkstramap.aux
noise.pdf: noise.tex
	xelatex -halt-on-error -interaction=batchmode noise.tex
	bibtex noise.aux

termlbrot: termlbrot.c

clean:
	git clean --force -x

.PHONY: clean depend
