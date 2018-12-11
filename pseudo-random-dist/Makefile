CFLAGS=-std=c99 -O3 -Wall -pedantic -pipe

.SUFFIXES: .tex .pdf

.tex.pdf:
	xelatex -halt-on-error -interaction=batchmode ${.IMPSRC}

luck.pdf: luck.tex

depend:
	@make -C .. depend

clean:
	git clean --force -x

.PHONY: clean depend
