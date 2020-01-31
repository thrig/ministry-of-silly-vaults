CFLAGS=-std=c99 -O2 -Wall -pedantic -fno-diagnostics-color -fstack-protector-strong -fPIC -fPIE -pipe
#CFLAGS+=-g

.SUFFIXES: .lisp .fasl .tex .pdf

.lisp.fasl:
	sbcl --noinform --non-interactive --eval '(let ((file (second *posix-argv*))) (if file (compile-file (second *posix-argv*)) (error "use BSD make")))' ${.IMPSRC}

# KLUGE requires multiple runs to fill in the references
.tex.pdf:
	xelatex -halt-on-error -interaction=batchmode ${.IMPSRC}
	bibtex ${.PREFIX}.aux

around.lisp: util.fasl common.fasl

caves.lisp: util.fasl common.fasl

common.fasl: common.lisp

# NOTE some OS may need `pkg-config --libs --cflags ncurses`
cone-geometry: cone-geometry.c
	$(CC) $(CFLAGS) -lm -lncurses cone-geometry.c -o cone-geometry

corridors.lisp: util.fasl common.fasl dijkstramap.fasl

dijkstramap.fasl: dijkstramap.lisp

dijkstramap.pdf: dijkstramap.tex

linewalker.lisp: util.fasl common.fasl

noise.lisp: util.fasl common.fasl

noise.pdf: noise.tex

poisson-disk-sample.lisp: util.fasl common.fasl

roguememory: roguememory.c
	$(CC) $(CFLAGS) -lncurses roguememory.c -o roguememory

room-size-stats.lisp: util.fasl common.fasl

rooms-random.lisp: util.fasl common.fasl

termlbrot: termlbrot.c
	$(CC) $(CFLAGS) -lm termlbrot.c -o termlbrot

util.fasl: util.lisp

voss.fasl: voss.lisp

clean:
	git clean --force -x

# this in turn requires perl and App::cpanminus installed. and make
# the LISP could, in theory, be any Common LISP implementation though
# unix argument parsing quickly gets into unportability (CLI-ARGS)
depend:
	cpanm --installdeps .
	sbcl --version

.PHONY: clean depend around.lisp caves.lisp clean corridors.lisp depend linewalker.lisp noise.lisp poisson-disk-sample.lisp room-size-stats.lisp rooms-random.lisp
