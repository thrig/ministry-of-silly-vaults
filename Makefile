CFLAGS+=-std=c99 -lm
#CFLAGS+=-g

crawlpaper: crawlpaper.go
	go build crawlpaper.go

# this in turn requires perl and App::cpanminus installed. And, make.
depend:
	@cpanm --installdeps .
	@go get github.com/fogleman/gg

noise.pdf: noise.tex
	xelatex -halt-on-error -interaction=batchmode noise.tex

termlbrot: termlbrot.c

clean:
	@-rm -f termlbrot noise.aux noise.log noise.out >/dev/null 2>&1

.PHONY: clean depend
