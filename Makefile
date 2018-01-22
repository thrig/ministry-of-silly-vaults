CFLAGS+=-std=c99 -lm
#CFLAGS+=-g

crawlpaper: crawlpaper.go
	go build crawlpaper.go

# this in turn requires perl and App::cpanminus installed. And, make.
depend:
	@cpanm --installdeps .
	@go get github.com/fogleman/gg

termlbrot: termlbrot.c

clean:
	@-rm -f termlbrot >/dev/null 2>&1
