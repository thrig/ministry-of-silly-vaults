CFLAGS+=-std=c99 -lm
#CFLAGS+=-g

# this in turn requires perl and App::cpanminus installed. And, make.
depend:
	@cpanm --installdeps .

termlbrot: termlbrot.c

clean:
	@-rm -f termlbrot >/dev/null 2>&1
