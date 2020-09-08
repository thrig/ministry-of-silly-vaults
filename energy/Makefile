CFLAGS=-std=c99 -O2 -Wall -pedantic -fno-diagnostics-color -fstack-protector-strong -fPIC -fPIE -pipe
#CFLAGS+=-g

energy: energy.c energy.h
	$(CC) $(CFLAGS) energy.c -o energy

depend:
	cpanm --installdeps .

.PHONY: depend
