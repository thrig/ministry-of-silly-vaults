all:
	@echo "use platform specific rule (or clean)"
	@false

Darwin: libminterm.dylib
OpenBSD: libminterm.so

minterm-darwin.o: minterm.c
	$(CC) -Os -Wall -pedantic -c minterm.c -o minterm-darwin.o
libminterm.dylib: minterm-darwin.o
	$(CC) -fno-common -Wl,-headerpad_max_install_names -headerpad_max_install_names -Wl,-search_paths_first -dynamiclib -prebind -current_version 1.0.0 -compatibility_version 1.0.0 minterm-darwin.o -o libminterm.dylib

minterm-openbsd.o: minterm.c
	$(CC) -O2 -fPIC -Wall -pedantic -c minterm.c -o minterm-openbsd.o
libminterm.so: minterm-openbsd.o
	$(CC) -fpic -shared -Wl,-export-dynamic -Wl,-soname,libminterm.so.1 minterm-openbsd.o -o libminterm.so

clean:
	@-rm -f *.core *.dylib *.o *.so 2>/dev/null
.PHONY: all clean
