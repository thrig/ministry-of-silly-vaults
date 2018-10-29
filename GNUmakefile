%.fasl: %.lisp
	sbcl --noinform --non-interactive --eval '(compile-file (second *posix-argv*))' $<
