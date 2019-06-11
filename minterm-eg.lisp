; minimal example of using minterm.lisp
;
;   make -C lib `uname` && sbcl --script minterm-eg.lisp

(load "minterm")

(minterm::with-rawterm
  (princ minterm::+clear-screen+)
  (destructuring-bind (cols rows x y) (minterm::termsize)
    (format t "terminal is ~dx~d and ~d,~d pixels" cols rows x y))
  (minterm::at 4 2)
  (format t "press any key to exit")
  ; SBCL needs this, other implementations may not
  (finish-output)
  (minterm::getch)
  (minterm::at 1 3))
