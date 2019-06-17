; minimal example of using minterm.lisp
;
;   make -C lib `uname` && sbcl --script minterm-eg.lisp

(load "minterm")

(defmacro prompt (&rest args)
  `(progn
     (format t ,@args)
     ; SBCL needs this, other implementations may not
     (finish-output)))

(minterm::with-rawterm
  (princ minterm::+clear-screen+)
  (destructuring-bind (cols rows x y) (minterm::termsize)
    (format t "terminal is ~dx~d and ~d,~d pixels" cols rows x y))
  (minterm::at 4 2)
  (prompt "press any key")
  (let ((key (minterm::getch)))
    (minterm::at 4 2)
    (princ minterm::+clear-right+)
    (prompt "got ~a" key)
    ; downside: does not detect numpad keys as those are multi-character
    ; sequences starting with \e, typically (but I don't use the numpad)
    (minterm::getch)
    (minterm::at 1 3)))
