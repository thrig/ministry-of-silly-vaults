;;;;; minterm - a wafer-thin terminal library for Common LISP via CFFI
;;;;;
;;;;;   make -C lib `uname`
;;;;;
;;;;; only SBCL has been tested, and also Makefile entries will need to
;;;;; be added for systems beyond Darwin and OpenBSD. see the lib
;;;;; directory for details
;;;;;
;;;;; minterm-eg.lisp uses this library for a minimal usage example

;;; setup quicklisp or install CFFI and dependencies manually
(require :asdf)
(asdf:load-system :cffi)
(defpackage :minterm (:use :common-lisp :cffi))
(in-package :minterm)

;;; KLUGE or instead install the C library to a standard lib directory
;;; (another idea might be to drop the library and just make the C calls
;;; directly from LISP?)
(pushnew #P"./lib/" *foreign-library-directories* :test #'equal)

(define-foreign-library libminterm (t (:default "libminterm")))
(use-foreign-library libminterm)

(defcstruct winsize
  (ws_row :unsigned-short)
  (ws_col :unsigned-short)
  (ws_xpixel :unsigned-short)
  (ws_ypixel :unsigned-short))

(defcfun "rawterm" :void)
(defcfun "readkey" :int)
(defcfun "restore" :void)
(defcfun ("termsize" %termsize) :void (ws :pointer))

; ANSI and XTerm control sequences
(defconstant +alt-screen+   #.(format nil "~C[?1049h" #\Esc))
(defconstant +clear-screen+ #.(format nil "~C[1;1H~C[2J" #\Esc #\Esc))
(defconstant +clear-right+  #.(format nil "~C[K"    #\Esc))
(defconstant +hide-cursor+  #.(format nil "~C[?25l" #\Esc))
(defconstant +hide-pointer+ #.(format nil "~C[>3p"  #\Esc))
(defconstant +show-cursor+  #.(format nil "~C[?25h" #\Esc))
(defconstant +term-norm+    #.(format nil "~C[m"    #\Esc))
(defconstant +unalt-screen+ #.(format nil "~C[?1049l" #\Esc))

(defun at (col row)
  (format t "~C[~d;~dH" #\Esc row col))

; PORTABILITY - implementations love to handle control+c themselves
; which may be bad in the middle of a game or something so that may need
; to be caught as appropriate
(defun getch ()
  (let ((keycode))
    (handler-case
      (restart-case (setf keycode (readkey))
        (inject-control-c () (setf keycode 3)))
      #+SBCL
      (SB-SYS:INTERACTIVE-INTERRUPT () (setf keycode 3)))
    keycode))

(defun termsize ()
  (with-foreign-object (ws 'winsize)
    (%termsize ws)
    (with-foreign-slots ((ws_col ws_row ws_xpixel ws_ypixel) ws winsize)
      (list ws_col ws_row ws_xpixel ws_ypixel))))

(defun termsizep (min-cols min-rows)
  (destructuring-bind (cols rows x y) (termsize)
    (and (>= cols min-cols) (>= rows min-rows))))

(defmacro with-rawterm (&body body)
  `(unwind-protect
     (progn (rawterm) ,@body)
     (restore)))
