;;; make arc4random(3) available to Common LISP

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init) (load quicklisp-init)))

(asdf:load-system :cffi)
(defpackage :a4r (:use :common-lisp :cffi))
(in-package :a4r)

(defcfun "arc4random" :uint32)
(defcfun ("arc4random_buf" %a4rbuf) :void
         (buf :pointer) (size :unsigned-long))
(defcfun "arc4random_uniform" :uint32
         (upper :uint32))

(defun arc4random-buf (size)
  (declare (fixnum size))
  (unless (plusp size) (error "size must be positive"))
  (let ((vector (make-array size :element-type '(unsigned-byte 8))))
    (with-foreign-pointer (buf size)
      (%a4rbuf buf size)
      (loop for i from 0 below size
            do (setf (aref vector i) (mem-ref buf :unsigned-char i))))
    vector))
