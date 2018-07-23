;;;;; Voss fractal methods -- similar to the routines of my Music::Voss
;;;;; Perl module at https://github.com/thrig/Music-Voss wherein one may
;;;;; consult references.bib for the source articles

;;; bitchange returns a function that calls the given changefn for any
;;; of count elements when the appropriate bit has changed in an
;;; incremented integer n; the returned function returns the sum of the
;;; changefn results for the count of numbers. an example with three
;;; changers and the roll of a six-sided die on change may help:
;;;
;;;   (defparameter *bc*
;;;     (bitchange 3 (lambda (n x)
;;;                    (declare (ignore n x))
;;;                    (1+ (random 6)))))
;;;   (dotimes (x 10) (format t "~&~a" (funcall *bc*)))
(defun bitchange (count changefn)
  (let ((total count)
        (fn changefn)
        (numbers (make-array count :element-type 'integer))
        (n 0))
    (dotimes (x total)
      (setf (aref numbers x) (funcall fn n x)))
    (lambda ()
      (let ((sum 0))
        (incf n)
        (dotimes (x total)
          (unless (= (ldb (byte 1 x) n) (ldb (byte 1 x) (1- n)))
            (setf (aref numbers x) (funcall fn n x)))
          (setf sum (+ sum (aref numbers x))))
        sum))))

;;; "Musimathics, Vol 1" p.358 based function generator based on the
;;; given exponent (typically 2) and a list of functions to (sometimes)
;;; call depending on a number incremented each call. this version
;;; maintains state
(defun powers (power &rest fns)
  (let* ((base power)
         (flist fns)
         (fcount (list-length fns))
         (numbers (make-array fcount
                              :element-type 'integer
                              :initial-element 0))
         (n 0))
    (lambda ()
      (let ((sum 0))
        (dotimes (x fcount)
          (when (zerop (mod n (expt base x)))
            (setf (aref numbers x) (funcall (nth x flist) n x)))
          (setf sum (+ sum (aref numbers x))))
        (incf n)
        sum))))

(defun powers-stateless (power &rest fns)
  (let* ((base power)
         (flist fns)
         (fcount (list-length fns))
         (n 0))
    (lambda ()
      (let ((sum 0))
        (dotimes (x fcount)
          (when (zerop (mod n (expt base x)))
            (setf sum (+ sum (funcall (nth x flist) n x)))))
        (incf n)
        sum))))

;;; variation on Fourier syntheses only with exponents instead of linear
;;; harmonics. input limits: 0 < r <= 1, 0 < H <= 1 where r controls the
;;; texture of the spectrum and H the self-similarity parameter. unlike
;;; previous functions this function uses floating points and not ints
(defun weierstrass
  (lacunarity hurst-expt count &optional
              (phasefn (lambda (x p k r H N)
                         (declare (ignore x p k r H N)) 0)))
  (let ((r lacunarity) (H hurst-expt) (N count) (fn phasefn))
    (lambda (x &optional (p 0))
      (loop for k below N summing
            (* (expt r (* k H))
               (sin (+ (* pi (expt r (- k)) x)
                       (funcall fn x p k r H N))))))))
