; array lookup wins, unless the board size is too large. memory costs
; will likewise need to be profiled (especially for larger board sizes
; or if running on a memory-constrained system).
(defparameter *boardsize*  100)

(defparameter *test-iters* 10000000)

;(defparameter *lookup-hash* (make-hash-table :test 'equal))
(defparameter *lookup-array* (make-array (list *boardsize* *boardsize*)))

(load "../util.lisp")
(proclaim '(optimize speed))
(progn (setq *random-state* (make-random-state t)) t)

;(defun makekey (x y)
;  (format nil "~{~d,~d~}"
;          (if (< x y) (list x y) (list y x))))

(defun sqrt-asq+bsq (a b)
  (sqrt (+ (expt a 2) (expt b 2))))

(dotimes (x *boardsize*)
  (dotimes (y *boardsize*)
    (let ((sq (sqrt-asq+bsq x y)))
;     (sethash *lookup-hash* (makekey x y) sq)
      (setf (aref *lookup-array* x y) sq)
      t)))

'by-math
(time (repeat *test-iters*
              (sqrt-asq+bsq (random *boardsize*) (random *boardsize*))))

; slow!
;'by-hash
;(time (repeat *test-iters*
;              (gethash (makekey (random *boardsize*) (random *boardsize*)) *lookup-hash*)))

'by-array
(time (repeat *test-iters*
                (aref *lookup-array* (random *boardsize*) (random *boardsize*))))
