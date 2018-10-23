;;;;; only considers rooms in isolation so may produce different results
;;;;; than a crowded map that allows for large numbers of retries
;;;;;
;;;;;   sbcl --script room-size-stats.lisp | r-fu colsumm - rows cols

(defparameter +rows+ 24)
(defparameter +cols+ 80)
(defparameter *room-max-row* 8)
(defparameter *room-max-col* 10)
(load "util")
(load "common")
(load "dijkstramap")
(proclaim '(optimize speed))
(progn (setq *random-state* (make-random-state t)) t)

(defun random-room-row (r)
  (+ r (decay :odds 0.11
              :min 3
              :max (min *room-max-row* (- +rows+ r 1)))))

(defun random-room-col (c)
  (+ c (decay :odds 0.07
              :min 3
              :max (min *room-max-col* (- +cols+ c 1)))))

(defun print-room-dimensions ()
  (let* ((p1 (random-point (- +rows+ 3) (- +cols+ 3)))
         (p2 (make-point
               (random-room-row (point-row p1))
               (random-room-col (point-col p1)))))
    (format t "~a ~a~%"
            (- (point-row p2) (point-row p1))
            (- (point-col p2) (point-col p1)))))

(repeat 10000 (print-room-dimensions))
