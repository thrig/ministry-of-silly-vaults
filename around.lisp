;;;;; mostly that some maths is correct

(defparameter +rows+ 23)
(defparameter +cols+ 79)

(defparameter *min-distance* 5)

(load "util")
(load "common")

(progn (setq *random-state* (make-random-state t)) t)

(defparameter *board* (make-board +rows+ +cols+))

;;; the negative sin is so the letters go around a unit circle the
;;; visibly "correct" way
;(draw-at 11 39 #\o)
;(defun which-way-is-up ()
;  (do ((i 0 (+ i (/ pi 4)))
;       (n 0 (1+ n)))
;    ((= n 8) t)
;    (draw-at-point
;      (make-point
;        (round (- 11 (* 10 (sin i))))
;        (round (+ 39 (* 10 (cos i)))))
;      (code-char (+ 65 n)))))
;(which-way-is-up)

(defparameter *origin* (make-point 11 22))
(draw-at-point *origin* #\o)
(repeat 100 (draw-at-point (random-point-around *origin* *min-distance*) #\x))

(defparameter *origin* (make-point 11 58))
(draw-at-point *origin* #\o)
(repeat 500 (draw-at-point (random-point-around *origin* *min-distance*) #\x))

(display-board)
