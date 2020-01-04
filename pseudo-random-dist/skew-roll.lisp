; https://github.com/thrig/statslite
(ql:quickload :statslite)
(defpackage :foo (:use :cl :statslite))
(in-package :foo)

(block nil (setq *random-state* (make-random-state t)) (return))

(deftype positive-integer () '(integer 0 *))

; adapted from "Behavioral Mathematics for Game AI" by Dave Mark (p.
; 264), code that skews a distribution of dice rolls by dropping high or
; low rolls from the set (or both)
(defun skew-roll (times sides &key (drop-lo 0) (drop-hi 0))
  (declare (positive-integer drop-lo drop-hi))
  (when (>= (+ drop-lo drop-hi) times)
    (error "cannot drop more elements than will generate"))
  (let ((rolls (sort (loop :repeat times :collecting (random sides)) #'<)))
    (when (plusp drop-hi)
      (rplacd (nthcdr (- times drop-hi 1) rolls) nil))
    (when (plusp drop-lo)
      (setf rolls (nthcdr drop-lo rolls)))
    rolls))

; 3d11
;(skew-roll 3 11)
; 3d11 but skewed
;(skew-roll 4 11 :drop-lo 1)
;(skew-roll 4 11 :drop-hi 1)
; even more skewed
;(skew-roll 5 11 :drop-hi 2)
; mostly to test for logic errors, as drop-lo should not be done first
;(skew-roll 4 11 :drop-lo 1 :drop-hi 2)

; Ibid., p.267 a simplified normal distribution generator with a few
; dice rolls because apparently games don't have the CPU budget for
; perfect stats
(defun skew-dist (min max &key (dice 3) (drop-lo 0) (drop-hi 0) (pinch 0))
  (declare (positive-integer dice drop-lo drop-hi pinch))
  (when (< max min) (error "max ~a less than min ~a" max min))
  (let* ((times (+ dice pinch))
         (range (- max min))
         (sides (floor (+ range times) times))
         (odd (- range (* times (1- sides))))
         (roll
          (reduce #'+
                  (skew-roll (+ times drop-lo drop-hi) sides :drop-lo drop-lo
                   :drop-hi drop-hi))))
    (when (plusp odd) (incf roll (random (1+ odd))))
    (+ roll min)))

; #<STATSLITE 10000 [3.00 18.00] mean 10.48 range 15.00 sd 2.92>
(statslite (loop :repeat 10000 :collecting (skew-dist 3 18)))
(statslite (loop :repeat 10000 :collecting (skew-dist 3 18 :drop-lo 1)))
; #<STATSLITE 10000 [3.00 18.00] mean 12.21 range 15.00 sd 2.84>
