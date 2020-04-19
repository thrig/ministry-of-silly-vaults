;;;;; use trig functions to make a random walls/floor/water variation map

; this will need a big terminal with a small font
(defparameter +rows+ 128)
(defparameter +cols+ 128)

(load "util")
(load "common")
(proclaim '(optimize speed))
(randomize)

(defparameter *board* (make-board +rows+ +cols+ *wall*))

(defvar *water* #\~)

(defparameter rratio (/ pi +rows+))
(defparameter cratio (/ pi +cols+))

; multiple functions are used with different offsets (for variety) and
; multiplications (for fractal goodness)
(defparameter roff1 (random +rows+))
(defparameter roff2 (random +rows+))
(defparameter roff3 (random +rows+))
(defparameter coff1 (random +cols+))
(defparameter coff2 (random +cols+))
(defparameter coff3 (random +cols+))

; these may need to be scaled to the row and column ratios if the map is
; not really square
;(defparameter rmul1 (+ 0 (random 2)))
;(defparameter cmul1 (+ 0 (random 3)))
(defparameter rmul2 (+ 3 (random 3)))
(defparameter cmul2 (+ 4 (random 5)))
(defparameter rmul3 (+ 1 (random 11)))
(defparameter cmul3 (+ 1 (random 7)))

; random within this call adds noise, remove if you don't like that
(defun calc (r c)
  (let* ((val
          (+ (cos (* (* (- r roff1) rratio) (1+ (random 2))))
             (sin (* (* (- c coff1) cratio) (1+ (random 3))))
             (cos (* (* (- r roff2) rratio) rmul2))
             (sin (* (* (- c coff2) cratio) rmul2))
             (cos (* (* (- r roff3) rratio) rmul3))
             (sin (* (* (- c coff3) cratio) rmul3)))))
    (cond ((> val 0.9) *wall*) ((> val -0.9) *floor*) (t *water*))))

(dotimes (r +rows+)
  (dotimes (c +cols+)
    (setf (aref *board* r c) (calc r c))))

(no-return (display-board))
(format t "--~&")
