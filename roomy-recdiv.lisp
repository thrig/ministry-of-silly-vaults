; recursive descent room-maker (via a slightly buggy maze algo)

(defparameter +rows+ 24)
(defparameter +cols+ 80)

(load "util")
(load "common")

(randomize)

(defparameter *board* (make-board +rows+ +cols+ *floor*))

; from pseudo-random-dist/skew-roll.lisp to make the rooms somewhat more
; middling
(defun skew-roll (times sides &key (drop-lo 0) (drop-hi 0))
  (when (>= (+ drop-lo drop-hi) times)
    (error "cannot drop more elements than will generate"))
  (let ((rolls (sort (loop :repeat times :collecting (random sides)) #'<)))
    (when (plusp drop-hi)
      (rplacd (nthcdr (- times drop-hi 1) rolls) nil))
    (when (plusp drop-lo)
      (setf rolls (nthcdr drop-lo rolls)))
    rolls))
(defun skew-dist (min max &key (dice 3) (drop-lo 0) (drop-hi 0) (pinch 0))
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

; this could be modified to not create diagonal doors by not using where
; a door was dropped previously
(defun wall-loc (len prev)
  (declare (ignore prev))
  ; NOTE use (- len 2) if you need something for RANDOM
  (let ((range (- len 3)))
    (1+ (skew-dist 0 range :dice 2))))

(defun recdiv-horiz (row col height width min-row min-col prev)
  (let ((div-row (wall-loc height prev)) (door-col (random width)))
    (draw-horiz (+ row div-row) col width *wall*)
    (draw-at (+ row div-row) (+ col door-col) *door*)
    ; sometimes draw multiple doors, as a (good) player will quickly
    ; notice their lack and will hunt for the door. need a better way to
    ; do this (use the `binpack` method?)
    (when (odds-onein 3)
      (draw-at (+ row div-row) (+ col (random width)) *door*))
    (recdiv row col div-row width min-row min-col door-col)
    (let ((new-row (1+ div-row)))
      (recdiv (+ row new-row) col (- height new-row) width min-row min-col
       door-col))))

(defun recdiv-vert (row col height width min-row min-col prev)
  (let ((div-col (wall-loc width prev)) (door-row (random height)))
    (draw-vert row (+ col div-col) height *wall*)
    (draw-at (+ row door-row) (+ col div-col) *door*)
    (when (odds-onein 3)
      (draw-at (+ row (random height)) (+ col div-col) *door*))
    (recdiv row col height div-col min-row min-col door-row)
    (let ((new-col (1+ div-col)))
      (recdiv row (+ col new-col) height (- width new-col) min-row min-col
       door-row))))

(defun recdiv
       (row col height width &optional (min-row 3) (min-col 3) (prev -1))
  ; this could be checked by the individual routines, especially if the
  ; input is very different than a square?
  ; a "depth" flag would also help so that the odds of making a bigger
  ; room goes up the deeper you get (avoiding the "lucky early roll
  ; creates a huge room" problem)
  (when (and (> height min-row) (> width min-col) (odds-ninm 95 100))
    ; this could instead be done randomly (coinflip is bad though)
    (if (> height width)
      (recdiv-horiz row col height width min-row min-col prev)
      (recdiv-vert row col height width min-row min-col prev))))
  ; could otherwise do something here with a completed room

(recdiv 1 1 (- +rows+ 2) (- +cols+ 2))
(do-rect-ring
 (make-rectangle (make-point 0 0) (make-point (1- +rows+) (1- +cols+)))
 (lambda (r c) (draw-at r c *wall*)))
(no-return (display-board))
