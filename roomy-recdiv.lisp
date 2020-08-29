; recursive descent room-maker (via a slightly buggy maze algo)

(defparameter +rows+ 23)
(defparameter +cols+ 80)

(load "util")
(load "common")

(randomize)

(defparameter *board* (make-board +rows+ +cols+ *floor*))

; NOTE use (- len 2) if you use RANDOM
(defun wall-loc (len)
  (1+ (skew-dist 0 (- len 3) :dice 3)))

(defun recdiv-horiz (row col height width min-row min-col depth)
  (let ((div-row (wall-loc height)) (door-col (random width)))
    (draw-horiz (+ row div-row) col width *wall*)
    (draw-at (+ row div-row) (+ col door-col) *door*)
    ; sometimes add extra doors especially on long walls to create loops
    ; in the maze
    (when (and (< depth 2) (odds-ninm 90 100))
      (draw-at (+ row div-row) (+ col (random width)) #\'))
    (recdiv row col div-row width min-row min-col (1+ depth))
    (let ((new-row (1+ div-row)))
      (recdiv (+ row new-row) col (- height new-row) width min-row min-col
       (1+ depth)))))

(defun recdiv-vert (row col height width min-row min-col depth)
  (let ((div-col (wall-loc width)) (door-row (random height)))
    (draw-vert row (+ col div-col) height *wall*)
    (draw-at (+ row door-row) (+ col div-col) *door*)
    (when (and (< depth 3) (odds-ninm 95 100))
      (draw-at (+ row (random height)) (+ col div-col) #\'))
    (recdiv row col height div-col min-row min-col (1+ depth))
    (let ((new-col (1+ div-col)))
      (recdiv row (+ col new-col) height (- width new-col) min-row min-col
       (1+ depth)))))

; the odds here (plus depth) helps control rooms from getting too small
; (as does the random skew on the wall location) and may need tweaks to
; not create overly large rooms
(defun recdiv
       (row col height width
        &optional (min-row 3) (min-col 3) (depth 0))
  (when
      (and (> height min-row) (> width min-col)
           (or (< depth 4) (odds-ninm 957 1000)))
    (if (> height width)
        (recdiv-horiz row col height width min-row min-col depth)
        (recdiv-vert row col height width min-row min-col depth))))
  ; could do something here with a completed room if when fails

(recdiv 1 1 (- +rows+ 2) (- +cols+ 2))
(do-rect-ring
 (make-rectangle (make-point 0 0) (make-point (1- +rows+) (1- +cols+)))
 (lambda (r c) (draw-at r c *wall*)))
(no-return (display-board))
