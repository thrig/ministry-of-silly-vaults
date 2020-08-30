; recursive descent room-maker (via a slightly buggy maze algo)

(defparameter +rows+ 23)
(defparameter +cols+ 79)

(load "util")
(load "common")

(randomize)

(defparameter *board* (make-board +rows+ +cols+ *floor*))
(defparameter *rooms* nil)

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

(defun colomnize-horiz (row col height width ch)
  (when (oddp width)
    (let ((r1 (1+ row)) (r2 (+ row (- height 2))))
      (loop for ccc from (1+ col) to (1- (+ col width)) by 2
            do (draw-at r1 ccc ch) (draw-at r2 ccc ch))
      (let ((delta (- r2 r1)))
        (when (and (> delta 3) (evenp delta))
          (loop for rrr from (+ r1 2) to (- r2 2) by 2
                do (loop for ccc from (1+ col) to (1- (+ col width)) by 2
                         do (draw-at rrr ccc ch)))))
      t)))

(defun colomnize-vert (row col height width ch)
  (when (oddp height)
    (let ((c1 (1+ col)) (c2 (+ col (- width 2))))
      (loop for rrr from (1+ row) to (1- (+ row height)) by 2
            do (draw-at rrr c1 ch) (draw-at rrr c2 ch))
      (let ((delta (- c2 c1)))
        (when (and (> delta 3) (evenp delta))
          (loop for ccc from (+ c1 2) to (- c2 2) by 2
                do (loop for rrr from (1+ row) to (1- (+ row height)) by 2
                         do (draw-at rrr ccc ch)))))
      t)))

; decorate big rooms with columns, where possible
(defun colomnize (row col height width &optional (ch #\BLACK_SMALL_DIAMOND))
  (when (and (> height 4) (> width 4))
    (if (> height width)
        (colomnize-vert row col height width ch)
        (colomnize-horiz row col height width ch))))

; sometimes close off rooms with only one door leading into them
(defun maybe-fill-in (row col height width)
  (let ((doors 0)
        (border
         (make-rectangle (make-point (1- row) (1- col))
          (make-point (+ row height) (+ col width)))))
    (block door-count
      (do-rect-ring border
       (lambda (r c)
         (unless (eq (aref *board* r c) *wall*)
           (when (> (setf doors (1+ doors)) 1) (return-from door-count))))))
    (when (and (= doors 1) (odds-ninm 75 100))
      (do-rect-ring border (lambda (r c) (setf (aref *board* r c) *wall*))))))

; the odds here (plus depth) helps control rooms from getting too small
; (as does the random skew on the wall location) and may need tweaks to
; not create overly large rooms
(defun recdiv
       (row col height width &optional (min-row 3) (min-col 3) (depth 0))
  (if (and (> height min-row) (> width min-col)
           (or (< depth 4) (odds-ninm (+ 60 (* 5 depth)) 100)))
      (if (> height width)
          (recdiv-horiz row col height width min-row min-col depth)
          (recdiv-vert row col height width min-row min-col depth))
      (unless (colomnize row col height width)
        (push (list row col height width) *rooms*))))

(recdiv 1 1 (- +rows+ 2) (- +cols+ 2))
(do-rect-ring
 (make-rectangle (make-point 0 0) (make-point (1- +rows+) (1- +cols+)))
 (lambda (r c) (draw-at r c *wall*)))
(dolist (room *rooms*)
  (apply #'maybe-fill-in room))
(no-return (display-board))
