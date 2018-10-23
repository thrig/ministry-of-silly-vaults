;;;;; common routines used by various other scripts

(load "util")

(defvar *board* nil)

(defvar *floor* #\.)
(defvar *door*  #\+)
(defvar *wall*  #\#)

(defvar +rows+ 10)
(defvar +cols+ 20)

(declaim (inline copy-point make-point point-row point-col
                 set-point set-point-row set-point-col
                 draw-at draw-at-point random-point
                 get-obj-at random-between p-inbounds?))

;;; these are '(0 . 0) points (row . col)
(defun copy-point (point) (cons (car point) (cdr point)))
(defun make-point (row col)
  (declare (optimize (speed 3)) (fixnum row col))
  (cons row col))
(defun point-row (point) (car point))
(defun point-col (point) (cdr point))
(defun set-point (point row col)
  (declare (optimize (speed 3)) (fixnum row col)) 
  (rplaca point row) (rplacd point col))
(defun set-point-row (point value)
  (declare (optimize (speed 3)) (fixnum value)) 
  (rplaca point value))
(defun set-point-col (point value)
  (declare (optimize (speed 3)) (fixnum value)) 
  (rplacd point value))

(defun make-rectangle (p1 p2)
  (when (> (car p1) (car p2)) (rotatef (car p1) (car p2)))
  (when (> (cdr p1) (cdr p2)) (rotatef (cdr p1) (cdr p2)))
  (cons p1 p2))

(defun rect-p1 (rect) (car rect))
(defun rect-p2 (rect) (cdr rect))
(defun rect-xys (rect) (values (caar rect) (cdar rect) (cadr rect) (cddr rect)))

(defun rect-overlap? (r1 r2 &optional (row-shift 0) (col-shift 0))
  (cond
    ((or (> (- (point-row (rect-p1 r1)) row-shift) (point-row (rect-p2 r2)))
         (> (- (point-row (rect-p1 r2)) row-shift) (point-row (rect-p2 r1)))) nil)
    ((or (> (- (point-col (rect-p1 r1)) col-shift) (point-col (rect-p2 r2)))
         (> (- (point-col (rect-p1 r2)) col-shift) (point-col (rect-p2 r1)))) nil)
    (t t)))
(defun rect-inside-overlap? (r1 r2) (rect-overlap? r1 r2 -1 -1))

(defun point-inside? (point rect)
  (and
    (>= (car point) (caar rect))
    (<= (car point) (cadr rect))
    (>= (cdr point) (cdar rect))
    (<= (cdr point) (cddr rect))))

(defun add-points (p1 &rest points)
  (let ((np (copy-point p1)))
    (dolist (p2 points)
      (set-point-row np (+ (point-row np) (point-row p2)))
      (set-point-col np (+ (point-col np) (point-col p2))))
    np))

(defun draw-at (row col obj)
  (declare (optimize (speed 3)) (fixnum row col))
  (setf (aref *board* row col)
        (if (functionp obj) (funcall obj) obj))
  (list row col))
(defun draw-at-point (point obj)
  (setf (aref *board* (point-row point) (point-col point))
        (if (functionp obj) (funcall obj) obj))
  point)

(defun draw-corridor (p1 p2 obj)
  (draw-at-point p1 obj)
  (while (not (same-point p1 p2))
    (let ((deltar (- (point-row p2) (point-row p1)))
          (deltac (- (point-col p2) (point-col p1))))
      (declare (optimize (speed 3)) (fixnum deltar deltac))
      (if (< (random 1.0) (/ (abs deltar) (+ (abs deltar) (abs deltac))))
        (set-point-row p1 (+ (point-row p1) (sign-of deltar)))
        (set-point-col p1 (+ (point-col p1) (sign-of deltac))))
      (draw-at-point p1 obj))))

(defun draw-horiz (row col count obj)
  (declare (optimize (speed 3)) (fixnum row col count))
  (do ((iters (abs count) (1- iters))
       (nudge (if (plusp count) 1 -1)))
    ((or (zerop iters)
         (not (array-in-bounds-p *board* row col))) nil)
    (draw-at row col obj)
    (setf col (+ col nudge))))

(defun draw-vert (row col count obj)
  (declare (optimize (speed 3)) (fixnum row col count))
  (do ((iters (abs count) (1- iters))
       (nudge (if (plusp count) 1 -1)))
    ((or (zerop iters)
         (not (array-in-bounds-p *board* row col))) nil)
    (draw-at row col obj)
    (setf row (+ row nudge))))

(defun do-rect (rect fn)
  (loop for r from (caar rect) to (cadr rect) do
        (loop for c from (cdar rect) to (cddr rect) do
              (funcall fn r c))))
; these require at minumum a
; ###
; #.#
; ### type rectangle that has distinct inner and outer parts
(defun do-rect-inside (rect fn)
  (multiple-value-bind (x1 y1 x2 y2) (rect-xys rect)
    (when (and
            (> (- x2 x1) 1)
            (> (- y2 y1) 1))
      (loop for r from (1+ x1) to (1- x2) do
            (loop for c from (1+ y1) to (1- y2) do
              (funcall fn r c))))))
(defun do-rect-ring (rect fn)
  (multiple-value-bind (x1 y1 x2 y2) (rect-xys rect)
    (when (and
            (> (- x2 x1) 1)
            (> (- y2 y1) 1))
      (loop for r from (1+ x1) to (1- x2) do
            (funcall fn r y1)
            (funcall fn r y2))
      (loop for c from y1 to y2 do
            (funcall fn x1 c)
            (funcall fn x2 c)))))
; corners of rooms may require special designation
(defun do-rect-corner (rect fn)
  (multiple-value-bind (x1 y1 x2 y2) (rect-xys rect)
    (funcall fn x1 y1)
    (funcall fn x2 y2)
    (funcall fn x1 y2)
    (funcall fn x2 y1)))

(defun get-obj-at (row col)
  (declare (optimize (speed 3)) (fixnum row col))
  (aref *board* row col))
(defun get-point-obj (point)
  (aref *board* (point-row point) (point-col point)))

(defun add-border (&optional (obj *wall*))
  (no-return
    (loop for r from 0 to (1- +rows+) do
          (draw-at r 0 obj)
          (draw-at r (1- +cols+) obj))
    (loop for c from 1 to (- +cols+ 2) do
          (draw-at 0 c obj)
          (draw-at (1- +rows+) c obj))))

;;; not very interesting nor efficient
(defun boundary-fill (row col fill limit)
  (declare (optimize (speed 3)) (fixnum row col))
  (when (array-in-bounds-p *board* row col)
    (let ((value (get-obj-at row col)))
      (when (and (not (eq value fill))
                 (not (eq value limit)))
        (draw-at row col fill)
        (boundary-fill (1+ row) col fill limit)
        (boundary-fill (1- row) col fill limit)
        (boundary-fill row (1+ col) fill limit)
        (boundary-fill row (1- col) fill limit)))))

(defun clear-board (&optional (obj *floor*))
  (no-return
    (dotimes (r +rows+)
      (dotimes (c +cols+)
        (draw-at r c obj)))))

(defun display-board ()
  (no-return
    (dotimes (r +rows+)
      (dotimes (c +cols+)
        (format t "~c" (aref *board* r c)))
      (fresh-line))))

(defun make-board (rows cols &optional (obj *floor*))
  (declare (optimize (speed 3)) (fixnum rows cols))
  (make-array (list rows cols)
              :element-type 'character
              :initial-element (if (functionp obj) (funcall obj) obj)))

(defun p-inbounds? (point)
  (array-in-bounds-p *board* (point-row point) (point-col point)))

(defun rect-inbounds? (rect)
  (and (p-inbounds? (rect-p1 rect))
       (p-inbounds? (rect-p2 rect))))

(defun points-adjacent-to (point)
  (let ((points nil) (tmp nil))
    (dolist (row '(1 0 -1))
      (dolist (col '(1 -1))
        (setf tmp (make-point (+ (point-row point) row)
                              (+ (point-col point) col)))
        (when (p-inbounds? tmp) (push tmp points))))
    (dolist (row '(1 -1))
        (setf tmp (make-point (+ (point-row point) row)
                              (point-col point)))
        (when (p-inbounds? tmp) (push tmp points)))
    points))

(defun points-diagonal-to (point)
  (let ((points nil) (tmp nil))
    (dolist (row '(1 -1))
      (dolist (col '(1 -1))
        (setf tmp (make-point (+ (point-row point) row)
                              (+ (point-col point) col)))
        (when (p-inbounds? tmp) (push tmp points))))
    points))

(defun points-square-to (point)
  (let ((points nil) (tmp nil))
    (dolist (offset '(1 -1))
      (setf tmp (make-point (+ (point-row point) offset) (point-col point)))
      (when (p-inbounds? tmp) (push tmp points))
      (setf tmp (make-point (point-row point) (+ (point-col point) offset)))
      (when (p-inbounds? tmp) (push tmp points)))
    points))

; r <= min .. r < max
(defun random-between (min max)
  (declare (optimize (speed 3)) (fixnum min max))
  (when (> min max) (rotatef min max))
  (if (= min max) min
    (let ((len (- max min)))
      (+ min (random len)))))

(defun random-point (&optional (rows +rows+) (cols +cols+))
  (declare (optimize (speed 3)) (fixnum rows cols))
  (make-point (random rows) (random cols)))

(defun random-point-around (p mindist)
  (let* ((r1 (random 1.0))
         (r2 (random 1.0))
         (radius (* mindist (1+ r1)))
         (angle (* 2 pi r2)))
    (make-point
      (round (- (point-row p) (* radius (sin angle))))
      (round (+ (point-col p) (* radius (cos angle)))))))

(defun random-point-inside (&optional (rows +rows+) (cols +cols+))
  (declare (optimize (speed 3)) (fixnum rows cols))
  (make-point (1+ (random (- rows 2)))
              (1+ (random (- cols 2)))))

(defun random-turn (point)
  (let ((new-point (copy-point point)))
    (if (eq (point-row new-point) 0)
      (set-point-row new-point (if (coinflip) 1 -1))
      (set-point-row new-point 0))
    (if (eq (point-col new-point) 0)
      (set-point-col new-point (if (coinflip) 1 -1))
      (set-point-col new-point 0))
    new-point))

(defun reverse-direction (point)
  (let ((new-point (copy-point point)))
    (set-point-row new-point (reverse-magnitude (point-row new-point)))
    (set-point-col new-point (reverse-magnitude (point-col new-point)))
    new-point))

; TODO needs testing and may be biased, is there a better way to pick
; legal rectanges of particular dimensions within a given rectangle?
(defun random-rect (min-rows min-cols max-rows max-cols
                    &key (border 0) (rows +rows+) (cols +cols+))
  (declare (optimize (speed 3))
           (fixnum min-rows min-cols max-rows max-cols border rows cols))
  (let ((2x-border (* 2 border)))
    (when (or (> (+ 2x-border min-rows) rows)
              (> (+ 2x-border min-cols) cols))
      (error "board too small for given inputs"))
    (let* ((p1 (make-point
                 (if (eq rows min-rows) 0
                   (+ border (random (- rows -1 min-rows 2x-border))))
                 (if (eq cols min-cols) 0
                   (+ border (random (- cols -1 min-cols 2x-border))))))
           (p2 (make-point
                 (random-between (+ (point-row p1) min-rows -1)
                                 (min (+ (point-row p1) max-rows)
                                      (- rows border)))
                 (random-between (+ (point-col p1) min-cols -1)
                                 (min (+ (point-col p1) max-cols)
                                      (- cols border))))))
      (make-rectangle p1 p2))))

; NOTE returns the list of points high to low, NREVERSE or shuffle the
; points if necessary
(defun n-random-points (n &key (rows (1- +rows+)) (cols (1- +cols+)))
  (declare (optimize (speed 3)) (fixnum n rows cols))
  (do* ((points nil)
        (total (* rows cols) (1- total))
        (left (min n total))
        (r 0) (c 0))
    ((= 0 left) points)
    (when (< (random 1.0) (/ left total))
      (push (make-point r c) points)
      (decf left))
    (if (= cols c)
      (progn (setf c 0) (incf r))
      (incf c))))

(defun row-walk (row col fn)
  (declare (optimize (speed 3)) (fixnum row col))
  (do ((results nil) (r row (1+ r)))
    ((>= r +rows+) results)
    (let ((ret (funcall fn r col)))
      (if (null ret)
        (return-from row-walk results)
        (push ret results)))))

(defun col-walk (row col fn)
  (declare (optimize (speed 3)) (fixnum row col))
  (do ((results nil) (c col (1+ c)))
    ((>= c +cols+) results)
    (let ((ret (funcall fn row c)))
      (if (null ret)
        (return-from col-walk results)
        (push ret results)))))

(defun same-point (p1 p2) (equal p1 p2))
