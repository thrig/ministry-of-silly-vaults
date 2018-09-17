;;;;; common routines used by various other scripts

(defvar *board* nil)

(defvar *floor* #\.)
(defvar *door*  #\+)
(defvar *wall*  #\#)

(defvar *rows* 10)
(defvar *cols* 20)

;;; these are '(0 . 0) points (row . col)
(defun copy-point (point) (cons (car point) (cdr point)))
(defun make-point (row col) (cons row col))
(defun point-row (point) (car point))
(defun point-col (point) (cdr point))
(defun set-point (point row col) (rplaca point row) (rplacd point col))
(defun set-point-row (point value) (rplaca point value))
(defun set-point-col (point value) (rplacd point value))

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

(defun point-within? (point rect)
  (and
    (>= (car point) (caar rect))
    (<= (car point) (cadr rect))
    (>= (cdr point) (cdar rect))
    (<= (cdr point) (cddr rect))))

(defun draw-at (row col obj)
  (setf (aref *board* row col)
        (if (functionp obj) (funcall obj) obj))
  (list row col))
(defun draw-at-point (point obj)
  (setf (aref *board* (point-row point) (point-col point))
        (if (functionp obj) (funcall obj) obj))
  point)

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

(defun get-obj-at (row col) (aref *board* row col))
(defun get-point-obj (point)
  (aref *board* (point-row point) (point-col point)))

(defun add-border (&optional (obj *wall*))
  (no-return
    (loop for r from 0 to (1- *rows*) do
          (draw-at r 0 obj)
          (draw-at r (1- *cols*) obj))
    (loop for c from 1 to (- *cols* 2) do
          (draw-at 0 c obj)
          (draw-at (1- *rows*) c obj))))

(defun clear-board (&optional (obj *floor*))
  (no-return
    (dotimes (r *rows*)
      (dotimes (c *cols*)
        (draw-at r c obj)))))

(defun display-board ()
  (no-return
    (dotimes (r *rows*)
      (dotimes (c *cols*)
        (format t "~c" (aref *board* r c)))
      (fresh-line))))

(defun make-board (rows cols &optional (obj *floor*))
  (make-array (list rows cols)
              :element-type 'character
              :initial-element (if (functionp obj) (funcall obj) obj)))

(defun p-inbounds? (point)
  (array-in-bounds-p *board* (point-row point) (point-col point)))

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
  (when (> min max) (rotatef min max))
  (if (= min max) min
    (let ((len (- max min)))
      (+ min (random len)))))

(defun random-point (&optional (rows *rows*) (cols *cols*))
  (make-point (random rows)
              (random cols)))

(defun random-point-around (p mindist)
  (let* ((r1 (random 1.0))
         (r2 (random 1.0))
         (radius (* mindist (1+ r1)))
         (angle (* 2 pi r2)))
    (make-point
      (round (- (point-row p) (* radius (sin angle))))
      (round (+ (point-col p) (* radius (cos angle)))))))

(defun random-point-inside (&optional (rows *rows*) (cols *cols*))
  (make-point (1+ (random (- rows 2)))
              (1+ (random (- cols 2)))))

; NOTE returns the list of points high to low, NREVERSE or shuffle the
; points if necessary
(defun n-random-points (n &key (rows (1- *rows*)) (cols (1- *cols*)))
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
  (do ((results nil) (r row (1+ r)))
    ((>= r *rows*) results)
    (let ((ret (funcall fn r col)))
      (if (null ret)
        (return-from row-walk results)
        (push ret results)))))

(defun col-walk (row col fn)
  (do ((results nil) (c col (1+ c)))
    ((>= c *cols*) results)
    (let ((ret (funcall fn row c)))
      (if (null ret)
        (return-from col-walk results)
        (push ret results)))))

(defun same-point (p1 p2) (equal p1 p2))
