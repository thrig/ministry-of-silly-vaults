;;;;; common routines used by linewalker.lisp noise.lisp wallwalker.lisp

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

(defun draw-at (row col obj)
  (setf (aref *board* row col)
        (if (functionp obj) (funcall obj) obj))
  (list row col))
(defun draw-at-point (point obj)
  (setf (aref *board* (point-row point) (point-col point))
        (if (functionp obj) (funcall obj) obj))
  point)

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

(defun p-inbounds? (point &optional (rows *rows*) (cols *cols*))
  (not (cond ((< (point-row point) 0))
             ((< (point-col point) 0))
             ((>= (point-row point) rows))
             ((>= (point-col point) cols)))))

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

(defun n-random-points (n &key (rows *rows*) (cols *cols*)
                          (method #'random-point))
  (do ((points nil))
    ((= n (list-length points)) points)
    (pushnew (funcall method rows cols) points :test #'equal)))

(defun same-point (p1 p2) (equal p1 p2))
