(defvar *edge* #\?)
(defvar *fill* #\?)

(defun draw-at (row col &optional (icon *edge*))
  (setf (aref *board* row col) icon))

(defun add-a-border (&optional (edge *edge*))
  (loop for r from 0 to (1- *rows*) do
        (draw-at r 0 edge)
        (draw-at r (1- *cols*) edge))
  (loop for c from 1 to (- *cols* 2) do
        (draw-at 0 c edge)
        (draw-at (1- *rows*) c edge)))

(defun clear-board (&optional (fill *fill*))
  (setf *board* (make-array (list *rows* *cols*)
                            :element-type 'character
                            :initial-element fill))
  (values))

(defun display-board ()
  (dotimes (r *rows*)
    (dotimes (c *cols*)
      (format t "~c" (aref *board* r c)))
    (fresh-line))
  (values))

(defun is-in-bounds (point)
  (not (cond ((< (aref point 0) 0))
             ((< (aref point 1) 0))
             ((>= (aref point 0) *rows*))
             ((>= (aref point 1) *cols*)))))

(defun n-random-points (n &optional (rows *rows*) (cols *cols*))
  (do ((points nil))
    ((= n (list-length points)) points)
    (pushnew (cons (random rows) (random cols)) points :test #'equal)))

(defun random-point () (list (random *rows*) (random *cols*)))
(defun random-point-inside () (list
                                (1+ (random (- *rows* 2)))
                                (1+ (random (- *cols* 2)))))
