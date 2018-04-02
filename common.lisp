(defun display-board ()
  (dotimes (r *rows*)
    (dotimes (c *cols*)
      (format t "~c" (aref *board* r c)))
    (fresh-line)))

(defun is-in-bounds (point)
  (not (cond ((< (aref point 0) 0))
             ((< (aref point 1) 0))
             ((>= (aref point 0) *rows*))
             ((>= (aref point 1) *cols*)))))

(defun random-point () (list (random *rows*) (random *cols*)))
(defun random-point-inside () (list
                                (1+ (random (- *rows* 2)))
                                (1+ (random (- *cols* 2)))))
