;;;;; Dijkstra Maps implementation
;;;;; http://www.roguebasin.com/index.php?title=The_Incredible_Power_of_Dijkstra_Maps

(defvar *dimap-cost-max*  MOST-POSITIVE-FIXNUM)
(defvar *dimap-cost-min*  0)
(defvar *dimap-cost-bad* -1)

;;; to convert from array of characters given a character array
;;; #2A((#\. #\# #\x) (#\# #\. #\.) (#\. #\. #\.)) for a level map
;;;   .#x
;;;   #..
;;;   ...
(defun dimap-costs (c)
  (cond ((eq c #\#) *dimap-cost-bad*)
        ((eq c #\x) *dimap-cost-min*)
        (t *dimap-cost-max*)))
(defun make-dimap (map &optional (costfn #'dimap-costs))
  (let* ((dimap (make-array (array-dimensions map) :element-type 'fixnum)))
    (dotimes (x (array-total-size dimap))
      (setf (row-major-aref dimap x)
            (funcall costfn (row-major-aref map x))))
    dimap))

(defun dimap-reset (dimap)
  (dotimes (x (array-total-size dimap))
    (when (>= (row-major-aref dimap x) *dimap-cost-min*)
      (setf (row-major-aref dimap x) *dimap-cost-max*))))

;;; these are directly adjacent squares, no diagonal motion
(defun dimap-adjacent-squares (aref coords)
  (flet
    ((all-adjacents (coords ranks)
       (let ((ret nil) (tmp nil))
         (dotimes (i ranks)
           (dolist (adjacent '(-1 1))
             (setf tmp (copy-list coords))
             (setf (nth i tmp) (+ adjacent (nth i tmp)))
             (push tmp ret)))
         ret)))
    (let ((len (list-length coords)))
      (or (eq len (array-rank aref))
          (error "coordinate list must equal array rank"))
      (mapcan (lambda (c) (and (apply #'array-in-bounds-p (cons aref c))
                               (list c)))
              (all-adjacents coords len)))))

;;; (hopefully) translate the row-major number to a list of array index
(defun dimap-array-coords (aref n)
  (let ((coords nil))
    (dolist (size (nreverse (array-dimensions aref)))
      (push (mod n size) coords)
      (setf n (/ (- n (first coords)) size)))
    coords))

(defun dimap-calc (dimap)
  (do ((iters 0 (1+ iters)) (done nil)) (done iters)
    (setf done t)
    (dotimes (n (array-total-size dimap))
      (let ((value (row-major-aref dimap n)))
        (unless (<= value *dimap-cost-min*)
          (let* ((coords (dimap-array-coords dimap n))
                 (adj (dimap-adjacent-squares dimap coords))
                 (costs (mapcan (lambda (x) (and (> x *dimap-cost-bad*)
                                                 (list x)))
                                (mapcar (lambda (c)
                                          (apply #'aref
                                                 (cons dimap c))) adj))))
            (unless (null costs)
              (let ((min (apply #'min costs)))
                (when (> value (+ 2 min))
                  (setf (row-major-aref dimap n) (1+ min))
                  (setf done nil))))))))))

; (coords . value) pairs for adjancent squares with lower values (if
; any) from the given major-aref
(defun dimap-next (dimap n)
  (and (>= n (array-total-size dimap))
       (error "index out of bounds for array"))
  (let ((value (row-major-aref dimap n)))
    (if (<= value *dimap-cost-min*)
      nil
      (let* ((coords (dimap-array-coords dimap n))
             (adj (dimap-adjacent-squares dimap coords)))
        (mapcan (lambda (cv) (and (< (cdr cv) value)
                                  (> (cdr cv) *dimap-cost-bad*)
                                  (list cv)))
                (mapcar (lambda (c)
                          (cons c (apply #'aref
                                         (cons dimap c)))) adj))))))

(defun dimap-path (dimap n pickfn)
  (and (>= n (array-total-size dimap))
       (error "index out of bounds for array"))
  (do ((path nil) (options (dimap-next dimap n) (dimap-next dimap n)))
    ((null options) path)
    (push (setf n (apply #'array-row-major-index
                         (cons dimap (car (funcall
                                            pickfn options))))) path)))

(defun dimap-unconnected (dimap)
  (let ((unconn nil))
    (dotimes (n (array-total-size dimap))
      (and (eq (row-major-aref dimap n) *dimap-cost-max*)
           (push n unconn)))
    unconn))

; four dimensions? ¡no problema!
;(defparameter level
;  (make-array '(3 3 3 3)
;               :initial-contents
;               '((((99 -1 99) (-1 99 -1) (99 -1 99))
;                  ((-1 99 99) (99 99 99) (99 99 -1))
;                  ((99 99 99) (99 99 99) (99 99 99)))
;                 (((-1 99 99) (99 99 99) (99 99 -1))
;                  ((99 99 99) (99  0 99) (99 99 99))
;                  ((99 99 99) (99 99 99) (99 99 99)))
;                 (((99 99 99) (99 99 99) (99 99 99))
;                  ((99 99 99) (99 99 99) (99 99 99))
;                  ((99 99 99) (99 99 99) (99 99 99))))))
;(dimap-calc level)
;(format t "~a~%" level)
;(setf *dimap-cost-max* 99)
;(format t "~a~%" (dimap-unconnected level))
