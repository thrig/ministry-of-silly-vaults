;;; random walk that fills a board with walls plus some extra code to
;;; add doors if possible. no attempt is made to ensure that walkable
;;; areas are connected. probably less efficient than the "Cellular
;;; Automata Method for Generating Random Cave-Like Levels" algorithm

(defparameter *rows* 12)
(defparameter *cols* 72)

(defparameter *door* #\+)
(defparameter *edge* #\#)
(defparameter *fill* #\SPACE)

(defparameter *door-percent* 0.33)
(defparameter *fill-percent* 0.56)
(defparameter *carve-if-surrounded-by* 8)

(defparameter *board* (make-array (list *rows* *cols*)
                                  :element-type 'character
                                  :initial-element *fill*))

(defparameter *iterations* (round (* (* *rows* *cols*) *fill-percent*)))

(load "util.lisp")
(load "common.lisp")

(progn (setq *random-state* (make-random-state t)) t)

(defun add-a-nice-border ()
  (loop for r from 0 to (1- *rows*) do
        (draw-at r 0 *edge*)
        (draw-at r (1- *cols*) *edge*))
  (loop for c from 1 to (- *cols* 2) do
        (draw-at 0 c *edge*)
        (draw-at (1- *rows*) c *edge*)))

(defun add-some-doors ()
  (loop for r from 1 to (- *rows* 2) do
        (loop for c from 1 to (- *cols* 2) do
              (if (eql (aref *board* r c) *edge*)
                (cond
                  ((and
                     (eql (aref *board* (1- r) c) *fill*)
                     (eql (aref *board* (1+ r) c) *fill*)
                     (eql (aref *board* r (1- c)) *edge*)
                     (eql (aref *board* r (1+ c)) *edge*))
                   (maybe-door r c))
                  ((and 
                     (eql (aref *board* (1- r) c) *edge*)
                     (eql (aref *board* (1+ r) c) *edge*)
                     (eql (aref *board* r (1- c)) *fill*)
                     (eql (aref *board* r (1+ c)) *fill*))
                   (maybe-door r c)))))))

(defun carve-out-solid-blocks ()
  (let ((tofix nil))
    (loop for r from 1 to (- *rows* 2) do
          (loop for c from 1 to (- *cols* 2) do
                (and (eql (aref *board* r c) *edge*)
                     (>= (peer-edge-count r c) *carve-if-surrounded-by*)
                     (push (vector r c) tofix))))
    (loop for point in tofix do
          ; plant? pudding? just show areas that could be carved out for now
          (draw-at (aref point 0) (aref point 1) #\P))))

(defun maybe-door (row col)
  (if (< (random 1.0) *door-percent*)
    (draw-at row col *door*)))

(defun draw-at (row col &optional (icon *edge*))
  (setf (aref *board* row col) icon))

(defun generate-choices (row col)
  (let ((choices nil))
    (loop for offset in '(-1 1) do
          (push (vector (+ row offset) col) choices)
          (push (vector row (+ col offset)) choices))
    choices))

(defun is-not-occupied (point)
  (eql (aref *board* (aref point 0) (aref point 1)) *fill*))

(defun legal-choices (choices) 
  (mapcan #'(lambda (p)
              (if (and
                    (is-in-bounds p)
                    (is-not-occupied p))
                (list p))) choices))

; NOTE assumes that one is not working at the edges of the board
; NOTE also assumes that itself is a peer in the count, and deducts that
(defun peer-edge-count (row col)
  (let ((count 0))
    (dotimes (dr 3)
      (dotimes (dc 3)
        (if (eql (aref *board* (+ row dr -1) (+ col dc -1)) *edge*)
          (setq count (1+ count)))))
    (1- count)))

(let ((row (random *rows*)) (col (random *cols*)) (newpoint nil))
  (repeat *iterations*
          (draw-at row col)
          (setq newpoint (random-list-item (legal-choices (generate-choices row col))))
          (if (null newpoint)
            (progn (setq row (random *rows*)) (setq col (random *cols*)))
            (progn (setq row (aref newpoint 0)) (setq col (aref newpoint 1))))))

(add-a-nice-border)
(carve-out-solid-blocks)
(add-some-doors)
(display-board)
