;;; random walk that fills a board with walls plus some extra code to
;;; add doors if possible. no attempt is made to ensure that walkable
;;; areas are connected. probably less efficient than the "Cellular
;;; Automata Method for Generating Random Cave-Like Levels" algorithm

(defparameter *rows* 12)
(defparameter *cols* 72)

(defparameter *door* #\+)
(defparameter *edge* #\#)
(defparameter *fill* #\SPACE)

(defparameter *carve-if-surrounded-by* 8)

(defparameter *door-percent* 0.33)
(defparameter *fill-percent* 0.56)

(defparameter *iterations* (round (* (* *rows* *cols*) *fill-percent*)))

(load "util.lisp")
(load "common.lisp")

(progn (setq *random-state* (make-random-state t)) t)

(defparameter *board* (make-board *rows* *cols* *fill*))

; NOTE assumes that one is not working at the edges of the board
(defun peer-count (row col obj)
  (let ((count -1))                     ; self not included in the count
    (dolist (deltar (range -1 1) count)
      (dolist (deltac (range -1 1))
        (and (eq (get-obj-at (+ row deltar) (+ col deltac)) obj)
             (incf count))))))

(defun carve-out-solid-blocks ()
  (let ((tofix nil))
    (loop for r from 1 to (- *rows* 2) do
          (loop for c from 1 to (- *cols* 2) do
                (and (eq (get-obj-at r c) *edge*)
                     (>= (peer-count r c *edge*) *carve-if-surrounded-by*)
                     (push (make-point r c) tofix))))
    ; plant? pudding? just show areas that could be carved out for now
    (loop for p in tofix do
          (draw-at-point p #\P))))

(defun generate-choices (row col)
  (let ((choices nil))
    (dolist (offset '(-1 1))
      (push (make-point (+ row offset) col) choices)
      (push (make-point row (+ col offset)) choices))
    choices))

(defun unoccupied? (point)
  (eq (get-point-obj point) *fill*))

(defun legal-choices (choices)
  (mapcan #'(lambda (point)
              (and (p-inbounds? point)
                   (unoccupied? point)
                   (list point))) choices))

(defun maybe-door (row col)
  (if (< (random 1.0) *door-percent*)
    (draw-at row col *door*)))

(defun add-some-doors ()
  (loop for r from 1 to (- *rows* 2) do
        (loop for c from 1 to (- *cols* 2) do
              (if (eq (get-obj-at r c) *edge*)
                (cond
                  ((and
                     (eq (get-obj-at (1- r) c) *fill*)
                     (eq (get-obj-at (1+ r) c) *fill*)
                     (eq (get-obj-at r (1- c)) *edge*)
                     (eq (get-obj-at r (1+ c)) *edge*))
                   (maybe-door r c))
                  ((and 
                     (eq (get-obj-at (1- r) c) *edge*)
                     (eq (get-obj-at (1+ r) c) *edge*)
                     (eq (get-obj-at r (1- c)) *fill*)
                     (eq (get-obj-at r (1+ c)) *fill*))
                   (maybe-door r c)))))))

;;; this walks the walls as edges around the board, with a jump to some
;;; new start position if necessary
(let ((newpoint nil) (row (random *rows*)) (col (random *cols*)))
  (repeat *iterations*
          (draw-at row col *edge*)
          (setq newpoint (random-list-item
                           (legal-choices
                             (generate-choices row col))))
          (if (null newpoint)
            (progn
              (setq row (random *rows*))
              (setq col (random *cols*)))
            (progn
              (setq row (point-row newpoint))
              (setq col (point-col newpoint))))))

(carve-out-solid-blocks)
(add-some-doors)
(add-border)
(display-board)
