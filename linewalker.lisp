;;;;; draws lines from random starting points (the walkers) to a target
;;;;; point. a more complicated implementation might ensure that no two
;;;;; walkers overlap or that none start on axis with the target

(defparameter *floor* #\.)
(defparameter *wall* #\#)

(defparameter *rows* 23)
(defparameter *cols* 79)

;;; how many times to converge on a random target, and how many walkers
;;; per run converge on the target. connectedness if necessary could be
;;; guaranteed by using the target from a run as a walker in subsequent
;;; runs. another method would be to stop the walkers after N moves and
;;; then change the target, etc
(defparameter *trials* 5)
(defparameter *walker-count* 11)

;;; 1.0 means there is a 100% chance that an on-axis move will happen;
;;; higher values can be used to make 45˚ lines less regular, if less
;;; efficient to calculate. this could also be done post-generation
(defparameter *move-odds* 1.5)

(defparameter *board* (make-array (list *rows* *cols*)
                                  :element-type 'character
                                  :initial-element *wall*))
(defparameter *target* nil)
(defparameter *walkers* nil)

(load "util.lisp")
(load "common.lisp")

(progn (setq *random-state* (make-random-state t)) t)

(defun draw-at (point &optional (icon *floor*))
  (setf (aref *board* (first point) (second point)) icon)
  point)

;;; the odds of moving on a particular axis depend on the slope of the
;;; walker and the target; on-axis or 45˚ should be a 100% chance of a
;;; move in one or both axes while any other slope will vary the odds
(defun move-walker (w)
  (let ((deltar (- (first *target*) (first w)))
        (deltac (- (second *target*) (second w)))
        (newp (copy-list w)))
    (if (not (zerop deltar))
      (if (< (random *move-odds*)
             (abs (/ deltar (if (zerop deltac) 1 deltac))))
        (setf (first newp) (+ (first newp) (if (plusp deltar) 1 -1)))))
    (if (not (zerop deltac))
      (if (< (random *move-odds*)
             (abs (/ deltac (if (zerop deltar) 1 deltar))))
        (setf (second newp) (+ (second newp) (if (plusp deltac) 1 -1)))))
    (draw-at newp)))

(defun same-point (p1 p2)
  (and (= (first p1) (first p2)) (= (second p1) (second p2))))

(defun update-walker (w)
  (if (same-point w *target*) nil (list (move-walker w))))

(loop for n from 1 to *trials* do
      (setq *target* (random-point-inside))
      (setq *walkers* (loop for w from 1 to *walker-count* collect
                            (random-point-inside)))
      (while (not (null *walkers*))
             (setq *walkers* (mapcan #'update-walker *walkers*)))
      ; this seems a beneficial addition to vary the number of walkers
      ;(setq *walker-count* (- *walker-count* 5)))
      (setq *walker-count* (1+ (isqrt *walker-count*))))

(fresh-line)
(display-board)

;;; there may also be a need to put more thought into the placement of
;;; the targets, e.g. not close to one another
