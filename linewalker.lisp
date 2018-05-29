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

(defparameter *target* nil)
(defparameter *walkers* nil)

(load "util.lisp")
(load "common.lisp")

(progn (setq *random-state* (make-random-state t)) t)

(defparameter *board* (make-board *rows* *cols* *wall*))

;;; the odds of moving on a particular axis depend on the slope of the
;;; line between the walker and its target; on-axis or 45˚ should be a
;;; 100% chance of a move in one or both axes while any other slope will
;;; vary the odds
(defun move-walker (w)
  (let ((deltar (- (point-row *target*) (point-row w)))
        (deltac (- (point-col *target*) (point-col w)))
        (newp (copy-point w)))
    (if (not (zerop deltar))
      (if (< (random *move-odds*)
             (abs (/ deltar (if (zerop deltac) 1 deltac))))
        (set-point-row newp (+ (point-row newp) (sign-of deltar)))))
    (if (not (zerop deltac))
      (if (< (random *move-odds*)
             (abs (/ deltac (if (zerop deltar) 1 deltar))))
        (set-point-col newp (+ (point-col newp) (sign-of deltac)))))
    (draw-at-point newp *floor*)))

;;; this version suggested by redblobgames. see
;;; https://www.redblobgames.com/grids/line-drawing.html for info on
;;; moving things in somewhat less random ways. this moves only either
;;; by row or by column
(defun move-walker-1 (w)
  (let ((deltar (- (point-row *target*) (point-row w)))
        (deltac (- (point-col *target*) (point-col w)))
        (newp (copy-point w)))
    (if (< (random 1.0) (/ (abs deltar) (+ (abs deltar) (abs deltac))))
      (set-point-row newp (+ (point-row newp) (sign-of deltar)))
      (set-point-col newp (+ (point-col newp) (sign-of deltac))))
    (draw-at-point newp *floor*)))

;;; and an improved diagonal move version also suggested by redblobgames
(defun move-walker-2 (w)
  (let* ((deltar (- (point-row *target*) (point-row w)))
         (deltac (- (point-col *target*) (point-col w)))
         (newp (copy-point w))
         (nsteps (max (abs deltar) (abs deltac))))
    (when (< (random nsteps) (abs deltar))
      (set-point-row newp (+ (point-row newp) (sign-of deltar))))
    (when (< (random nsteps) (abs deltac))
      (set-point-col newp (+ (point-col newp) (sign-of deltac))))
    (draw-at-point newp *floor*)))

(defun update-walker (w)
  (if (same-point w *target*) nil (list (move-walker-2 w))))

(dotimes (n *trials*)
  (setq *walkers* (n-random-points (1+ *walker-count*)
                                   :method #'random-point-inside))
  (setq *target* (first *walkers*))
  (setq *walkers* (rest *walkers*))
  (while (not (null *walkers*))
         (setq *walkers* (mapcan #'update-walker *walkers*)))
  ; this seems a beneficial addition to vary the number of walkers
  (setq *walker-count* (max 1 (isqrt *walker-count*))))

(fresh-line)
(display-board)

;;; there may also be a need to put more thought into the placement of
;;; the targets, e.g. not close to one another
