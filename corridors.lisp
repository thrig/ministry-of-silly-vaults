;;;;; draw branching corridors (based on an idea presented on ##rld)

(defparameter +rows+ 23)
(defparameter +cols+ 79)

;;; TWEAK onsets and odds in update-agent can also be fiddled with
(defparameter *total-moves* 0)
(defparameter *max-moves* (truncate (/ (* +rows+ +cols+) 4)))

(load "util")
(load "common")
(progn (setq *random-state* (make-random-state t)) t)
(defparameter *board* (make-board +rows+ +cols+ *wall*))

(defparameter +headings+ '((-1 . 0) (1 . 0) (0 . 1) (0 . -1)))
(defun random-direction () (random-list-item +headings+))

(defstruct agent (position nil) (heading nil) (moves 0))
(defun move-agent (agent)
  (add-points (agent-position agent) (agent-heading agent)))
(defun new-agents-at (point)
  (let ((here point) (agents nil))
    (dolist (there +headings+)
      (push (make-agent :position here :heading there) agents))
    agents))

(defun update-agent (agent)
  (let ((new-point (move-agent agent)) (moves (agent-moves agent)))
    (unless (p-inbounds? new-point) (return-from update-agent))
    (when (eq *floor* (get-point-obj new-point)) (return-from update-agent))
    (draw-at-point new-point *floor*)
    (setf (agent-position agent) new-point)
    (incf (agent-moves agent))
    (incf *total-moves*)
    (when (and (> moves 4) (zerop (random 8))) (return-from update-agent))
    (let ((agents (list agent)))
      (when (and (> moves 2) (zerop (random 5)))
        (push (make-agent :position new-point
                          :heading (random-turn (agent-heading agent))) agents))
      agents)))

(while (< *total-moves* *max-moves*)
  (let ((start (random-point)))
    (draw-at-point start *floor*)
    (do ((agents (new-agents-at start) (mapcan #'update-agent agents)))
      ((or (null agents) (>= *total-moves* *max-moves*)) nil))))

(display-board)
