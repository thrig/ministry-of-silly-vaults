;;;;; draw branching corridors and ensure that they are all connected.
;;;;; probably still too many dead ends (unless you want those)

(defparameter +rows+ 23)
(defparameter +cols+ 79)

; how much to fill the board with corridors
(defparameter +max-moves+ (truncate (* (* +rows+ +cols+) 0.25)))

(load "util")
(load "common")
(load "dijkstramap")
(proclaim '(optimize speed))
(progn (setq *random-state* (make-random-state t)) t)

(defparameter *board* (make-board +rows+ +cols+ *wall*))
; do not draw to the edge of the board
(defparameter +bounds+
  (make-rectangle (make-point 1 1) (make-point (- +rows+ 2) (- +cols+ 2))))

(defparameter +headings+ '((-1 . 0) (1 . 0) (0 . 1) (0 . -1)))
(defun random-direction () (random-list-item +headings+))

(defstruct agent (position nil) (heading nil) (moves 0))
(defun move-agent (agent)
  (declare (agent agent))
  (the cons (add-points (agent-position agent) (agent-heading agent))))
; always four agents from a starting point
(defun new-agents-at (point)
  (declare (cons point))
  (mapcar (lambda (h) (make-agent :position point :heading h)) +headings+))

(defparameter *total-moves* 0)

(defun update-agent (agent)
  (declare (agent agent))
  (let ((new-point (move-agent agent)))
    ; kill off the agent if it moves outside the bounds or to a square
    ; that already has been moved to, otherwise possible move, kill, or
    ; spawn new agents, maybe
    (cond ((not (point-inside? new-point +bounds+))
           ; kill at border, turn to limit dead ends at borders
           (list (make-agent :position (agent-position agent)
                             :heading (random-turn (agent-heading agent)))))
          ((eq *floor* (get-point-obj new-point)) nil)
          (t (draw-at-point new-point *floor*)
             (incf *total-moves*)
             (let ((agents nil) (moves (agent-moves agent)))
               (when (and (> moves 2) (zerop (random 6)))
                 (let ((newdir (random-turn (agent-heading agent))))
                   (push (make-agent :position new-point
                                     :heading newdir) agents)
                   (when (zerop (random 6))
                     (push (make-agent :position new-point
                                       :heading (reverse-direction newdir))
                           agents))))
               (if (or (< moves 2) (plusp (random 6)))
                 ; not dead yet: move and register as alive
                 (progn
                   (setf (agent-position agent) new-point)
                   (incf (agent-moves agent))
                   (push agent agents))
                 ; dead. create a new agent turned to limit dead ends.
                 ; someone on chat complainint about too many dead ends
                 ; in a previous version of this code
                 (push (make-agent :position new-point
                                   :heading (random-turn
                                              (agent-heading agent)))
                       agents))
               agents)))))

; loop until enough moves have been made by the agents. this may require
; multiple starting points and may not result in a connected map
(while t
  (let ((start (random-point-inside)))
    (draw-at-point start *floor*)
    (do ((agents (new-agents-at start) (mapcan #'update-agent agents)))
      ((null agents) nil)
      (and (>= *total-moves* +max-moves+) (return-from while)))))

; ensure everything is connected by drawing horizontal lines with a
; random point in each unconnected area (kluge)
(defparameter *dimap* (make-dimap *board*))
(dimap-calc *dimap*)
(setf (row-major-aref *dimap*
                      (random-list-item (dimap-unconnected *dimap*)))
      *dimap-cost-min*)
(dimap-calc *dimap*)
(do ((unconn (dimap-unconnected *dimap*) (dimap-unconnected *dimap*)))
  ((null unconn) nil)
  (let ((coord (dimap-array-coords *dimap* (random-list-item unconn))))
    (do ((row (first coord))
         (col 2 (1+ col))
         (max-col (- +cols+ 2))
         (drawing nil))
      ((eq col max-col) nil)
      (if drawing
        (progn
          (draw-at row col *floor*)
          (setf (aref *dimap* row col) *dimap-cost-max*))
        (when (eq *floor* (get-obj-at row col)) (setf drawing t)))
    (dimap-calc *dimap*))))

(display-board)
