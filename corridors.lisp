;;;;; draw branching corridors and ensure that they are all connected.
;;;;; probably still too many dead ends (unless you want those)

(defparameter +rows+ 23)
(defparameter +cols+ 79)

; how much to fill the board with corridors (probably want to lowball
; this value, see while loop below)
(defparameter +max-moves+ (truncate (* (* +rows+ +cols+) 0.20)))

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

(defun nudge (agent orig)
  (let ((nudge (add-points orig (random-turn (agent-heading agent)))))
    (if (point-inside? nudge +bounds+)
      (progn
        (draw-at-point nudge *floor*)
        nudge)
      orig)))

(defparameter *total-moves* 0)

; can return itself, or not, or one or more other new agents hopefully
; going off in new directions
(defun update-agent (agent)
  (declare (agent agent))
  (let ((new-point (move-agent agent)))
    (cond ((not (point-inside? new-point +bounds+))
           ; kill at border and generate turned agent to limit dead ends
           ; (ideally this would generate an agent that turns away from
           ; the border, but that's more work)
           (list (make-agent :position (agent-position agent)
                             :heading (random-turn (agent-heading agent)))))
          ; whoops something was already here
          ((eq *floor* (get-point-obj new-point)) nil)
          (t (draw-at-point new-point *floor*)
             (incf *total-moves*)
             (let ((agents nil) (alive t) (moves (agent-moves agent)))
               (when (> moves 2)
                 (if (zerop (random 8))
                   (progn
                     (setf alive nil)
                     ; dead, create turned agent to help limit dead ends
                     (push (make-agent :position new-point
                                       :heading (random-turn
                                                  (agent-heading agent)))
                           agents))
                   (progn
                     ; spawn new agents heading in new directions?
                     (when (zerop (random 6))
                       (let ((dir (random-turn (agent-heading agent))))
                         (push (make-agent :position new-point
                                           :heading dir) agents)
                         (when (zerop (random 6))
                           (push (make-agent :position new-point
                                             :heading (reverse-direction dir))
                                 agents))))
                     ; nudge the corridor to add variety? increased odds
                     ; of this creates less regular corridors
                     (when (zerop (random 3))
                       (setf new-point (nudge agent new-point))))))
               (when alive
                 (setf (agent-position agent) new-point)
                 (incf (agent-moves agent))
                 (push agent agents))
               agents)))))

; loop until enough moves have been made by the agents. this may require
; multiple starting points and may not result in a connected map (this
; may overdraw as we do not want to cut a particular start short and
; increase the risk of isolated segments)
(while (< *total-moves* +max-moves+)
  (let ((start (random-point-inside)))
    (draw-at-point start *floor*)
    (do ((agents (new-agents-at start) (mapcan #'update-agent agents)))
      ((null agents)))))

; ensure everything is connected with some Dijkstra magic
(defparameter *dimap* (make-dimap *board*))
(dimap-calc *dimap*)
(let ((goal-major (random-list-item (dimap-unconnected *dimap*))))
  (dimap-setf-major *dimap* goal-major *dimap-cost-min*)
  (dimap-calc *dimap*)
  (do ((target (dimap-array-coords *dimap* goal-major))
       (unconn (dimap-unconnected *dimap*) (dimap-unconnected *dimap*)))
    ((null unconn))
    (let ((coord (dimap-array-coords *dimap* (random-list-item unconn))))
      ; this routine does a random walk so will fit in when the odds of
      ; a nudge happening is high, but probably not so much with more
      ; regular corridors where a draw-a-straight-line algo might be
      ; more suitable
      (draw-corridor (apply #'make-point coord)
                     (apply #'make-point target)
                     #'(lambda (p)
                         (draw-at-point p *floor*)
                         (dimap-setf-major *dimap*
                                           (point-major p)
                                           *dimap-cost-max*)))
      (dimap-calc *dimap*))))

(no-return (display-board))
