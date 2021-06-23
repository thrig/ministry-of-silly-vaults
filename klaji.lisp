; klaji.lisp - yet another attempt at drawing something like a street
; grid. see also blobs.lisp and corridors.lisp. downside: there is
; nothing that constrains the grid towards a point (e.g. around a
; particular walking radius of travel one might expect in a real town).
; this could be done by drawing into a board of a particular shape, but
; that would be something of a kluge (not Cluj)

(load "util")
(load "common")

(randomize)

(defparameter +rows+ 23)
(defparameter +cols+ 79)

; white noise to restrict the roads helps avoid what might be termed the
; "suburban sterility complex" (the same grid everywhere)
(defconstant +percent-noise+ 0.1)
; however! this can cause too-small road networks should the seed start
; at an unlucky point. higher noise percentages will cause results to be
; thrown out and retried more often
(defparameter *points* 0)
(defconstant +min-points+ 200)

; or rather a "T" from a given heading. these happen when the agent
; advances to each grid spacing value
(defparameter +forks+
  '(((-1 . 0) (0 . -1) (0 . 1)) ((1 . 0) (0 . -1) (0 . 1))
    ((0 . -1) (-1 . 0) (1 . 0)) ((0 . 1) (-1 . 0) (1 . 0))))

; "empty", "roads", "building"
; TODO need to place buildings adjacent to roads somehow
(defconstant +free+ #\.)
(defconstant +used+ #\=)
(defconstant +nope+ #\#)
(defconstant +noise+ #\,)

(declaim (inline fork fork-headings generate make-spacing make-start-point))

(defstruct agent (position nil) (heading nil) (moves 0) (spacing nil))

(defparameter *board* (make-board +rows+ +cols+ +free+))

(defun fork-headings (heading)
  (rest (assoc heading +forks+ :test #'equal)))

; unlike UNIX, this is a proper three-pronged fork
(defun fork (agent)
  (declare (agent agent))
  (let ((orthogonal (copy-point (agent-spacing agent)))
        (pos (agent-position agent))
        (ret (list agent)))
    (nreverse-point orthogonal)
    (dolist (h (fork-headings (agent-heading agent)))
      (push
       (make-agent :position (copy-point pos) :heading h :spacing orthogonal)
       ret))
    ret))

; agents walk out from a point in compass directions. the spacing
; controls the grid size those agents use
(defun four-agents-at (point spacing)
  (declare (cons point spacing))
  (let ((ortho (copy-point spacing)))
    (nreverse-point ortho)
    (list
     (make-agent :position (copy-point point) :heading '(-1 . 0) :spacing
      spacing)
     (make-agent :position (copy-point point) :heading '(1 . 0) :spacing
      spacing)
     (make-agent :position (copy-point point) :heading '(0 . -1) :spacing
      ortho)
     (make-agent :position (copy-point point) :heading '(0 . 1) :spacing
      ortho))))

; the magical value 3 provides for 2xN sized blocks between the roads
(defun make-spacing (apart)
  (if (coinflip) `(3 . ,apart) `(,apart . 3)))

; locate start point somewhere inside the board so less likely to end up
; with a run along the edges
(defun make-start-point ()
  (let ((point (random-point-inside (- +rows+ 4) (- +cols+ 4))))
    (incf-point point '(2 . 2))))

; advance an agent a step. fork orthogonal agents at grid spacing
; points. remove agent if out of bounds, or if hit another road
; segement. (could also check for illegal 2x2 roads like in blobs.lisp,
; especially if there are multiple road systems on a single board)
(defun update-agent (agent)
  (declare (agent agent))
  (let ((new-point (move-agent agent)) (moves (incf (agent-moves agent))))
    (when
        (and (p-inbounds? new-point)
             (equal (get-point-obj new-point) +free+))
      (draw-at-point new-point +used+)
      (incf *points*)
      (when (< *points* +min-points+)
        (if (zerop (mod moves (first (agent-spacing agent))))
            (fork agent)
            (list agent))))))

(defun generate ()
  (dolist
      (point (n-random-points (truncate (* +rows+ +cols+ +percent-noise+))))
    (draw-at-point point +noise+))
  (let ((start (make-start-point)))
    (draw-at-point start +used+)
    (incf *points*)
    (animate (four-agents-at start (make-spacing (roll 2 4 +2)))
     #'update-agent)))

(loop :do
  (generate)
  (when (>= *points* +min-points+) (return))
  (clear-board +free+)
  (setf *points* 0))

(no-return (display-board))
