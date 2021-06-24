; klaji.lisp - yet another attempt at drawing something like a street
; grid. see also blobs.lisp and corridors.lisp. downside: there is
; nothing that constrains the grid towards a point (e.g. around a
; particular walking radius of travel one might expect in a real town).
; this could be done by drawing into a board of a particular shape, but
; that would be something of a kluge (not Cluj)

(load "util")
(load "common")

(randomize)

; where the agent is (a point), a vector (heading) the agent moves in,
; which here is a step in some compass direction, how many moves this
; agent has made (a tally for the spacing), and how often cross-roads
; should be forked off from the agent (the spacing)
(defstruct agent (position nil) (heading nil) (moves 0) (spacing nil))

(defparameter +rows+ 23)
(defparameter +cols+ 79)

; white noise to restrict the roads helps avoid what might be termed the
; "suburban sterility complex" (the same grid everywhere)
(defconstant +percent-noise+ 0.11)
; however! this can cause too-small road networks should the seed start
; at an unlucky point. higher noise percentages will cause results to be
; thrown out and retried more often. also setting the noise too high may
; result in a street grid that is too irregular. goldilocks!
(defparameter *points* 0)
; noise is ignored when there are fewer than this many points generated
; to help get the grid started
(defconstant +min-points+ 16)
; no more than this many road points will be generated
(defconstant +max-points+ 256)

; or rather a "T" from a given heading. these happen when an agent
; advances to each grid spacing value
(defparameter +forks+
  '(((-1 . 0) (0 . -1) (0 . 1)) ((1 . 0) (0 . -1) (0 . 1))
    ((0 . -1) (-1 . 0) (1 . 0)) ((0 . 1) (-1 . 0) (1 . 0))))

(defconstant +free+  #\.)
(defconstant +noise+ #\,)
(defconstant +road+  #\=)
(defconstant +house+ #\#)

(declaim
 (inline fork fork-headings generate make-spacing make-start-point middle-of))

(defparameter *board* (make-board +rows+ +cols+ +free+))

(defun fork-headings (heading)
  (rest (assoc heading +forks+ :test #'equal)))

; unlike UNIX, this is a proper three-pronged fork. unlike a proper
; fork, two of the tines go off at right angles to the third (a "T")
(defun fork (agent)
  (declare (agent agent))
  (let ((perpendicular (copy-point (agent-spacing agent)))
        (pos (agent-position agent))
        (ret (list agent)))
    (nreverse-point perpendicular)
    (dolist (h (fork-headings (agent-heading agent)))
      (push (make-agent :position (copy-point pos) :heading h
                        :spacing perpendicular)
            ret))
    ret))

; agents walk out from a point in compass directions. the spacing
; controls the grid size those agents use
(defun four-agents-at (point spacing)
  (declare (cons point spacing))
  (let ((perpendicular (copy-point spacing)))
    (nreverse-point perpendicular)
    (list
      (make-agent :position (copy-point point) :heading '(-1 . 0)
                  :spacing spacing)
      (make-agent :position (copy-point point) :heading '(1 . 0)
                  :spacing spacing)
      (make-agent :position (copy-point point) :heading '(0 . -1)
                  :spacing perpendicular)
      (make-agent :position (copy-point point) :heading '(0 . 1)
                  :spacing perpendicular))))

; the magical value 3 provides for 2xN sized blocks between the roads
(defun make-spacing (apart)
  (if (coinflip) `(3 . ,apart) `(,apart . 3)))

(defun middle-of (n &optional (factor 4))
  (roll factor (truncate (/ n factor))))

; locate start point closer to the middle so less likely to generate a
; town confined by the edge of the board (a distance-from-start-point
; check would also help cut down on such, or to increase the noise
; towards the edges of the board)
(defun make-start-point ()
  (make-point (middle-of +rows+) (middle-of +cols+)))

; advance an agent a step. fork perpendicular agents at grid spacing
; points. remove agent if out of bounds, or if cell moved to is not free
; (could also check for illegal 2x2 roads like in blobs.lisp, especially
; if there are multiple seed points on a single board)
(defun update-agent (agent)
  (declare (agent agent))
  (let ((new-point (move-agent agent)) (moves (incf (agent-moves agent))))
    (when
        (and (p-inbounds? new-point)
             (or (equal (get-point-obj new-point) +free+)
                 (and (< *points* +min-points+)
                   (equal (get-point-obj new-point) +noise+))))
      (draw-at-point new-point +road+)
      (incf *points*)
      (when (< *points* +max-points+)
        (if (zerop (mod moves (first (agent-spacing agent))))
            (fork agent)
            (list agent))))))

(defun generate ()
  (setf *points* 0)
  (let ((start (make-start-point)))
    (draw-at-point start +road+)
    (incf *points*)
    (animate (four-agents-at start (make-spacing (roll 2 3 +2)))
             #'update-agent)))

(defun noise ()
  (with-n-random-points
      (row col (truncate (* +rows+ +cols+ +percent-noise+)))
    (draw-at row col +noise+)))

(noise)
(let ((retries 0))
  (loop :do
    (generate)
    (when (>= *points* +max-points+) (return))
    (incf retries)
    (clear-board +free+))
  (format *error-output* "retries ~a~%" retries))
; this is (sometimes) neat but really needs the 2x2 road cleanup thing
;(generate)

; clean up the road-blocking noise
; NOTE =,= cases could either be fixed to === or to =.. or ..= to cut
; down on potential "house placed diagonal to a road" cases, e.g.
;   ...    .#.
;   =.= -> =.=
;   ...    ...
;
; likewise ==,
;          ..= bends might also be rectified, somehow
;          ..=
; ("somehow" would probably be a "cellular automaton smooth" type pass
; over the points)
(with-each-cell *board* line n
  (when (equal (aref line n) +noise+)
    (setf (aref line n) +free+)))

; place housen ("hice" was rejected as the plural for "house") near
; roads. maybe. this does not place housen at road ends (which CDDA
; tends not to do, perhaps so linking roads are easier to put in?)
(dotimes (r +rows+)
  (dotimes (c +cols+)
    (when (equal (get-obj-at r c) +free+)
      (let ((count 0))
        (with-adjacent-square r c nr nc
          (when (equal (get-obj-at nr nc) +road+) (incf count)))
        (when (plusp count)
          (with-adjacent-diagonal r c nr nc
            (when (equal (get-obj-at nr nc) +road+) (incf count)))
          (when (and (> count 1) (< (random 5) count))
            (draw-at r c +house+)))))))

; probably should decorate the board with water, forests, etc here

(no-return (display-board))
