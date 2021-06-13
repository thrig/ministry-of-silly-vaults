; blobs.lisp - corridors.lisp was the first attempt to draw something
; that looks like a street grid--while perhaps interesting, its output
; looks nothing like a street grid. this is another attempt, that again
; is a failure to produce a street grid, probably because it lacks a
; means to produce long roads and instead has each cell picking some
; adjacent cell, if legal, to expand into (random walk with a slight
; limitation on what cells are legal)

(defparameter +rows+ 23)
(defparameter +cols+ 79)

; how long a seeded point should spread for (higher means fewer seed
; points will be used and the potential to fill lots more of the board)
(defconstant +max-ttl+ 20)

; how much of the board to fill. NOTE the generation will perform poorly
; or even get stuck should their be few or no cells left to place a seed
; point at
(defconstant +max-generate+ (truncate (* +rows+ +cols+ 0.42)))
(defparameter *generated* 0)

(load "util")
(load "common")

(randomize)

; "empty", "roads", "building"
(defconstant +free+ #\.)
(defconstant +used+ #\=)
(defconstant +nope+ #\#)

(defparameter *active* (make-hash-table :test #'equal))
(defparameter *board* (make-board +rows+ +cols+ +free+))

(declaim (inline is-used restricted? seed-point set-point update-point))

(defun is-used (point offset)
  (let ((new-point (add-points point offset)))
    (and (p-inbounds? new-point) (equal (get-point-obj new-point) +used+))))

; do not allow "roads" to form 2x2 squares
(defun restricted? (point)
  (dolist
      (adjacents
       '(((-1 . -1) (0 . -1) (-1 . 0))
         ((-1 . 1) (-1 . 0) (0 . 1))
         ((1 . 1) (0 . 1) (1 . 0))
         ((1 . -1) (1 . 0) (0 . -1))))
    (when
        (and (is-used point (first adjacents))
             (is-used point (second adjacents))
             (is-used point (third adjacents)))
      (return t))))

(defun seed-point (point ttl)
  (sethash *active* point ttl)
  (incf *generated*)
  (draw-at-point point +used+))

; NOTE this assumes that the point has been checked if +free+
(defun set-point (point ttl)
  (if (restricted? point)
      (progn (incf *generated*) (draw-at-point point +nope+) nil)
      (progn (seed-point point ttl) t)))

; spread somewhere adjacent or kill the point
; TODO maybe weighting on the direction to spread would cut down on the
; "random walk" local fill problem? but this is probably the wrong
; approach for a street grid however much more code would be bolted on
(defun update-point (point)
  (let ((ttl (1- (gethash point *active*))))
    (if (plusp ttl)
        (let ((available
               (remove-if (lambda (p) (not (equal (get-point-obj p) +free+)))
                          (points-square-to point))))
          (if available
              (progn
               (set-point (random-list-item available) ttl)
               (setf (gethash point *active*) ttl))
              (remhash point *active*)))
        (remhash point *active*))))

; NOTE hash key ordering may vary; might instead pick a random point
; or sort the list for better reproducibility
(defun generate ()
  (loop :while (< *generated* +max-generate+)
        :do (let ((points (hashkeys *active*)))
              (if points
                  (dolist (p points) (update-point p))
                  (let ((point (random-point)))
                    (when (equal (get-point-obj point) +free+)
                      (seed-point point (1+ (random +max-ttl+)))))))))

(generate)
(no-return (display-board))
