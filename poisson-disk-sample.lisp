;;;;; attempt at a "Poisson disk sample set" implementation

;;; larger values will force sparser object spacing
(defparameter *grid-resolution* 3)

;;; see random-point-around for how this is used
(defparameter *min-distance* 7)

;;; higher means more fill (and at some point merely time wasted
;;; bouncing things off an already full grid)
(defparameter *nearby-count* 10)

(defparameter *rows* 23)
(defparameter *cols* 79)

(load "util.lisp")
(load "common.lisp")

(progn (setq *random-state* (make-random-state t)) t)

(defparameter *board* (make-board *rows* *cols*))

(defun board2grid (x)
  (cond ((atom x) (round (/ x *grid-resolution*)))
        (t (cons (board2grid (car x)) (board2grid (cdr x))))))

;;; TODO need a better name for this lower resolution version of
;;; the board
(defparameter *grid*
  (make-array (list (1+ (board2grid *rows*)) (1+ (board2grid *cols*)))
              :initial-element nil))
;(inspect *grid*)
;(defparameter *grid-consumed* 0)

(defun add-to-grid (p)
  ;(incf *grid-consumed*)
  (setf (aref *grid*
              (board2grid (point-row p))
              (board2grid (point-col p))) t))

(defun grid-taken? (p)
  (aref *grid*
        (board2grid (point-row p))
        (board2grid (point-col p))))

(defun poisson-set-r (to-process output)
  (and (null to-process) (return-from poisson-set-r output))
  (let ((p (pop-random to-process)) (new nil))
    (repeat *nearby-count*
      (setf new (random-point-around p *min-distance*))
      (and
        (p-inbounds? new)
        (not (grid-taken? new))
        (progn
          (push new output)
          (push new to-process)
          (add-to-grid new)))))
  (poisson-set-r to-process output))

(defun poisson-set (p)
  (add-to-grid p)
  (poisson-set-r (list p) (list p)))

; is it more efficient to simply generate whitenoise that honors the grid?
; problem with both these methods is as the grid fills up the attempts to
; place a point fail more often
(defun some-whitenoise ()
  (let ((new nil) (points nil))
    (repeat 750
      (setf new (random-point))
      (or (grid-taken? new)
          (progn
            (add-to-grid new)
            (pushnew new points))))
    points))

; TODO need better way to show what the grid looks like
;(add-to-grid '(0 . 0))
;(add-to-grid '(5 . 5))
;(add-to-grid '(5 . 78))
;(add-to-grid '(20 . 78))
;(dotimes (r *rows*)
;  (dotimes (c *cols*)
;    (let ((p (make-point r c)))
;      (or (grid-taken? p)
;          (draw-at-point p #\x)))))

;(defparameter *points* (poisson-set (random-point)))
(defparameter *points* (some-whitenoise))
(dolist (p *points*)
  (draw-at-point p #\x))
(display-board)
(format t "~&~D~%" (length *points*))
;(format t "~&~D~%" *grid-consumed*)
