;;; try out a technique usually reserved for animates wherein a circle
;;; of some radius is projected some distance out in front of the
;;; animate, and then the animate tries to move to a point that moves
;;; randomly on the perimeter of that circle. here this is instead for
;;; drawing things onto a level map
;;;
;;; things to TWEAK include the walker's v dist r theta values, along
;;; with the update-target code (how much to move the target by)
;;;
;;; (this code is based on a description of an algorithm that reportedly
;;; originates from Craig Reynolds of "boids" fame)

; how many moves the walker should make (more == more filled board, duh!)
(defparameter +moves+ 640)

(defparameter +rows+ 23)
(defparameter +cols+ 79)

(defparameter *bg* #\.)
(defparameter *fg* #\#)

(load "util")
(load "common")

(proclaim '(optimize speed))
(randomize)

(defstruct
    (animate
     (:print-function
      (lambda (a stream depth)
        (declare (ignore depth))
        (format stream "#<ANI dir:~a v:~$ [~$,~$] D:~$ R:~$ J:~a Th:~a>"
                (rad2deg (animate-dir a))
                (animate-v a)
                (animate-x a)
                (animate-y a)
                (animate-dist a)
                (animate-r a)
                (rad2deg (animate-jitter a))
                (animate-theta a)))))
  (dir 0.0 :type float)                 ; heading of walker
  (v 0.0 :type float)                   ; how far walker moves
  (x 0.0 :type float)                   ; where x == col
  (y 0.0 :type float)                   ;       y == row
  (dist 0.0 :type float)                ; how far circle is out ahead
  (r 0.0 :type float)                   ; radius of circle
  (jitter 0.0 :type float)              ; how much to change theta by
  (theta 0.0 :type float))              ; angle to target on circle

(defparameter *board* (make-board +rows+ +cols+ *bg*))
(defparameter *walker*
  (make-animate :dir (deg2rad (random 360))
                :v 1.0 :x 40.0 :y 10.0 :dist 5.0 :r 3.0
                :jitter (deg2rad 90)
                :theta (deg2rad (random 360))))

; ROUND is bad here as it might round up to the array limit, TRUNCATE
; always keeps it below that
(defun walker-row () (truncate (animate-y *walker*)))
(defun walker-col () (truncate (animate-x *walker*)))

(defun advance-walker ()
  (setf (animate-x *walker*)
        (+ (animate-x *walker*)
           (* (animate-v *walker*) (cos (animate-dir *walker*)))))
  (setf (animate-y *walker*)
        (+ (animate-y *walker*)
           (* (animate-v *walker*) (sin (animate-dir *walker*))))))

;;; this does wrap-around so things are being projected onto a donut
;;; mmm donut
(defun keep-walker-inbounds ()
  (unless (array-in-bounds-p *board* (walker-row) (walker-col))
    (setf (animate-x *walker*) (mod (animate-x *walker*) +cols+))
    (setf (animate-y *walker*) (mod (animate-y *walker*) +rows+))))

;;; point the animate at where the target point is on the circle out in
;;; front of the walker (or regardless to somewhere, if I've made any
;;; geometry errors...)
(defun track-target ()
  (let ((tx
         (+ (+ (animate-x *walker*)
               (* (animate-dist *walker*) (cos (animate-dir *walker*))))
            (* (animate-r *walker*) (cos (animate-theta *walker*)))))
        (ty
         (+ (+ (animate-y *walker*)
               (* (animate-dist *walker*) (sin (animate-dir *walker*))))
            (* (animate-r *walker*) (sin (animate-theta *walker*))))))
    (setf (animate-dir *walker*)
            (atan
             (/ (- ty (animate-y *walker*))
                (- tx (animate-x *walker*)))))))

;;; NOTE this is always a positive move, could also make it move
;;; back-and-forth by applying a random sign or minus half the angle
(defun update-target ()
  (setf (animate-theta *walker*)
          (+ (animate-theta *walker*)
             (random (animate-jitter *walker*)))))

(dotimes (n +moves+)
  (draw-at (walker-row) (walker-col) *fg*)
  (track-target)
  (advance-walker)
  (keep-walker-inbounds)
  (update-target))

(no-return (display-board))
