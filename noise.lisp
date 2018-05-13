;;;;; "white" versus "brown" noise for the random placement of objects
;;;;; onto a grid

(defparameter *rows*  23)
(defparameter *cols*  80)
(defparameter *fill*  #\.)

(defparameter *board* (make-array (list *rows* *cols*)
                                  :element-type 'character
                                  :initial-element *fill*))
(defparameter *plant*  #\P)
(defparameter *fungus* #\f)
(defparameter *rock*   #\#)

(defparameter *fill-percent* 10)

(load "common.lisp")

(progn (setq *random-state* (make-random-state t)) t)

(defun placements (rows cols percent)
  (truncate (* rows cols (/ percent 100))))

;;; under "white noise" objects are randomly placed on the field
(defun white-noise (&optional (obj *plant*))
  (dotimes (n (placements *rows* *cols* *fill-percent*))
    (setf (aref *board* (random *rows*) (random *cols*)) obj)))

(defun if-legal (new max)
  (and (>= new 0) (< new max) new))

(defun nearby (x max range)
  (setf range (if (oddp range) range (1+ range)))
  (do ((new nil))
    ((numberp new) new)
    (setf new (if-legal (+ x (- (random range)
                                (truncate (/ range 2)))) max))))

;;; with "brown noise" the placement of an object is done near the
;;; location of the previous object, if any
(defun brown-noise (&optional (obj *plant*))
  (let ((r (random *rows*)) (c (random *cols*)))
    (dotimes (n (placements *rows* *cols* *fill-percent*))
      (setf (aref *board* r c) obj)
      (setf r (nearby r *rows* 5))
      (setf c (nearby c *cols* 9)))))

(defun fill-via (fn &rest objects)
  (dolist (x objects) (funcall fn x)))

;(white-noise #\x)
;(display-board)
;(brown-noise #\x)
;(display-board)

(fill-via #'white-noise *plant* *fungus* *rock*)
(add-a-border *rock*)
(display-board)
(clear-board)
(fill-via #'brown-noise *plant* *fungus* *rock*)
(add-a-border *rock*)
(display-board)
