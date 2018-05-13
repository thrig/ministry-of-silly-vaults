;;;;; "white" versus "brown" noise for the random placement of objects
;;;;; onto a grid

(defparameter *rows*  20)
(defparameter *cols*  72)
(defparameter *fill*  #\.)
(defparameter *board* (make-array (list *rows* *cols*)
                                  :element-type 'character
                                  :initial-element *fill*))
(defparameter *plant*  #\P)
(defparameter *fungus* #\f)
(defparameter *rock*   #\#)

(load "common.lisp")
(load "util.lisp")

(progn (setq *random-state* (make-random-state t)) t)

(defun %-of-board (p) (truncate (* *rows* *cols* p)))

;;; under "white noise" objects are randomly placed on the field
(defun white-noise (&key (count 10) (obj #\?))
  (repeat count
          (setf (aref *board* (random *rows*) (random *cols*)) obj)))

(defun if-legal (new max)
  (and (>= new 0) (< new max) new))

(defun nearby (x max range)
  (setf range (if (oddp range) range (1+ range)))
  (do ((new nil (if-legal (+ x (- (random range)
                                  (truncate (/ range 2)))) max)))
    ((numberp new) new)))

;;; with "brown noise" the placement of an object is done near the
;;; location of the previous object, if any
(defun brown-noise (&key (count 10) (obj #\?)
                         (point (cons (random *rows*) (random *cols*)))
                         (rand-cols 9)
                         (rand-rows 5))
  (let ((r (car point)) (c (cdr point)))
    (repeat count
            (setf (aref *board* r c) obj)
            (setf r (nearby r *rows* rand-rows))
            (setf c (nearby c *cols* rand-cols)))))

;;; fill some percentage of the board with the given type of noise
;(white-noise :obj #\x :count (%-of-board 0.10))
;(display-board)
;(clear-board)
;(brown-noise :obj #\x :count (%-of-board 0.10))
;(display-board)

;;; more variety
;(white-noise :obj *plant*  :count (%-of-board 0.03))
;(brown-noise :obj *fungus* :count (%-of-board 0.10))
;(brown-noise :obj *rock*   :count (%-of-board 0.20))
;(display-board)

;(brown-noise :obj *fungus* :count (%-of-board 0.10) :point '(5 . 5))

;;; even more variety
(white-noise :obj *plant*  :count (%-of-board 0.03))
(dolist (p (n-random-points (decay :min 8)))
  (brown-noise :obj *fungus* :count (decay :min 7)
               :rand-cols 9 :rand-rows 5))
(dolist (p (n-random-points (decay :min 8)))
  (brown-noise :obj *rock* :count (decay :min 5)
               :rand-cols 3 :rand-rows 3))
(display-board)
