;;;;; "white" versus "brown" noise for the random placement of objects
;;;;; onto a grid

(defparameter +rows+ 20)
(defparameter +cols+ 72)

(defparameter *floor*  #\.)
(defparameter *plant*  #\P)
(defparameter *fungus* #\f)
(defparameter *rock*   #\#)

(load "util")
(load "common")

(progn (setq *random-state* (make-random-state t)) t)

(defparameter *board* (make-board +rows+ +cols+ *floor*))

(defun %-of-board (p) (truncate (* +rows+ +cols+ p)))

;;; under "white noise" objects are randomly placed on the field
(defun white-noise (&key (count 10) (obj #\x))
  (do* ((total (array-total-size *board*) (1- total))
        (n (if (> count total) total count))
        (b (make-array total
                       :displaced-to *board*
                       :element-type (array-element-type *board*))))
    ((= n 0) t)
    (when (< (random 1.0) (/ n total))
      (setf (aref b (1- total)) obj)
      (decf n))))

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
                         (point (random-point))
                         (rand-cols 9)
                         (rand-rows 5))
  (let ((r (point-row point)) (c (point-col point)))
    (repeat count
            (draw-at r c obj)
            (setf r (nearby r +rows+ rand-rows))
            (setf c (nearby c +cols+ rand-cols)))))

;;; fill some percentage of the board with the given type of noise
;(white-noise :count (%-of-board 0.10))
;(display-board)
;(clear-array *board* #\x)
;(brown-noise :count (%-of-board 0.10))
;(display-board)

;;; more variety
;(white-noise :obj *plant*  :count (%-of-board 0.03))
;(brown-noise :obj *fungus* :count (%-of-board 0.10))
;(brown-noise :obj *rock*   :count (%-of-board 0.20))
;(display-board)

;;; even more variety
(white-noise :obj *plant*  :count (%-of-board 0.03))
(dolist (p (n-random-points (decay :min 8)))
  (brown-noise :obj *fungus* :count (decay :min 7) :point p
               :rand-cols 9 :rand-rows 5))
(dolist (p (n-random-points (decay :min 8)))
  (brown-noise :obj *rock* :count (decay :min 5) :point p
               :rand-cols 3 :rand-rows 3))
(display-board)
