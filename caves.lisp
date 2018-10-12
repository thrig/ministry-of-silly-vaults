;;;;; generate cave-like rooms, similar to what a cellular automaton
;;;;; smooth may do to a random map but without the CA smooth; instead,
;;;;; rectangles are drawn and their corners filled in randomly to make
;;;;; them less rectanglish

(defparameter *rows* 23)
(defparameter *cols* 79)

(defparameter *floor* #\.)
(defparameter *wall*  #\#)
(defparameter *fill*  #\X)

(load "util.lisp")
(load "common.lisp")

(progn (setq *random-state* (make-random-state t)) t)

(defparameter *board* (make-board *rows* *cols* *wall*))

(defparameter *coffsets*
              (make-array 4
                          :initial-contents
                          '((1 . 1) (1 . -1) (-1 . -1) (-1 . 1))))

(defun fill-corner (row col roff coff rlen clen obj)
  (if (coinflip)
    (let ((len (random-between 1 clen)))
      (draw-horiz row col (* len coff) obj)
      (when (> len 1)
        (setf len (1- len))
        (fill-corner (+ row roff) col roff coff len len obj)))
    (let ((len (random-between 1 rlen)))
      (draw-vert row col (* len roff) obj)
      (when (> len 1)
        (setf len (1- len))
        (fill-corner row (+ col coff) roff coff len len obj)))))

(defun draw-cave (row col rlen clen)
  (let* ((corners
           (make-array 4
                       :initial-contents
                       (list (make-point row col)
                             (make-point row (+ col clen))
                             (make-point (+ row rlen) (+ col clen))
                             (make-point (+ row rlen) col))))
         (rect (make-rectangle (aref corners 0) (aref corners 2))))
    (when (rect-inbounds? rect)
      (do-rect rect (lambda (r c) (draw-at r c *floor*)))
      (dotimes (cnum 4)
        (fill-corner 
          (point-row (aref corners cnum))
          (point-col (aref corners cnum))
          (point-row (aref *coffsets* cnum))
          (point-col (aref *coffsets* cnum))
          (1- rlen) (1- clen) *fill*)))))

;(draw-cave 1 1 5 10)
;(draw-cave 10 20 8 16)
(repeat 50
        (draw-cave (random-between 1 (- *rows* 5))
                   (random-between 1 (- *cols* 5))
                   (random-between 4 10)
                   (random-between 4 16)))
(display-board)
