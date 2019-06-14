;;;;; generate cave-like rooms, similar to what a cellular automaton
;;;;; smooth may do to a random map but without the CA smooth; instead,
;;;;; rectangles are drawn and their corners filled in randomly to make
;;;;; them less rectanglish

(defparameter +rows+ 23)
(defparameter +cols+ 79)

(defparameter *floor* #\.)
(defparameter *wall*  #\#)
(defparameter *fill*  #\X)

(load "util")
(load "common")

(proclaim '(optimize speed))
(randomize)

(defparameter *board* (make-board +rows+ +cols+ *wall*))

;;; which direction (row . col) must be filled in at each corner
(defparameter *coffsets*
              (make-array 4
                          :initial-contents
                          '((1 . 1) (1 . -1) (-1 . -1) (-1 . 1))))

;;; recursive corner in-fill: picks a row or column as the direction to
;;; use, fills in 1 to N squares, then for the column or row adjacent to
;;; the point filled in repeats the process with one less than N for
;;; both directions (another option would be to use (1- N) and (1- rlen)
;;; or (1- clen) as appropriate for slightly different results)
(defun fill-corner (row col roff coff rlen clen obj)
  (if (coinflip)
    (let ((N (random-between 1 clen)))
      (draw-horiz row col (* N coff) obj)
      (when (> N 1)
        (setf N (1- N))
        (fill-corner (+ row roff) col roff coff N N obj)))
    (let ((N (random-between 1 rlen)))
      (draw-vert row col (* N roff) obj)
      (when (> N 1)
        (setf N (1- N))
        (fill-corner row (+ col coff) roff coff N N obj)))))

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
        (draw-cave (random-between 1 (- +rows+ 5))
                   (random-between 1 (- +cols+ 5))
                   (random-between 4 10)
                   (random-between 4 16)))
(no-return (display-board))
