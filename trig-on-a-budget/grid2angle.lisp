;;; How many ratios and angles are there in a grid of particular size?
;;; That is, when is it feasible to use a lookup table for trig instead
;;; of doing all the necessary math, and at what cost?

(load "util.lisp")

;;; change this to some (small) positive integer to calc the number of
;;; ratios for a given range of board sizes
(defparameter *boardmax* nil)

(defparameter *ratios* (make-hash-table :test 'equal))
(defparameter *angles* (make-hash-table :test 'equal))

(defun ratio2angle (x1 y1 x2 y2)
  (let ((adj (- x2 x1)) (opp (- y2 y1)))
    (cond ((zerop adj)
           (if (plusp opp)
             (values "0,1" 90)
             (values "0,-1" 270)))
          ((zerop opp)
           (if (plusp adj)
             (values "1,0" 0)
             (values "-1,0" 180)))
          (t (values
               (format nil "~d,~d" adj opp)
               (rad2deg (atan (/ opp adj))))))))

(defun different-square (x1 y1 x2 y2)
  (or (/= x1 x2) (/= y1 y2)))

(defun testpos (x1 y1 x2 y2)
  (when (different-square x1 y1 x2 y2)
    (multiple-value-bind (ratio angle) (ratio2angle x1 y1 x2 y2)
      (sethash *ratios* ratio angle)
      (sethash *angles* angle))))

(if (not (null *boardmax*))
  (let ((gridmin 1))
    (loop for gridmax from (1+ gridmin) to *boardmax* do
          (loop for player-x from gridmin to gridmax do
                (loop for player-y from gridmin to gridmax do
                      (loop for x from gridmin to gridmax do
                            (loop for y from gridmin to gridmax do
                                  (testpos player-x player-y x y)))))
          (let ((anglecount (hash-table-count *angles*))
                (ratiocount (hash-table-count *ratios*)))
            (format t "boardsize=~d ratios=~d angles=~d~%"
                    gridmax ratiocount anglecount)
            (when (< anglecount 0) (show-hash-keys *angles*))
            (when (< ratiocount 0) (show-hash *ratios*))
            (hash-empty *angles*)
            (hash-empty *ratios*)))))

(let ((gridmin 1) (gridmax 10))
  (loop for player-x from gridmin to gridmax do
        (loop for player-y from gridmin to gridmax do
              (loop for x from gridmin to gridmax do
                    (loop for y from gridmin to gridmax do
                          (testpos player-x player-y x y)))))
  (capture-stdout "grid.out" (show-hash *ratios*))
  (hash-empty *angles*)
  (hash-empty *ratios*))
