;;; given an animate with a heading (angle) and a location (x,y floating
;;; point) this code tries to determine the distance that animate has to
;;; the nearest border of the (col,row integer) cell that the animate is
;;; within (new from scratch impementation, may be buggy or unideal)

; some geometry foo
(defun deg2rad (degrees) (* degrees (/ pi 180)))
(defun rad2deg (radians) (* radians (/ 180 pi)))
(defun sq (x) (* x x))
(defun distance (x1 y1 x2 y2) (sqrt (+ (sq (- x2 x1)) (sq (- y2 y1)))))
(defun slope (x y nx ny) (/ (- ny y) (- nx x)))
(defun intercept (x y slope) (- y (* slope x)))
(defun slope-intercept (x y angle)
  (let* ((nx (+ x (cos angle)))
         (ny (+ y (sin angle)))
         (slope (slope x y nx ny)))
    (list slope (intercept x y slope))))

; these are where the slope/intercept of the animate crosses points
; formed by [x,1] or [x,0] or [0,y] or [1,y]. these points may not be on
; the "unit cell"
;
; solve x here so x=(y-b)/m so need slope and intercept (y is 1 or 0)
(defun cell-point-north (slope intercept) (list (/ (- 1 intercept) slope) 1))
(defun cell-point-south (slope intercept) (list (/ (- 0 intercept) slope) 0))
; solve y so mx+b (easy as x is 0 or 1)
(defun cell-point-west (slope intercept) (list 0 intercept))
(defun cell-point-east (slope intercept) (list 1 (+ slope intercept)))

(defun zero-to-one (x) (and (< x 1) (> x 0)))

(defun cell-point (angle slope intercept)
  (cond
   ((<= angle (/ pi 2))                                 ; I quadrant
    (let ((north (cell-point-north slope intercept))
          (east (cell-point-east slope intercept)))
      (if (zero-to-one (first north)) north east)))
   ((<= angle pi)                                       ; II
    (let ((north (cell-point-north slope intercept))
          (west (cell-point-west slope intercept)))
      (if (zero-to-one (first north)) north west)))
   ((<= angle (* (/ pi 2) 3))                           ; III
    (let ((south (cell-point-south slope intercept))
          (west (cell-point-west slope intercept)))
      (if (zero-to-one (first south)) south west)))
   ((<= angle (* 2 pi))                                 ; IV
    (let ((south (cell-point-south slope intercept))
          (east (cell-point-east slope intercept)))
      (if (zero-to-one (first south)) south east)))
   (t (error "unnormalized angle ~a" angle))))          ; wtf?

; an "animate" that has a bearing (direction) and location both floating
; point and row/column for grid (cell) display. probably instead should
; be a CLOS object rather than a macro-wrapped struct
(defstruct
    (animate
     (:print-function
      (lambda (a stream depth)
        (declare (ignore depth))
        (format stream "#<ANI dir:~a a:~$ velo:~$ [~A,~A] [~$,~$] m:~$ b:~$>"
                (rad2deg (animate-dir a))
                (animate-accel a)
                (animate-velo a)
                (animate-col a)
                (animate-row a)
                (animate-x a)
                (animate-y a)
                (animate-slope a)
                (animate-intercept a)))))
  (dir 0.0 :type float)
  (accel 0.0 :type float)
  (velo 0.0 :type float)
  (x 0.0 :type float)           ; col
  (y 0.0 :type float)           ; row
  (slope 0.0 :type float)
  (intercept 0.0 :type float)
  (distance 0.0 :type float)
  (row 0 :type fixnum)          ; y
  (col 0 :type fixnum))         ; x

(defmacro new-ani (&key dir (accel 0.0) velo x y)
  `(let* ((ani
           (make-animate :dir ,dir :accel ,accel :velo ,velo :x ,x :y
            ,y :col (floor ,x) :row (floor ,y)))
          (unitc-x (nth-value 1 (truncate ,x)))
          (unitc-y (nth-value 1 (truncate ,y))))
     (destructuring-bind
         (m b)
         (slope-intercept unitc-x unitc-y ,dir)
       (setf (animate-slope ani) m)
       (setf (animate-intercept ani) b)
       (let ((p (cell-point ,dir m b)))
         (setf (animate-distance ani)
                 (distance unitc-x unitc-y (first p) (second p)))))
     ani))

; for some various headings at a given starting location
(dolist (x '(30 45 60 90 120))
  (let ((ani (new-ani :dir (deg2rad x) :velo 1.0 :x 0.5 :y 0.5)))
    (format t "~a ~a~%" x (animate-distance ani))))
