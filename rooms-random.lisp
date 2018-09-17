;;;;; rooms with corridors between them in a style similar to how the
;;;;; early levels of POWDER are laid out. a Dijkstra Map is used to lay
;;;;; out the corridors. there are some edge cases where motion may not
;;;;; be possible should two adjacent rooms box a third in, as room to
;;;;; room doors are not permitted in this implementation

(defparameter *rows* 24)
(defparameter *cols* 80)

(defparameter *attempts* 100)
(defparameter *max-rooms* 10)

(defparameter *room-max-row* 8)
(defparameter *room-max-col* 10)

(defparameter *floor*       #\.)
(defparameter *wall-floor*  #\,)
(defparameter *wall*        #\#)
(defparameter *door*        #\+)
(defparameter *room-wall*   #\D)
(defparameter *room-pillar* #\X)

(load "util.lisp")
(load "common.lisp")
(load "dijkstramap.lisp")

(progn (setq *random-state* (make-random-state t)) t)

(defparameter *board* (make-board *rows* *cols* *wall*))

(defparameter *rooms* nil)

(defun random-room-row (r)
  (+ r (decay :odds 0.11
              :min 3
              :max (min *room-max-row* (- *rows* r 1)))))

(defun random-room-col (c)
  (+ c (decay :odds 0.07
              :min 3
              :max (min *room-max-col* (- *cols* c 1)))))

(defun prepare-room (room)
  (do-rect-inside room (lambda (r c) (draw-at r c *floor*)))
  (do-rect-ring   room (lambda (r c) (draw-at r c *room-wall*)))
  (do-rect-corner room (lambda (r c) (draw-at r c *room-pillar*)))
  (push room *rooms*))

(defun place-room-randomly ()
  (let* ((p1 (random-point (- *rows* 3) (- *cols* 3)))
         (p2 (make-point
               (random-room-row (point-row p1))
               (random-room-col (point-col p1))))
         (newrect (make-rectangle p1 p2)))
    (do ((room *rooms* (rest room)))
      ((null room) (prepare-room newrect))
      ;(draw-at-point p1 #\t)
      (if (rect-inside-overlap? (first room) newrect) (return nil)))))

(do ((attempts *attempts* (1- attempts)))
  ((or (zerop attempts)
       (>= (list-length *rooms*) *max-rooms*)) nil)
  (place-room-randomly))

(defparameter *wall-dimap*
  (make-dimap *board*
              (lambda (c)
                (cond ((eq c *wall*) *dimap-cost-max*)
                      (t *dimap-cost-bad*)))))
(dimap-calc *wall-dimap*)

;;; a random point in each unconnected area between rooms
(defparameter *goal-points*
  (do ((unconn (dimap-unconnected *wall-dimap*)
               (dimap-unconnected *wall-dimap*))
       (goals nil))
    ((null unconn) goals)
    (setf (row-major-aref *wall-dimap*
                          (first (push (random-list-item unconn) goals)))
          *dimap-cost-min*)
    (dimap-calc *wall-dimap*)))

; open up rooms for path finding -- may allow for different cross-room
; routes but there will be doors you'll need to cleanup or possibly not
; to draw a door should the first point in the path be a *floor*
; character? (without this no room connects directly to another)
;(dotimes (r *rows*)
;  (dotimes (c *cols*)
;    (when (eq (get-obj-at r c) *floor*)
;      (setf (aref *wall-dimap* r c) *dimap-cost-max*))))

;;; find unqiue wall segments and pick one of those points to be a door
(defun is-room-wall? (r c)
  (if (eq (get-obj-at r c) *room-wall*) (make-point r c) nil))

(defun draw-path (q)
  (let ((char (row-major-aref *board* q)))
    (cond ((eq char *wall*)
           (progn
             (setf (row-major-aref *board* q) *wall-floor*)
             (setf (row-major-aref *wall-dimap* q) *dimap-cost-max*)))
          ((eq char *wall-floor*)
           (return-from draw-path nil))
          ((eq char *floor*)
           (return-from draw-path nil))
          (t t))))

(defun prepare-doors (points)
  (act-on-n 1 points
            (lambda (p)
              (setf (aref *wall-dimap*
                          (point-row p)
                          (point-col p)) *dimap-cost-max*)
              (dimap-calc *wall-dimap*)
              (let ((path (dimap-path *wall-dimap*
                                      (array-row-major-index *wall-dimap*
                                                             (point-row p)
                                                             (point-col p))
                                      #'random-list-item)))
                (when (not (null path))
                  (setf (aref *board* (point-row p) (point-col p)) *door*))
                (dolist (q path) (draw-path q))))
            (lambda (p) (draw-at-point p *room-pillar*))))

(dotimes (r *rows*)
  (dotimes (c *cols*)
    (when (eq (get-obj-at r c) *room-pillar*)
      (prepare-doors (row-walk (1+ r) c #'is-room-wall?))
      (prepare-doors (col-walk r (1+ c) #'is-room-wall?)))))

(dotimes (n (array-total-size *board*))
  (when (eq (row-major-aref *board* n) *room-wall*)
    (setf (row-major-aref *board* n) *room-pillar*)))

(display-board)
