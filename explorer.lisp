;;;;; curses (via CL-CHARMS) explorer of a level map, requires a level
;;;;; map to load, e.g.
;;;;;
;;;;;   sbcl --script explorer.lisp board

(defparameter *debug-log*
  (open "debug" :direction :output :if-exists :append
        :if-does-not-exist :create))

(defparameter *board* nil)
(defparameter +rows+ 0)
(defparameter +cols+ 0)
(defparameter +player+ #\@)

#-quicklisp
(let ((quicklisp-init
       (merge-pathnames "quicklisp/setup.lisp"
                        (user-homedir-pathname))))
  (when (probe-file quicklisp-init) (load quicklisp-init)))
(ql:quickload 'cl-charms :silent t)

(defstruct point (col 0 :type fixnum) (row 0 :type fixnum))
(defmacro update-point (p col row)
  `(progn (setf (point-col ,p) ,col) (setf (point-row ,p) ,row) ,p))
(defun add-points (&rest points)
  (make-point :col (reduce #'+ (mapcar #'point-col points))
              :row (reduce #'+ (mapcar #'point-row points))))
(defun make-point-relative-to (p col row)
  (declare (fixnum col row))
  (make-point :col (+ (point-col p) col)
              :row (+ (point-row p) row)))

(defparameter *where* (make-point :col 1 :row 1))

(defparameter *keylist* nil)

(defmacro key-bind (key &body body)
  `(push '(,key ,@body) *keylist*))

(defun key-cond (key)
  (mapcar
   #'(lambda (kbod)
       (let ((bind (first kbod)) (body (rest kbod)))
         `((eq ,key ,bind) ,@body)))
   (nreverse *keylist*)))

(defmacro action-for (keyact)
  `(cond ,@(key-cond keyact)))

(defmacro move-player (&key (row 0) (col 0))
  (declare (fixnum row col))
  `(let ((goal (make-point-relative-to *where* ,row ,col)))
     (if (array-in-bounds-p *board* (point-row goal) (point-col goal))
       (prog1
         (list *where* goal)
         (setf *where* goal))
       nil)))

(key-bind #\h (move-player :row -1 :col  0))
(key-bind #\j (move-player :row  0 :col  1))
(key-bind #\k (move-player :row  0 :col -1))
(key-bind #\l (move-player :row  1 :col  0))
(key-bind #\y (move-player :row -1 :col -1))
(key-bind #\u (move-player :row  1 :col -1))
(key-bind #\b (move-player :row -1 :col  1))
(key-bind #\n (move-player :row  1 :col  1))
(key-bind #\q (exit))

(defun load-board (in)
  (let ((board nil) (rows 1) (cols 0))
    (flet ((line2aref (line r cols)
             (dotimes (c cols) (setf (aref board r c) (elt line c)))))
      (let ((line (read-line in nil)))
        (setf cols (length line))
        (setf board
                (make-array (list rows cols) :element-type 'character
                            :adjustable t))
        (line2aref line (1- rows) cols))
      (loop for line = (read-line in nil)
            while line
            do (let ((len (length line)))
                 (incf rows)
                 (when (not (eq len cols))
                   (error "uneven columns at line ~d" rows))
                 (adjust-array board (list rows cols))
                 (line2aref line (1- rows) cols))))
    (values board rows cols)))

(defun cli-args ()
  #+CCL *unprocessed-command-line-arguments*
  #+CLISP ext:*args*
  #+SBCL (cdr *posix-argv*)    ; nix the program name
  #-(or CCL CLISP SBCL) (error "cli-args unimplemented"))

(defun really-need-file (fname)
  (when (null fname) (format *error-output* "need a filename") (exit))
  fname)

(with-open-file (in (really-need-file (first (cli-args))))
  (multiple-value-bind (b r c)
      (load-board in)
    (setf *board* b)
    (setf +rows+ r)
    (setf +cols+ c)))

(defun draw-player (w)
  (let ((col (point-col *where*)) (row (point-row *where*)))
    (charms:write-char-at-point w +player+ col row)
    (charms:move-cursor w col row)
    ; TODO when exactly does this need to be called?
    (charms:refresh-window w)))

(defun explorer ()
  (charms:with-curses ()
    (let ((game-window (charms:make-window +cols+ +rows+ 0 0)))
      (charms:disable-echoing)
      (charms:enable-raw-input)
      (dotimes (r +rows+)
        (dotimes (c +cols+)
          (charms:write-char-at-point game-window (aref *board* r c) c r)
          (charms:refresh-window game-window)))
      (draw-player game-window)
      (do () (nil)
        (let ((key (charms:get-char charms:*standard-window*)))
          (let ((ret (action-for key)))
            (unless (null ret)
              (let ((col (point-col (car ret))) (row (point-row (car ret))))
                (charms:write-char-at-point game-window
                                            (aref *board* row col) col row)
                (draw-player game-window)))))))))

(explorer)
