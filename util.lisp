(declaim (inline decay sign-of random-list-item range))

; more or less like how Perl does it
(defun % (a b)
  (mod (truncate a) (truncate b)))

(defun binomial (n p &optional (x 0))
  (declare (fixnum n x))
  (dotimes (i n)
    (and (< (random 1.0) p) (incf x)))
  (the fixnum x))

(defun clear-array (a fill)
  (let* ((len (array-total-size a))
         (b (make-array len
                        :displaced-to a
                        :element-type (array-element-type a))))
    (dotimes (n len)
      (setf (aref b n) fill))))

(defmacro coinflip () `(zerop (random 2)))

(defun decay (&key (odds 0.1) (min 1) (max MOST-POSITIVE-FIXNUM))
  (declare (fixnum min max))
  (do ((count min (1+ count)))
    ((or (>= count max) (< (random 1.0) odds)) (the fixnum count))))

(defun deg2rad (degrees)
  (/ (* (round (% degrees 360)) pi) 180))
(defun rad2deg (radians)
  (% (round (* (/ 180 pi) radians)) 360))

(defmacro sethash (hash key &optional (value nil))
  `(setf (gethash ,key ,hash) ,value))
(defun hash-empty (table)
  (maphash #'(lambda (k v)
               (declare (ignore v))
               (remhash k table)) table))
(defun show-hash (table)
  (maphash #'(lambda (k v)
               (format t "~a=~a~%" k v)) table))
(defun show-hash-keys (table)
  (maphash #'(lambda (k v)
               (declare (ignore v))
               (format t "~a " k)) table)
  (fresh-line))
(defun show-hash-values (table)
  (maphash #'(lambda (k v)
               (declare (ignore k))
               (format t "~a " v)) table)
  (fresh-line))

(defmacro append-file (file)
  `(open ,file
         :direction :output
         :if-exists :append
         :if-does-not-exist :create))

(defmacro clobber-file (file)
  `(open ,file :direction :output :if-exists :supersede))

(defmacro capture (stream where &body body)
  (let ((out (gensym)))
    `(progn
       (if (streamp ,where)
         (setf ,out ,where)
         (setf ,out (clobber-file ,where)))
       (when ,out
         (let ((,stream ,out)) ,@body)
         (if (not (streamp ,where))
           (close ,out))))))

(defmacro capture-stdout (where &body body)
  `(capture *standard-output* ,where ,@body))

(defmacro no-return (&body body) `(progn ,@body (values)))

(defmacro odds-onein (n) `(zerop (random ,n)))
(defmacro odds-ninm (n m) `(< (random ,m) ,n))

(defun poisson (l)
  (do ((L (exp (- l)))
       (p 1.0 (* p (random 1.0)))
       (k 0 (1+ k)))
    ((<= p L) (1- k))))

(defun pop-nplus1 (n alist)
  (declare (fixnum n))
  (if (= n 1)
    (prog1
      (cadr alist)
      (rplacd alist (cddr alist)))
    (pop-nplus1 (1- n) (cdr alist))))
(defmacro pop-random (alist)
  (let ((n (gensym)))
    `(let ((,n (random (list-length ,alist))))
       (if (= ,n 0)
         (prog1
           (car ,alist)
           (setf ,alist (cdr ,alist)))
         (pop-nplus1 ,n ,alist)))))

(defun random-list-item (alist &optional alen)
  (or (listp alist)
      (error "need a list to act on"))
  (let ((len (if (integerp alen) alen (list-length alist))))
    (if (= 0 len) nil
      (nth (random len) alist))))

(defmacro randomize ()
  `(block nil (setq *random-state* (make-random-state t)) (return)))

(defun range (min max &optional (step 1))
  (declare (fixnum min max step))
  (if (zerop step) (error "step must not be zero"))
  (if (and (< max min) (plusp step)) (setf step (* step -1)))
  (do ((list nil) (op (if (< min max) #'> #'<)))
    ((funcall op min max) (the list (nreverse list)))
    (push min list)
    (setf min (+ min step))))

(defmacro repeat (count &body body)
  (let ((repnum (gensym)))
    `(progn
       (if (or (not (integerp ,count)) (< ,count 1))
         (error "repeat count must be positive integer"))
       (do ((,repnum ,count (1- ,repnum)))
         ((< ,repnum 1) (values))
         ,@body))))

(defun reverse-signum (n)
  (* (signum n) -1))

(defun select-n (n list)
  (declare (fixnum n))
  (do* ((item list (cdr item))
        (total (list-length list) (1- total))
        (left (min n total))
        (selection nil))
    ((= 0 left) selection)
    (when (< (random 1.0) (/ left total))
      (push (car item) selection)
      (decf left))))
; similar to previous but acts on each item somehow
(defun act-on-n (n list yes-fn no-fn)
  (declare (fixnum n))
  (do* ((item list (cdr item))
        (total (list-length list) (1- total))
        (left (min n total)))
    ((null item))
    (if (and (plusp left) (< (random 1.0) (/ left total)))
      (progn
        (funcall yes-fn (car item))
        (decf left))
      (funcall no-fn (car item)))))

; from "On Lisp" chapter 7
(defmacro showm (expr)
  `(pprint (macroexpand-1 ',expr)))

; zero is counted as positive here for reasons lost in the mists of
; time, probably something related to music theory. see also SIGNUM
(defun sign-of (n)
  (declare (fixnum n))
  (the fixnum (if (minusp n) -1 1)))

(defmacro while (expr &body body)
  (let ((label (gensym)))
    `(block while
      (tagbody ,label
        (if ,expr (progn ,@body (go ,label)))))))
