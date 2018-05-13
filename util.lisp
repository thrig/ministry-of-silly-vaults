(defmacro % (a b)
  `(mod (truncate ,a) (truncate ,b)))

(defun decay (&key (odds 0.1) (min 1) (max 640))
  (do ((count min (1+ count)))
    ((or (>= count max) (< (random 1.0) odds)) count)))

(defun deg2rad (degrees)
  (/ (* (round (% degrees 360)) pi) 180))
(defun rad2deg (radians)
  (% (round (* (/ 180 pi) radians)) 360))

(defmacro sethash (hash key &optional (value nil))
  `(setf (gethash ,key ,hash) ,value))
(defun hash-empty (table)
  (maphash #'(lambda (k v) (remhash k table)) table))
(defun show-hash (table)
  (maphash #'(lambda (k v) (format t "~a=~a~%" k v)) table))
(defun show-hash-keys (table)
  (maphash #'(lambda (k v) (format t "~a " k)) table)
  (fresh-line))
(defun show-hash-values (table)
  (maphash #'(lambda (k v) (format t "~a " v)) table)
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

(defmacro random-list-item (alist)
  `(progn
     (or (listp ,alist)
       (error "random-list-item needs a list to act on"))
     (if (= 0 (list-length ,alist))
       nil
       (nth (random (list-length ,alist)) ,alist))))

(defmacro repeat (count &body body)
  (let ((repnum (gensym)))
    `(progn
       (if (or (not (integerp ,count)) (< ,count 1))
         (error "repeat count must be positive integer"))
       (do ((,repnum ,count (1- ,repnum)))
         ((< ,repnum 1) (return))
         ,@body))))

(defun select-n (n list)
  (let ((len (list-length list)))
    (if (> n len)
      (error "cannot pick ~d items from ~d" n len))
    (do ((item list (cdr item))
         (remain len (1- remain))
         (selection nil)
         (want n))
      ((= 0 want) (nreverse selection))
      (if (< (random remain) (if (= want remain) remain (/ want remain)))
        (progn
          (push (car item) selection)
          (setf want (1- want)))))))

(defmacro while (expr &body body)
  `(tagbody check (if ,expr (progn ,@body (go check)))))
