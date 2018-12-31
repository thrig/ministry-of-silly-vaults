;;;;; compare energy functions, Common LISP version. these are best-case
;;;;; uses of the algorithm; notably, no aggregation and shuffling of
;;;;; moves that happen at the same time is done. this may be necessary
;;;;; for the cost or priority queue functions; the circular buffer
;;;;; naturally groups events together

(declaim (optimize (speed 3) (safety 0) (debug 0))
         (inline min-ani-cost rand-bi))

(block nil (setq *random-state* (make-random-state t)) (return))
(load "pqmin")

(defun make-cbuf (size)
  (declare (fixnum size))
  (let ((buf (make-array size :element-type 'list :initial-element nil))
        (cur 0))
    (declare (fixnum cur))
    (values
     (lambda ()
       (prog1 (aref buf cur)
         (setf (aref buf cur) nil)
         (setf cur (mod (1+ cur) size))))
     (lambda (offset animate)
       (push animate (aref buf (mod (+ cur offset -1) size)))))))

(defun run-cbuf (count moves costfn)
  (multiple-value-bind (active insert)
      (make-cbuf count)
    (dotimes (n count) (funcall insert 1 n))
    (do ((animates (funcall active) (funcall active)))
        ((<= moves 0))
      (dolist (ani animates)
        (funcall insert (funcall costfn) ani)
        (decf moves)))))

(defun min-ani-cost (animates)
  (let ((min-cost most-positive-fixnum))
    (dolist (ani animates)
      (let ((cost (animate-cost ani)))
        (when (< (animate-cost ani) min-cost)
          (setf min-cost cost))))
    min-cost))

(defun run-cost (count moves costfn)
  (let ((animates nil))
    (dotimes (n count)
      (push (make-animate :name n :cost 0) animates))
    (do ((min-cost (min-ani-cost animates) (min-ani-cost animates)))
        ((<= moves 0))
      (dolist (ani animates)
        (let ((epoch (- (animate-cost ani) min-cost)))
          (if (<= epoch 0)
              (progn
                (setf (animate-cost ani) (funcall costfn))
                (decf moves))
              (setf (animate-cost ani) epoch)))))))

(defun run-priq (count moves costfn)
  (let ((queue (new-priq count)))
    (dotimes (n count)
      (priq-push (make-animate :name n :cost 0) queue))
    (do ((ani (priq-pop queue) (priq-pop queue)))
      ((<= moves 0))
      (setf (animate-cost ani) (+ (animate-cost ani) (funcall costfn)))
      (priq-push ani queue)
      (decf moves))))

(defun rand-bi () (+ 1 (random 8)))

(time (run-cbuf 10000 1000000 #'rand-bi))
(time (run-cost 10000 1000000 #'rand-bi))
(time (run-priq 10000 1000000 #'rand-bi))
