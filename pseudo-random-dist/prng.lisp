(setq *random-state* (make-random-state t))

(defun make-prng (odds)
  (let* ((oddslst (copy-list odds))
         (curv 0)
         (maxv (1- (list-length oddslst))))
    #'(lambda ()
        (if (<= (random 1.0) (nth curv oddslst))
            (prog1 t
              (setq curv 0))
            (prog1 nil
              (setq curv (if (>= curv maxv) 0 (1+ curv))))))))

(defmacro when-trigger (call &body body) `(when (funcall ,call) ,@body))

(let ((ability (make-prng '(0.03 0.17 0.29 0.37 0.43 0.59 0.83)))
      (hits 0)
      (trials 10000))
  (dotimes (n trials) (when-trigger ability (incf hits)))
  (format t "~g~%" (/ hits trials)))
