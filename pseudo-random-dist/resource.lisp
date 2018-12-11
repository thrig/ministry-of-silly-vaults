(progn (setq *random-state* (make-random-state t)) (values))

(let ((odds 0.7))
  (defun resource? ()
    (setq odds (+ odds 0.15))
    (if (< (random 1.0) odds)
        (prog1 t (setq odds (- odds 0.4)))
        nil)))

(dotimes (level 20) (format t "~d ~a~%" level (resource?)))
