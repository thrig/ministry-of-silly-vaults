(block nil (setq *random-state* (make-random-state t)) (return))

; adapted from "Behavioral Mathematics for Game AI" by Dave Mark (p.
; 264), code that skews a distribution of dice rolls by dropping high or
; low rolls from the set (or both)
(defun skew-roll (times sides &key (drop-lo 0) (drop-hi 0))
  (when (>= (+ drop-lo drop-hi) times)
    (error "cannot drop more elements than will generate"))
  (let ((rolls (sort (loop :repeat times :collecting (random sides)) #'<)))
    (when (plusp drop-hi)
      (rplacd (nthcdr (- times drop-hi 1) rolls) nil))
    (when (plusp drop-lo)
      (setf rolls (nthcdr drop-lo rolls)))
    rolls))

; 3d11
(skew-roll 3 11)
; 3d11 but skewed
(skew-roll 4 11 :drop-lo 1)
(skew-roll 4 11 :drop-hi 1)
; even more skewed
(skew-roll 5 11 :drop-hi 2)
; mostly to test for logic errors, as drop-lo should not be done first
(skew-roll 4 11 :drop-lo 1 :drop-hi 2)
