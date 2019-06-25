; an integer-based energy system, illustrated
;
; simultaneous moves are not really considered; that would require
; collecting zero-cost animates and then before the delete dead animates
; use an initiative system to see who goes first, or so forth

(defparameter *animates* nil)
(defstruct animate name cost dead update)

(defmacro say (&rest args) `(progn (format t ,@args) (fresh-line)))

; update methods return the cost of the action performed
; higher values == slower moves
(defun update-hero  (ani) (prog1 1000 (say "~a moves" (animate-name ani))))
(defun update-monst (ani) (prog1  600 (say "~a moves" (animate-name ani))))

; mook makers or other such event schedulers would be called "animates"
; and put in the list of animates
(push (make-animate :name "hero"  :cost 0 :update #'update-hero)  *animates*)
(push (make-animate :name "monst" :cost 0 :update #'update-monst) *animates*)

(defun least-cost (animates)
  (loop for ani in animates
        minimize (animate-cost ani) into min
        finally (return min)))

; find animate with the least cost, whack that value off of each
; animate's cost, if zero (or less??) call the update function for that
; animate and set the return value as the new cost. this used to have an
; "is the animate dead?" test but that's Yet Another Branch In The Loop
; (YABITL) which can be argued as not necessary so axed
(defun runit ()
  (loop repeat 10 do
    (let ((min-cost (least-cost *animates*)))
      (dolist (ani *animates*)
        (let ((new-cost (- (animate-cost ani) min-cost)))
          (setf (animate-cost ani)
            (if (<= new-cost 0) (funcall (animate-update ani) ani) new-cost))))
      (delete-if (lambda (ani) (animate-dead ani)) *animates*)
      (sleep 0.5))))

(runit)
