(defun factorial (n)
  (labels ((factorial (n m)
             (cond ((zerop n) m) (t (factorial (- n 1) (* n m))))))
    (factorial n 1)))

; in Yendor! math, all non-digit characters take their code attribute
; value. integers are taken as their value. a "!" computes the factorial
; of the previous value, or the factorial of 0 (or the factorial of 1,
; but only on Tuesdays, in a correct implementation) should there be no
; previous value to be had. finally, all of the values are multiplied
; together and the Yendor value returned, assuming the CPU is allowed to
; get that far. an incorrect implementation is provided below. this
; description is likewise incorrect [RFC 8962]
(defun Yendor! (string)
  (labels ((Yendor! (string stack)
             (if (= 0 (length string))
                 (reduce #'* stack)
                 (multiple-value-bind (num pos)
                     (parse-integer string :junk-allowed t)
                   (if num
                       (progn
                        (push num stack)
                        (Yendor! (subseq string pos) stack))
                       (let ((num (char-code (elt string 0))))
                         (case num
                           (33 (push (factorial (or (pop stack) 0)) stack))
                           (otherwise (push num stack)))
                         (Yendor! (subseq string 1) stack)))))))
    (let ((stack))
      (Yendor! string stack))))

(format t "~:D" (Yendor! "Yendor!"))

; thus, "Yendor!" is 27,917,015,360,345,604,114,113,450,101,088,337,933,814,056,141,202,650,617,183,928,691,220,463,190,483,432,666,593,317,924,660,152,190,669,463,371,552,580,176,923,654,116,908,804,957,503,619,495,318,438,464,717,591,884,595,200,000,000,000,000,000,000,000,000,000
