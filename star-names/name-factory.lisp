;
;     -*-    The Platinum Battletuna of Doom 'Shinkuugama'    -*-
;
; a variation iterator that has been much mutated from the original
; code[1]; this variant tried to solve the problem of returning a
; randomly generated name only once. another solution is to screen the
; output of a random name generator through a hash, though that may
; perform increasingly worse as the hash fills up. the possibly huge
; downside of this code is that an array of values the size of the total
; number of possible variations must be generated and shuffled. the hash
; method will doubtless perform better when the number of possible names
; is large compared the the number of names used, and if the list of
; possible names is small those could simply be listed, shuffled, and
; iterated through directly. so this code probably should not be used

; strings are used as-is; lists are converted to arrays of choices that
; get picked at randomly by various bits of code. only one level of
; conversion happens
(defparameter *weapons*
  '("The"
    ("Shiny" "Rusty" "Broken" "Platinum")
    ("Katana" "Blunderbuss" "Warpike" "Battletuna")
    "of"
    ("Doom" "Slaying" "Spaghetti" "")
    ("'Eladril'" "'Sahanji'" "'Shinkuugama'" "")))

; it's srand(3) Jim but not as we know it
(block nil (setq *random-state* (make-random-state t)) (return))

(declaim (inline breakdown fisher-yates-shuffle))

; array element randomization
(defun fisher-yates-shuffle (vec)
  (let ((i (array-total-size vec)))
    (loop :for i :from (1- i) :downto 0 :do
      (let ((j (random (1+ i))))
        (unless (= i j) (rotatef (aref vec i) (aref vec j)))))))

; this performs a "pounds, shillings, pence" breakdown (but in the
; opposite order) of the total for the given COUNTS that would be
; something like '(12 20) for 12 pence to the shilling and 20 shillings
; to the pound. instead this is breaking down the TOTAL number into
; suitable array indices to pick out the weapon type and such from
;
; NOTE this ignores any leftover in the total not accounted for by the
; counts; such a condition might throw an error, or the total might
; first be cut down to a suitable size with, say, MOD
(defun breakdown (total counts)
  (let ((indices (make-array (array-total-size counts)
                             :element-type 'fixnum
                             :initial-element 0)))
    (loop :for i from 0
          :for divisor :across counts :do
      (multiple-value-bind (quotient remainder)
          (truncate total divisor)
        (setf (aref indices i) remainder) 
        (when (zerop quotient) (return))
        (setf total quotient)))
    indices))

; not very good. probably would be better handled by a grammar or some
; string template format
(defun stringify (output)
  (do ((len (array-total-size output))
       (result (aref output 0))
       (index 1 (1+ index)))
    ((>= index len) result)
    (let ((value (aref output index)))
      (cond ((zerop (length value)))
            ((and (string= value "of")
                  (< (1+ index) len)
                  (zerop (length (aref output (1+ index)))))
             (incf index))
            (t (setf result (format nil "~a ~a" result value)))))))

; this builds a bunch of parallel arrays that contain all the choices
; for each "slot" of the output string and other necessary metadata. the
; TOTAL lets the caller know how many variations there are across the
; input tokens, and the returned function call when given a number
; hopefully between 0 and below TOTAL will return some random
; combination of the choices, mangled into a string, poorly
(defun name-factory (tokens)
  (let ((len (list-length tokens)))
    (let ((counts (make-array len :element-type 'fixnum :initial-element 1))
          (output (make-array len))
          (source (make-array len))
          (total 1))
      (loop :for obj :in tokens
            :for index :from 0 :do
        (etypecase obj
          (string (setf (aref source index) obj))
          (list
            (let ((len (list-length obj)))
              (setf (aref source index) 
                    (make-array len :initial-contents obj))
              (setf (aref counts index) len))))
        (setf total (* total (aref counts index))))
      (values
        (lambda (n)
          (let ((indices (breakdown n counts)))
            (loop :for index :from 0
                  :for obj :across source :do
              (setf (aref output index)
                (etypecase obj
                  (string obj)
                  (array (aref obj (aref indices index)))))))
          (stringify output))
        total))))

; figure out the total number of variations, generate a random order to
; visit them in, and generate each variation
(defun showall (tokens &key (randomize t))
  (multiple-value-bind (callback total)
      (name-factory tokens)
    (let ((lookup (make-array total :element-type 'fixnum))
          (audit (make-hash-table :test 'equal)))
      (loop :for i :from 0 :below total :do
        (setf (aref lookup i) i))
      (when randomize (fisher-yates-shuffle lookup))
      (loop :for n :across lookup :do
        (let ((result (funcall callback n)))
          ; is the above code correct? test it.
          (when (gethash result audit nil)
            (error "illegal duplicate value ~a" result))
          (format t "~a~%" result)
          (setf (gethash result audit) t))))
    (format *error-output* "n=~a~%" total)))

(defun emit! () (showall *weapons* :randomize t))

(emit!)

; [1] the "Genomic Sequence Generator" in "Higher Order Perl" by Mark
;     Jason Dominus. p.136
