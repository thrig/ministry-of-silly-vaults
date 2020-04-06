;;;;; vault rotations and mirrorings such that a vault can be presented
;;;;; in any aspect

(load "util")

(defmacro with-array-cells (vault tmp n &body body)
  (let ((len (gensym)))
    `(let* ((,len (array-total-size ,vault))
            (,tmp (make-array ,len :displaced-to ,vault
                              :element-type (array-element-type ,vault))))
       (dotimes (,n ,len) ,@body))))

(defmacro with-vault (vault rows cols &body body)
  `(destructuring-bind (,rows ,cols) (array-dimensions ,vault) ,@body))

(defmacro with-new-vault (vault new rows cols &body body)
  `(let ((,new (make-array (list ,rows ,cols) :element-type
                           (array-element-type ,vault))))
     ,@body
     ,new))

(defun dup (vault)
  (with-vault vault rows cols
    (with-new-vault vault new rows cols
      (dotimes (r rows)
        (dotimes (c cols)
          (setf (aref new r c) (aref vault r c)))))))

(defun flip-cols (vault)
  (with-vault vault rows cols
    (dotimes (r rows)
      (dotimes (c (truncate (/ cols 2)))
        (rotatef (aref vault r c) (aref vault r (- (1- cols) c)))))))

(defun flip-rows (vault)
  (with-vault vault rows cols
   (dotimes (r (truncate (/ rows 2)))
     (dotimes (c cols)
       (rotatef (aref vault r c) (aref vault (- (1- rows) r) c))))))

; the easiest rotate+mirror operation to do
(defun romirror (vault)
  (with-vault vault rows cols
   (with-new-vault vault new cols rows
    (dotimes (r rows)
      (dotimes (c cols) (setf (aref new c r) (aref vault r c)))))))

; or counter clock-wise, at least in a terminal
(defun rotate-90 (vault)
  (with-vault vault rows cols
   (with-new-vault vault new cols rows
    (loop :for sr :from 0 :below rows
          :do (loop :for sc :from 0 :below cols
                    :for dc :from (1- cols) :downto 0
                    :do (setf (aref new dc sr) (aref vault sr sc)))))))

; same as flip-cols + flip-rows
(defun rotate-180 (vault)
  (with-vault vault rows cols
   (with-new-vault vault new rows cols
    (loop :for sr :from 0 :below rows
          :for dr :from (1- rows) :downto 0
          :do (loop :for sc :from 0 :below cols
                    :for dc :from (1- cols) :downto 0
                    :do (setf (aref new dr dc) (aref vault sr sc)))))))

(defun rotate-270 (vault)
  (with-vault vault rows cols
   (with-new-vault vault new cols rows
    (loop :for sr :from 0 :below rows
          :for dr :from (1- rows) :downto 0
          :do (loop :for sc :from 0 :below cols
                    :do (setf (aref new sc dr) (aref vault sr sc)))))))

(defun show (vault)
  (with-vault vault rows cols
    (dotimes (r rows)
      (dotimes (c cols)
        (princ (aref vault r c)))
      (fresh-line))))

(defun sig (vault)
  (let ((sig (make-string (array-total-size vault))))
    (with-array-cells vault tmp n
      (setf (aref sig n) (aref tmp n)))
    sig))

; this is not much code for non-biased results but worst case will flip
; the vault three times
(defun simple-vary (vault)
  (when (coinflip) (flip-cols vault))
  (when (coinflip) (flip-rows vault))
  (when (coinflip) (flip-cols vault))
  (if (coinflip) (romirror vault) vault))

(defun vary (vault)
  (ecase (random 8)
    (0 vault)
    (1 (rotate-90 vault))
    (2 (rotate-180 vault))
    (3 (rotate-270 vault))
    (4 (progn (flip-cols vault) vault))
    (5
     (let ((new (rotate-90 vault)))
       (flip-cols new)
       new))
    (6
     (let ((new (rotate-180 vault)))
       (flip-cols new)
       new))
    (7
     (let ((new (rotate-270 vault)))
       (flip-cols new)
       new))))

(randomize)

(let ((i 64) (v (make-array '(3 5) :element-type 'character)))
  (with-array-cells v tmp n
    (setf (aref tmp n) (code-char (incf i))))
  ;(show v)
  ;(format t "--~&")
  ;(show (romirror v))
  (loop :repeat 10000 :do
    (let ((vault (dup v)))
      (format t "~a~%" (sig (vary vault))))))
