;;;;; class-based circular buffer implementation that uses sub-arrays
;;;;; for the events so that a Fisher-Yates shuffle can more easily
;;;;; randomize the events for a given turn

(block nil (setq *random-state* (make-random-state t)) (return))

;;; borrowed from "Knuth shuffle" Rosetta Code page
(defun nshuffle-array (array)
  (loop for i from (length array) downto 2
        do (rotatef (aref array (random i))
                    (aref array (1- i)))
        finally (return array)))

(defclass cbuf nil
  ((size :initarg :size :reader cbuf-size)
   (buffer)
   (subsize :initarg :subsize)
   (iter :initform 0 :reader cbuf-iter :type fixnum))
  (:documentation "circular buffer implementation"))

(defmethod initialize-instance :after ((cb cbuf) &key)
  (let ((size (slot-value cb 'size)))
    (setf (slot-value cb 'buffer) (make-array size))
    (dotimes (n size)
      (setf (aref (slot-value cb 'buffer) n)
              (make-array (slot-value cb 'subsize) :fill-pointer 0
                          :adjustable t)))))

(defmethod cbuf-events ((cb cbuf))
  (prog1 (aref (slot-value cb 'buffer) (slot-value cb 'iter))
    (setf (aref (slot-value cb 'buffer) (slot-value cb 'iter))
            (make-array (slot-value cb 'subsize) :fill-pointer 0
                        :adjustable t))
    (setf (slot-value cb 'iter)
            (mod (1+ (slot-value cb 'iter)) (slot-value cb 'size)))))

(defmethod cbuf-insert ((cb cbuf) offset &rest items)
  (declare (fixnum offset))
  (dolist (obj items)
    (vector-push-extend obj
      (aref (slot-value cb 'buffer)
            (mod (+ (slot-value cb 'iter) offset -1)
                 (slot-value cb 'size)))))
  (values))

;;; subsize might be the expected number of events per turn, though I
;;; have not microbenchmarked how good LISP implementations are at
;;; pre-filling versus extending a vector on demand
(defun make-cbuf (size) (make-instance 'cbuf :size size :subsize 1))

(defparameter *events* (make-cbuf 4))

;;; pre-game-loop events
(cbuf-insert *events* +1 'no 'pa 're 'ci 'vo 'mu 'xa 'ze 'bi 'so)
(cbuf-insert *events* +2 'aaa)

;;; run through a few turns
(format t "iter ~a ~a~%" (cbuf-iter *events*)
        (nshuffle-array (cbuf-events *events*)))
(cbuf-insert *events* +1 'bbb 'ccc 'ddd)

(dotimes (n 4)
  (format t "iter ~a ~a~%" (cbuf-iter *events*)
          (nshuffle-array (cbuf-events *events*))))
