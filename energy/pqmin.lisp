(declaim (optimize (speed 3) (safety 0) (debug 0))
         (inline priq-exch priq-lessp priq-length priq-emptyp priq-sink
          priq-swim priq-push priq-peek priq-pop))

(defun print-ani (x stream depth)
  (declare (ignore depth))
  (format stream "#<animate ~a cost ~a>" (animate-name x) (animate-cost x)))

(defstruct (animate (:print-function print-ani))
  "an animate object"
  (name nil)
  (cost 0 :type fixnum))

(defstruct priq
  "minimum value priority queue"
  (length 0 :type fixnum)
  (queue nil :type simple-array))

;;; this is a 1-based array; zeroth element is unused. there's an
;;; animate in the first spot because SBCL was yelling about it
;;; being NIL
(defmacro new-priq (&optional (max 32)) "make a new priority queue"
 `(the priq
       (make-priq :queue
        (make-array (1+ ,max) :element-type 'animate :initial-element
                    (make-animate) :adjustable nil :fill-pointer nil
                    :displaced-to nil))))

(defun priq-emptyp (q)
  "is the priority queue empty?"
  (the boolean (zerop (priq-length q))))

(defun priq-exch (x y q)
  "exchange two elements by index in the queue"
  (declare (fixnum x y)
           (priq q))
  (rotatef (aref (priq-queue q) x) (aref (priq-queue q) y))
  (values))

(defun priq-lessp (x y q)
  "comparison for elements in the queue"
  (declare (fixnum x y)
           (priq q))
  (the boolean
       (< (animate-cost (aref (priq-queue q) y))
          (animate-cost (aref (priq-queue q) x)))))

(defun priq-sink (k q)
  "lower an element by index in the queue"
  (declare (fixnum k)
           (priq q))
  (do ((dk 0))
      ((> (ash k 1) (priq-length q)) nil)
    (setf dk (ash k 1))
    (and (< dk (priq-length q)) (priq-lessp dk (1+ dk) q) (incf dk))
    (and (not (priq-lessp k dk q)) (return))
    (priq-exch dk k q)
    (setf k dk))
  (values))

(defun priq-swim (k q)
  "raise an element by index in the queue"
  (declare (fixnum k)
           (priq q))
  (do ((hk (ash k -1) (ash k -1)))
      ((not (and (> k 1) (priq-lessp hk k q))) nil)
    (priq-exch hk k q)
    (setf k hk))
  (values))

(defun priq-pop (q)
  "remove and return the head of the queue"
  (declare (priq q))
  (if (priq-emptyp q)
      nil
      (prog1 (aref (priq-queue q) 1)
        (let ((len (priq-length q)))
          (priq-exch 1 len q)
          (setf (aref (priq-queue q) len) nil)
          (decf (priq-length q))
          (priq-sink 1 q)))))

(defun priq-peek (q)
  "peek at the head of the queue"
  (declare (priq q))
  (if (priq-emptyp q)
      nil
      (aref (priq-queue q) 1)))

(defun priq-push (ani q)
  "adds an animate to the queue"
  (declare (priq q))
  (let ((n (incf (priq-length q))))
    (setf (aref (priq-queue q) n) ani)
    (priq-swim n q))
  (values))
