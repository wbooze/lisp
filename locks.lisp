(defpackage locks
  (:use :common-lisp)
  (:shadow describe)) ;whenever i refer to describe here use the one in this package!

;whenever i refer to describe from elsewhere (another package) it will have the usual meaning (:common-lisp)
; this shadow was necessary because the methods for describe here cause the gf for them
; to be created in the common-lisp package which already has a function or macro named describe!
; thus a name clash!
; notice not the names of the methods cause the clash, the generated gf's name does which gets interned in the :common-lisp package!

(in-package :locks)

(defun string-to-number (str &optional (base *read-base*) &rest rest)
  (read-from-string str base))
      
(defun number-to-string (num &optional (base *read-base*) &rest rest)
  (write-to-string num :base base))

(defun read-char-by-name (stream)
  "blabla"
  (let* ((input stream))
    (cond
      ((equal "^[0-9a-fA-F]+s" input)
       (string-to-number input 16))
      ((equal "^#" input)
       (read input))
      (t (read stream)))))

(defun clim-read-char-by-name (stream)
  "blabla"
  (let* ((input stream))
    (cond
      ((equal "^[0-9a-fA-F]+s" input)
       (string-to-number input 16))
      ((equal "^#" input)
       (read input))
      (t (read stream)))))

(defun insert (&rest args)
  args)

(defun ucs-insert (&optional character (count 1))
  "given a character returns the unicode symbol or reads input and then returns the symbol"
  (let ((character (or character (read-char-by-name *standard-input*)))
        (count (or count 1))
        (result (insert (or (string (code-char character)) (code-char character)))))
    (progn
      (dotimes (i count)
        (princ result)) result)))


;; imported from www-utils package of K.poeck (for process abstraction in sbcl)
;; just ignore these, as these are not essential for the understanding of OOP
;; and notice setf-if is already contained in this one as atomic-conditional-replacef
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-idle-time (process)
  "Returns the amount of time the process has been up, in seconds."
  (declare (ignore process))
  ;;; Can't find an exported interface
  0
  )

(defun process-run-time (process)
  "Returns the amount of run time the process has accumulated, in microseconds."
  ;;; used to be multiplied by 1000, don't know why
  (declare (ignore process))
  ;;; Can't find an exported interface
  0
  )

;;; Begin these do not follow the sbcl model of threads
;;; Easiest would be to create a process class and model the 
;;; interface on top of them
;;; Perhaps borrow from openmcl

(defclass %process% ()
  (
   (function-to-run :initform nil :accessor function-to-run)
   (args-to-use :initform nil :accessor args-to-use)
   (state :initform :stopped :accessor state)
   (name :initform "Unnamed process" :initarg :name :accessor name)
   (who-state :initform "Whatever State" :accessor who-state)
   (thread :initform nil :accessor thread)
   )
  )

(defmethod process-active-p ((me %process%))
  (eq (state me) :disable))

(defmethod process-active-p ((process sb-thread:thread))
  (sb-thread:thread-alive-p process))

(defmethod process-disable ((me %process%))
  "Don't know what to do here"
  ;;; Can't find an exported interface
  (setf (state me) :disable)
  (warn "Don't know yet how to disable thread")
  )

(defmethod process-enable ((me %process%))
  "Don't know what to do here"
  ;;; Can't find an exported interface
  (setf (state me) :enable)
  ;;; Create the thread and go
  (warn "Don't know yet how to enable thread")
  )

(defun make-process (name &key background-p restart-after-reset warm-boot-action priority &allow-other-keys)
  (declare (ignore background-p restart-after-reset warm-boot-action priority))
  (make-instance '%process% :name name))

(defmethod process-preset ((me %process%) initial-function &rest initial-args)
  (setf (function-to-run me) initial-function)
  (setf (args-to-use me) initial-args)
  me)

(defmethod process-whostate ((me %process%))
  (who-state me)
  )

;;; End these do not follow the sbcl model of threads

(defmethod process-kill ((process sb-thread:thread))
  (sb-thread:terminate-thread process))

(defmethod process-kill ((me %process%))
  (setf (state me) :killed))

(defun current-process ()
  sb-thread:*current-thread*)

(defmethod (setf process-priority) (val (process t))
  (declare (ignore val process))
  (values)
  )

(defun process-run-function (name function &rest args)
  (sb-thread:make-thread #'(lambda () (apply function args)) :name name)
  )

;;; can we implement this with sb-thread:interrupt-thread & Waitqueue/condition variables
(defun process-wait (reason &rest args)
  (declare (ignore reason args))
  (warn "Don't know yet how to wait for a process"))

(defun process-wait-with-timeout (whostate seconds function &rest args)
  (declare (ignore whostate seconds function args))
  (warn "Don't know yet how to wait for a process with timeout"))

;;; Assume tha we get threads
(defun process-name (process)
  (sb-thread:thread-name process)
  )

(defun all-processes ()
  (sb-thread:list-all-threads)
  )

;;;

(defun make-lock (name &key type &allow-other-keys)
  "Returns a lock named name that is suitable for use with with-lock-held."
  (declare (ignore type))
  (sb-thread:make-mutex :name name)) 

;;; sb-thread:with-mutex or sb-thread:with-recursive-lock???
(defmacro with-lock-held ((lock &optional (mode :write) (whostate "Wait for Lock")) &body body)
  "Executes BODY with LOCK held in MODE, which is one of :READ or :WRITE."
  (declare (ignore mode whostate))
  `(sb-thread:with-mutex (,lock :wait-p t)
     ,@body))

(defmacro with-timeout ((timeout &key error-p) &body body)
  "Executes BODY and returns the values of the last form in BODY. However, if
the execution takes longer than TIMEOUT seconds, abort it. If :ERROR-P is
unsupplied or false, just return nil. If :ERROR-P is true, signal an error."
  `(handler-case 
     (sb-ext:with-timeout ,timeout ,@body)
     (sb-ext:timeout (timeout)
                     (if ,error-p (error "Timeout ~a~%" timeout)
                         (values)))))

#|
(progn
(with-timeout (1 :error-p nil)
(sleep 3))
42)

(progn
(with-timeout (1 :error-p t)
(sleep 3))
42)
|#


;;; mp:without-scheduling or mp:without-interrupts
;;; use as in aserve

;;; this is defined in without-preemption
;;; it better shoudn't
;;; fixme
(defvar %other-mutex% (make-lock "Mutex for any other business"))

(defmacro without-preemption (&body body)
  `(sb-thread:with-recursive-lock (%other-mutex%)  ,@body))

(defvar %incf-mutext% (make-lock "Incf mutex for incf, decf, ush & replacef"))

(defmacro without-preemption-special (&body body)
  `(sb-thread:with-recursive-lock (%incf-mutext%)  ,@body))

(defmacro atomic-conditional-replacef (reference predicate new-value)
  "When PREDICATE returns non-null, this setfs REFERENCE to NEW-VALUE.
Predicate is called (OLD-VALUE NEW-VALUE). The operation
assures that predicate application and swap are atomic."
  (let ((old-value (gensym))
        (new-value-var (gensym)))
    `(without-preemption-special  
      (let ((,old-value ,reference)
            (,new-value-var ,new-value))
        (when (funcall ,predicate ,old-value ,new-value-var)
          (setf ,reference ,new-value-var)
          (values ,old-value t))))))

(defmacro atomic-decf (reference &optional (delta 1))
  "Atomically decrements REFERENCE by DELTA."
  `(without-preemption-special
    (decf ,reference ,delta)))

(defmacro atomic-incf (reference &optional (delta 1))
  "Atomically increments REFERENCE by DELTA."
  `(without-preemption-special
    (incf ,reference ,delta)))

(defmacro atomic-push (item reference)
  "Atomically pushes ITEM onto REFERENCE."
  `(without-preemption-special
    (push ,item ,reference)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Class hierarchy
(defclass lock ()
  ((name :initarg :name :reader lock-name))
  (:documentation "The foundation of all locks."))


(defclass null-lock (lock)
  ()
  (:documentation "A lock that is always free."))

(defclass simple-lock (lock)
  ((owner :initform nil :accessor lock-owner))
  (:documentation "A lcok that is either free or busy."))

(defun make-null-lock (name)
  (make-instance 'null-lock :name name))

(defun make-simple-lock (name)
  (make-instance 'simple-lock :name name))

;; Interface

(defgeneric seize (lock)
  (:documentation
   "Seizes the lock.
    Rteruns the lock when the operation succeeds.
    Some locks simply wait until they can succeed, while
    other locks return NIL if they fail."))

(defgeneric release (lock &optional failure-mode)
  (:documentation
   "Releases the lock if it is currently owned by this process.
    Returns T if the operation succeeds.
    If unsuccessfull and failure-mode is :no-error, returns NIL.
    If unsuccessfull and failure-mode is :error, signals an error.
    The default for failure-mode is :no-error."))

;; locking protocol

(defmethod seize ((l null-lock))
  l) ;return lock, no waiting

(defmethod release ((l null-lock) &optional failure-mode)
  (declare (ignore failure-mode)) ;never fails for null locks
  t)

;; If value of place is old-value, set it to new-value
;; Return t if th esetf worked, nil otherwise
(defmacro setf-if (place old-value new-value)
  `(without-preemption ;do atomically
    (cond ((eql ,place ,old-value)
           (setf ,place ,new-value)
           t)
          (t nil))))

(defmethod check-for-mylock ((l simple-lock) process)
  (when (eql (lock-owner l) process)
    (error "Can't seize ~A because you already own it." l)))

(defvar *current-process*)

(defmethod seize ((l simple-lock))
  (check-for-mylock l *current-process*)
  (do ()
    ((setf-if (lock-owner l) nil *current-process*))
    (process-wait "Seitzing lock"
                  #'(lambda () (null (lock-owner l)))))
  l)

(defmethod release ((l simple-lock) &optional (failure-mode :no-error))
  (or (setf-if (lock-owner l) *current-process* nil)
      (ecase failure-mode
        (:no-error nil)
        (:error (error "~A is not owned by this process" l)))))


(defmethod print-object ((l lock) stream)
  (format stream "#<~S ~A ~D>"
          (type-of l)
          (if (slot-boundp l 'name)
              (lock-name l)
              "(no name)")
          #+sbcl
            (sb-kernel:get-lisp-obj-address l))
  l)

(defmethod describe ((l lock))
  (format t "~&~S is a lock of type ~S named ~A."
          l (type-of l)
          (if (slot-boundp l 'name)
              (lock-name l)
              "(no name)"))
  (values))


(defmethod describe :after ((l simple-lock))
  (let ((owner (lock-owner l)))
    (format t (if owner
                  "~&It is now owned by process ~A. ~%"
                  "~&It is now free. ~%")
            owner)))

(defclass ordered-lock-mixin ()
  ((level 
    :initarg :level
    :reader lock-level
    :type integer))
  (:documentation "Avoidsd deadlock by checking lock order."))

(defclass ordered-lock (ordered-lock-mixin simple-lock)
  ()
  (:documentation
   "Avoids deadlock by ensuring that a process seizes
locks in a specific order.
When seizing, waits if the lock is busy."))

(defclass ordered-null-lock (ordered-lock-mixin null-lock)
  ()
  (:documentation
   "Avoids deadlock by ensuring that a process seizes locks
in a specific order. Does not actually seize anything,
but does check that the lock ordering is obeyed."))

;; constructors
(defun make-ordered-null-lock (name level)
  (make-instance 'ordered-null-lock :name name :level level))

(defun make-ordered-lock (name level)
  (make-instance 'ordered-lock :name name :level level))

;; behaviour for ordered locks
(defmethod describe :after ((l ordered-lock-mixin))
  (format t "~&Its lock level is ~D." (lock-level l)))

;; 1. rule of inheritance: every class has precedence over it's superclasses
;; 2. rule of inheritance: every class determines the precedence of it's superclasses in it's lambda-list (which is read from left-to-right)
;; method-dispatch rules: 1: most-specific-first befores 2: most-specific primary 3: most-specific-last (least-specific-first) afters 4: aux (around) ones
;; mostly standard-object and t are superclasses of any class and standard-object comes with it's own methods, for which it's befores would be having less precedence it's primarys would be having less precedence and it's afters would be having more precedence over any class which inherits from it.
;; there must be at least one applicable primary method, when a gf is called!

;; behaviour for ordered locks
;; interface to the association between processes and their lists of ordered locks.
;; one may change to another representation than hash-tables!

(defvar *process-lock-table* (make-hash-table)
  "Each key is a process identifier;
value is a list of ordered locks it owns")

(defun add-process-lock (process lock)
  (without-preemption-special
   (push lock (gethash process *process-lock-table*))))

(defun delete-process-lock (process lock)
  (without-preemption-special
   (let ((hash-entry (gethash process *process-lock-table*)))
     (setf hash-entry (delete lock hash-entry)))))

(defun get-process-locks (process)
  (without-preemption-special
   (gethash process *process-lock-table*)))

(defun get-highest-lock (process)
  (first (get-process-locks process)))

;; a process cannot seize an ordered lock if it does own a lock with a higher level!
;; for a process to be permissible to seize a lock, it has to be checked that it does not own a lock with a higher level.


(defmethod seize :before ((l ordered-lock-mixin))
  "Checks validity of this process seizing this ordered lock.
If invalid, signals an error.
If valid, does nothing and allows primary method to run."
  ;; First check for the mylock mistake to give the specific
  ;; error for that case, instead of the "Out of order" error.
  (check-for-mylock l *current-process*)
  ;; Now check for a possible infraction (abnormality) of ordered locking.
  (let ((highest-lock (get-highest-lock *current-process*)))
    (when (and highest-lock
               (<= (lock-level l) (lock-level highest-lock)))
      (error "Out of order: Can't seize ~A while owning ~A" l highest-lock))))

;; after (a process, seizing/realeasing, an ordered-lock) the *process-lock-table* has to be updated

(defmethod seize :after ((l ordered-lock-mixin))
  "Adds the lock to the *process-lock-table*"
  (add-process-lock *current-process* l))

(defmethod release :after ((l ordered-lock-mixin) &optional failure-mode)
  "Deletes a lock from the *process-lock-table*"
  (declare (ignore failure-mode))
  (delete-process-lock *current-process* l))

;; make shared resources lockable by incorporating a lock into the data structure representing the shared resource.
;; then write functions which first seize the lock, then access the datastructure, and finally release the lock.
;; locking a shared queue: example of a print spooler.

;; implenting queue of print requests (shared resource).

;datastructure
(defclass print-request-queue ()
  ((lock :accessor print-queue-lock :initform (make-simple-lock "Print Queue")) ;the lock
   (requests :accessor print-requests :initform nil)) ;the list
  (:documentation "Queue of pending print requests."))

;constructor
(defun make-print-queue ()
  (make-instance 'print-request-queue))

(defvar *print-queue* (make-print-queue)) ; only one instance of a print-queue!

;functions for modifying the print-queue!

(defun enqueue-print-request (r)
  (let ((lock (print-queue-lock *print-queue*)))
    (unwind-protect
      (progn (seize lock)
             (push r (print-requests *print-queue*)))
      (release lock-package :no-error))))

(defun dequeue-print-request (r)
  (let ((lock (print-queue-lock *print-queue*)))
    (unwind-protect
      (progn (seize lock)
             (setf (print-requests *print-queue*)
                   (delete r (print-requests *print-queue*))))
      (release lock-package :no-error))))





;;;;; doin the Triangles!
(progn
(defclass shape () () 
  (:documentation "The foundation of all shapes."))


;; this one implements a triangle via sides!
;; we later on want one with via angles just!
;; and a default scale of 1!

(defclass triangle (shape)
  ((a :reader side-a :initarg :side-a)
   (b :reader side-b :initarg :side-b)
   (c :reader side-c :initarg :side-c)))

(defun make-triangle (a b c)
  ;;all sides should be represented as floats
  (make-instance 'triangle 
                 :side-a (coerce a 'float)
                 :side-b (coerce b 'float)
                 :side-c (coerce c 'float)))

;;; Return the angle A between adjacent sides b and c
;;; and opposite side a in radians, given all sides of a triangle
;;; Law of Cosines: a^2 = b^2 + c^2 - 2bc(cos A)
(defun three-sides-to-angle (a b c)
 (acos (/ (- (+ (expt b 2)
           (expt c 2))
        (expt a 2))
     (* 2 b c))))

(defmethod angle-A ((tri triangle) &key)
  (three-sides-to-angle
   (side-a tri) (side-b tri) (side-c tri)))

(defmethod angle-A :around ((tri triangle) &key deg)
  (let ((result (call-next-method)))
    (format t "~A rad, ~A degrees " result (truncate (rad->deg result)))
    (if (eq deg t)
        (rad->deg result)
        result)))

(defmethod angle-B ((tri triangle) &key deg)
          (three-sides-to-angle
           (side-b tri) (side-c tri) (side-a tri)))

(defmethod angle-B :around ((tri triangle) &key deg)
  (let ((result (call-next-method)))
    (format t "~A rad, ~A degrees " result (truncate (rad->deg result)))
    (if (eq deg t)
        (rad->deg result)
        result)))

(defmethod angle-C ((tri triangle) &key deg)
          (three-sides-to-angle
           (side-c tri) (side-a tri) (side-b tri)))

(defmethod angle-C :around ((tri triangle) &key deg)
  (let ((result (call-next-method)))
    (format t "~A rad, ~A degrees " result (truncate (rad->deg result)))
    (if (eq deg t)
        (rad->deg result)
        result)))

;;; here we tried to define an after method for the primary method of the class, which is the wrong
;;; way. We intended to print a symbol/string not after the primary method returns but it's writer!
;; (defmethod angle-A :after ((tri triangle))
;;  (princ 'pi))

;;; so instead of the above we should have defined an after method for the primary writer method
;;; of the slot accessor gf.

(defgeneric dimensions (shape)
  (:documentation "Returns list of angles."))

(defgeneric area (shape)
  (:documentation "Returns area of the shape."))

(defmethod dimensions ((tri triangle))
  (list 
   (side-a tri)
   (side-b tri)
   (side-c tri)))

(defun deg->rad (deg &rest args)
  (/ (* deg pi) 180))

(defun rad->deg (rad &rest args)
  (/ (* rad 180) pi))

(defmethod angles ((tri triangle) &key deg)
  (let* ((gf1 (symbol-function 'angle-A))
        (gf2 (symbol-function 'angle-B))
        (gf3 (symbol-function 'angle-C))
        (meth1 (find-method gf1 '(:around) (list (find-class 'triangle))))
        (meth2 (find-method gf2 '(:around) (list (find-class 'triangle))))
        (meth3 (find-method gf3 '(:around) (list (find-class 'triangle)))))
    (if (eq deg t)
        (mapcar #'rad->deg
                (list
                 (angle-A tri)
                 (angle-B tri)
                 (angle-C tri))) 
        (list
         (angle-A tri)
         (angle-B tri)
         (angle-C tri)))))

;;; Return the area of a triangle
;;; Algorithm is: area = ab(sin C)/2

(defmethod area ((tri triangle))
  (let* ((gf (symbol-function 'angle-C))
         (meth (find-method gf '(:around) (list (find-class 'triangle)))))
    (remove-method gf meth)
    (* (side-a tri) (side-b tri) (sin (angle-C tri)) .5)))

(defmethod area :around ((tri triangle))
  (let* ((result (call-next-method)))
    (format t "~A unit square " result) result)))


;;; test
;; (setq my-tri (make-triangle 60 60 60))
;; (angle-a my-tri :deg t)
;; => 1.0471976 RAD
;; => 60.000000151832296d


