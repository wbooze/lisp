
(defvar *stack*  (make-array 100 :fill-pointer 0))
(defvar *memory* (make-array  26 :initial-element 0))

(defun reset ()
  (setf *stack*  (make-array 100 :fill-pointer 0)
        *memory* (make-array  26 :initial-element 0))
  (values))

(defvar *names*  "abcdefghijklmnopqrstuvwxyz")

(defun index-to-name (i)
  (aref *names* i))
(defun index-from-name (name)
  (position name *names*))


(defun dump ()
  (loop
    :for i :below (fill-pointer *stack*)
    :initially (format t "--------------------~%")
    :do (format t "[~3D]: ~10D~%" i (aref *stack* i))
    :finally  (format t "--------------------~%"))
  (loop
    :for i :below (length *memory*)
    :do (format t "[~3D]: ~10D~%" (index-to-name i) (aref *memory* i))
    :finally  (format t "--------------------~%")))

(defun namep       (x) (consp x))
(defun wrap-name   (x) (cons 'name x))
(defun unwrap-name (x) (cdr x))

(defun vm-push (n)
  (if (< (fill-pointer *stack*) (array-dimension *stack* 0))
      (vector-push n *stack*)
      (error "Stack is full")))

(defun vm-pushv (n)
  (if (< (fill-pointer *stack*) (array-dimension *stack* 0))
      (vector-push (wrap-name n) *stack*)
      (error "Stack is full")))

(defun vm-pop ()
  (if (plusp (fill-pointer *stack*))
      (let ((value (vector-pop *stack*)))
        (if (namep value)
            (vm-retrieve value)
            value))
      (error "Stack is empty")))

(defun vm-popv ()
  (if (plusp (fill-pointer *stack*))
      (vector-pop *stack*)
      (error "Stack is empty")))

(defun vm-store (d x)
  (if (< -1 (unwrap-name d) (length *memory*))
      (setf (aref *memory* (unwrap-name d)) x)
      (error "Invalid memory address ~D" d)))

(defun vm-retrieve (d)
  (if (< -1 (unwrap-name d) (length *memory*))
      (aref *memory* (unwrap-name d)) 
      (error "Invalid memory address ~D" d)))



(defconstant +pushi+ 0)
(defconstant +pushn+ 1)
(defconstant +store+ 2)
(defconstant +neg+   3)
(defconstant +add+   4)
(defconstant +mul+   5)
(defconstant +sub+   6)
(defconstant +div+   7)
(defconstant +print+ 8)
(defconstant +dump+  9)
(defconstant +stop+ 10)

(defvar *codops* #((pushi i) 
                   (pushn n) 
                   (store) 
                   (neg)   
                   (add)   
                   (mul)   
                   (sub)   
                   (div)   
                   (print) 
                   (dump)  
                   (stop)))

(defun vm-disassemble (pgm pc)
  (format t "~3D: " pc)
  (let ((code (aref pgm (prog1 pc (incf pc)))))
    (format t "~S~%"
            (if (< -1 code (length *codops*))
                (let ((discode (aref *codops* code)))
                  (if (< 1 (length discode))
                      (list (first discode)
                            (ecase (second discode)
                              ((i) (aref pgm (prog1 pc (incf pc))))
                              ((n) (intern (string-upcase
                                            (index-to-name
                                             (aref pgm (prog1 pc (incf pc)))))))))
                      discode))
                `(error ,code)))
    pc))


(defun vm-run (pgm &key (trace nil) (reset t) (pc 0))
  (when reset (reset))
  (when trace
    (let ((*print-pretty* t)
          (*print-right-margin* 72))
      (format t "~A~%PC: ~3D~%" pgm pc))
    (dump))
  (let ((pc pc))
    (handler-case
        (loop
          (when trace (vm-disassemble pgm pc))
          (let ((code (aref pgm (prog1 pc (incf pc)))))
            (case code
              (#.+pushi+ (vm-push  (aref pgm (prog1 pc (incf pc)))))
              (#.+pushn+ (vm-pushv (aref pgm (prog1 pc (incf pc)))))
              (#.+store+ (vm-store (vm-popv) (vm-pop)))
              (#.+neg+   (vm-push (- (vm-pop))))
              (#.+add+   (vm-push (+ (vm-pop) (vm-pop))))
              (#.+mul+   (vm-push (* (vm-pop) (vm-pop))))
              (#.+sub+   (let ((b (vm-pop))
                               (a (vm-pop)))
                           (vm-push (- a b))))
              (#.+div+   (let ((b (vm-pop))
                               (a (vm-pop)))
                           (when (zerop b)
                             (error 'division-by-zero))
                           (vm-push (/ a b))))
              (#.+print+ (format t "~D~%" (vm-pop)))
              (#.+dump+  (dump))
              (#.+stop+  (return-from vm-run pc)))))
      (error (err)
        (format *error-output* "PC = ~D~%~A~%" pc err)))))

;; 5 5 + d !
;; d d * c !
;; d c * m !
;; d =
;; c =
;; m =
;; 4 6 9 1 0 d*+d*+d*+d*+ a ! a =

(vm-run (vector
         +pushi+ 5 +pushi+ 5 +add+ +pushn+ 3 +store+ ; +dump+
         +pushn+ 3 +pushn+ 3 +mul+ +pushn+ 2 +store+ ; +dump+ 
         +pushn+ 3 +pushn+ 2 +mul+ +pushn+ 12 +store+ ; +dump+
         +pushn+ 3 +print+
         +pushn+ 2 +print+
         +pushn+ 12 +print+
         +pushi+ 4 +pushi+ 6 +pushi+ 9 +pushi+ 1 +pushi+ 0
         +pushn+ 3 +mul+ +add+
         +pushn+ 3 +mul+ +add+
         +pushn+ 3 +mul+ +add+
         +pushn+ 3 +mul+ +add+
         +pushn+ 0 +store+
         +pushn+ 0 +print+
         +stop+)
        :trace nil)

;; prints:
;; 10
;; 100
;; 1000
;; 1964
;; --> 66 
