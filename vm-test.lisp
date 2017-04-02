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

(defvar *codops* 
  #((pushi i) 
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

(defstruct vm "Implementation of a simple Stack Virtual Machine"
           (stack (make-array 100))
           (memory (make-array 26 :initial-element 0))
           (program (make-array 1 :initial-element +stop+))
           (sp 1)
           (pc 0))

(defun vm-reset (vm)
  (setf (vm-stack  vm) (make-array 100 :fill-pointer 0)
    (vm-memory vm) (make-array  26 :initial-element 0)
    (vm-pc vm) 0
    (vm-sp vm) 0)
  (values))

(defvar *names*  "abcdefghijklmnopqrstuvwxyz")
(defun index-to-name (i)
  (aref *names* i))
(defun index-from-name (name)
  (position name *names*))

(defun vm-dump (vm)
  (loop
    :for i :below (vm-sp vm)
    :initially (format t "--------------------~%")
    :do (format t "[~3D]: ~10D~%" i (aref (vm-stack vm) i))
    :finally  (format t "--------------------~%"))
  (loop
    :for i :below (length (vm-memory vm))
    :do (format t "[~3D]: ~10D~%" (index-to-name i) (aref (vm-memory vm) i))
    :finally  (format t "--------------------~%")))

(defun namep       (x) (consp x))
(defun wrap-name   (x) (cons 'name x))
(defun unwrap-name (x) (cdr x))

(defun vm-push (vm n)
  (if (< (vm-sp vm) (array-dimension (vm-stack vm) 0))
      (setf (aref (vm-stack vm) (vm-sp vm)) n
            (vm-sp vm) (1+ (vm-sp vm)))
      (error "Stack is full - vm-push")))

(defun vm-pushv (vm n)
  (if (< (vm-sp vm) (array-dimension (vm-stack vm) 0))
      (setf (aref (vm-stack vm) (vm-sp vm))  (wrap-name n)
            (vm-sp vm) (1+ (vm-sp vm)))
      (error "Stack is full - vm-pushv")))

(defun vm-pop (vm )
  (if (plusp (vm-sp vm))
      (progn
        (decf (vm-sp vm))
        (let ((value (aref (vm-stack vm) (vm-sp vm))))
          (if (namep value)
              (vm-retrieve vm value)
              value)))
      (error "Stack is empty - vm-pop")))

(defun vm-popv (vm)
  (if (plusp (vm-sp vm))
      (progn
        (decf (vm-sp vm))
        (aref (vm-stack vm) (vm-sp vm)))
      (error "Stack is empty - vm-popv")))

(defun vm-store (vm d x)
  (if (< -1 (unwrap-name d) (length (vm-memory vm)))
      (setf (aref (vm-memory vm) (unwrap-name d)) x)
      (error "Invalid memory address ~D" d)))

(defun vm-retrieve (vm d)
  (if (< -1 (unwrap-name d) (length (vm-memory vm)))
      (aref (vm-memory vm) (unwrap-name d)) 
      (error "Invalid memory address ~D" d)))

(defun vm-disassemble (vm &optional (pc 0))
  (format t "~3D: " pc)
  (macrolet ((next-instruction ()
               `(aref (vm-program vm) (prog1 pc (incf pc)))))
    (let ((code (next-instruction)))
      (format t "~(~A~)~%"
              (if (< -1 code (length *codops*))
                  (let ((discode (aref *codops* code)))
                    (if (< 1 (length discode))
                        (list (first discode)
                              (ecase (second discode)
                                ((i) (next-instruction))
                                ((n) (intern (string-upcase
                                              (index-to-name
                                               (next-instruction)))))))
                        discode))
                  `(error ,code)))
      pc)))

(defun vm-run (vm &key (trace nil) (reset t) (pc (vm-pc vm)))
  (when reset
    (vm-reset vm))
  (setf pc (vm-pc vm))
  (with-accessors ((program vm-program)
		    (pc vm-pc)) vm
    (when trace
      (let ((*print-pretty* t)
	     (*print-right-margin* 72))
        (format t "~A~%PC: ~3D~%" program pc))
      (vm-dump vm))

    (handler-case
      (loop
	(when trace (vm-disassemble vm pc))
	(macrolet ((next-instruction ()
		     `(aref program (prog1 pc (incf pc)))))
	  (let ((code (next-instruction)))
	    (case code
	      (#.+pushi+ (vm-push  vm (next-instruction)))
	      (#.+pushn+ (vm-pushv vm (next-instruction)))
	      (#.+store+ (vm-store vm (vm-popv vm) (vm-pop vm)))
	      (#.+neg+   (vm-push vm (- (vm-pop vm))))
	      (#.+add+   (vm-push vm (+ (vm-pop vm) (vm-pop vm))))
	      (#.+mul+   (vm-push vm (* (vm-pop vm) (vm-pop vm))))
	      (#.+sub+   (let ((b (vm-pop vm))
				(a (vm-pop vm)))
			   (vm-push vm (- a b))))
	      (#.+div+   (let ((b (vm-pop vm))
				(a (vm-pop vm)))
			   (when (zerop b)
			     (error 'division-by-zero))
			   (vm-push vm (/ a b))))
	      (#.+print+ (format t "~D~%" (vm-pop vm)))
	      (#.+dump+  (vm-dump vm))
	      (#.+stop+  (return-from vm-run pc))))))
      (error (err)
        (format *error-output* "PC = ~D~%~A~%" pc err)))))

(defun test/vm-push ()
  (let* ((vm (make-vm))
	  (old-sp (vm-sp vm)))
    (vm-push vm 42)
    (assert (= (1+ old-sp) (vm-sp vm))))
  :success)

(test/vm-push)

(vm-run (make-vm :program (vector
			    ;; 5 5 + d !
			    ;; d d * c !
			    ;; d c * m !
			    ;; d =
			    ;; c =
			    ;; m =
			    ;; 4 6 9 1 0 d*+d*+d*+d*+ a ! a =
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
			    +stop+))
  :trace t)

;; prints:
;; 10
;; 100
;; 1000
;; 1964
;; --> 66 
