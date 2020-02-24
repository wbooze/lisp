(defpackage :m-test 
  (:use :common-lisp :clim)
  (:shadowing-import-from  :common-lisp #:interactive-stream-p)
  (:export #:m-repl))

(in-package :m-test)

(setf *read-default-float-format* 'double-float)

(defparameter *text-size* 20)

(defun make-random-color ()
  (clim:make-rgb-color 
    (/ (+ 50 (random (- 255 50))) 255)
    (/ (+ 50 (random (- 255 50))) 255)
    (/ (+ 50 (random (- 255 50))) 255)))

(defgeneric display-formula (stream form))
(defgeneric display-function (stream name))
(defgeneric display-compound-form (form operator operands))

(defmethod display-formula (stream form)
  (princ form stream) (values))

(defmethod display-formula (stream (form list))
  (display-compound-form stream (first form) (rest form)) (values))

(defmethod display-function (stream name)
  (princ name stream) (values))

(defmethod display-compound-form (stream operator operands)
  (display-function stream operator)
  (write-char #\( stream)
  (loop for exp in operands and idx from 0 by 1 do
    (progn
      (unless (zerop idx)
	(write-string " " stream))
      (display-formula stream exp)))
  (write-char #\) stream)
  (values))

(defun connect-addition-operands (a b stream)
  "Print connecting operator between A and B, and present B (A should already have been printed)"
  (unless (and (listp b)
	    (eq (first b) '-)
	    (= (length b) 2))
    (write-string "+" stream))
  (display-formula stream b))

(defun print-in-parens (stream fn)
  (write-string "(" stream)
  (funcall fn)
  (write-string ")" stream))

(defmacro with-parens ((stream) &body body)
  `(print-in-parens ,stream (lambda () ,@body)))

(defun connect-subtraction-operands (a b stream)
  "Print connecting operator between A and B, and present B (A should already have been printed)"
  (let ((outer (if (and (listp b)
		     (eq (first b) '-)
		     (= (length b) 2))
		 #'print-in-parens
		 #'funcall)))
    (write-string "-" stream)
    (funcall outer (lambda () (display-formula stream b)))))

(defun dopairs (fn list args)
  (when (and list (> (length list) 1))
    (apply fn (first list) (second list) args)
    (dopairs fn (rest list) args)))

(defmethod display-compound-form (stream (operator (eql '+)) operands)
  (when operands
    (display-formula stream (first operands))
    (dopairs #'connect-addition-operands operands (list stream))))

(defmethod display-compound-form (stream (operator (eql '-)) operands)
  (cond ((zerop (length operands))
	  (error "No arguments to subtraction operator"))
    ((= 1 (length operands))
      (write-string "-" stream)
      (display-formula stream (first operands)))
    (t (display-formula stream (first operands))
      (dopairs #'connect-subtraction-operands operands (list stream)))))

(defun connect-multiplication-operands (a b stream)
  (write-string "*" stream)
  (display-formula stream b))

(defmethod display-compound-form (stream (operator (eql '*)) operands)
  (cond ((zerop (length operands))
	  (error "No arguments to multiplication operator"))        
    (t (display-formula stream (first operands))           
      (dopairs #'connect-multiplication-operands operands (list stream)))))

(defun connect-division-operands (a b stream)
  (write-string "/")
  (display-formula stream b))

(defmethod display-compound-form (stream (operator (eql '/)) operands)
  (cond
    ((zerop (length operands))
      (error "No arguments to division operator"))
    ((=1 (length operands)
       (write-string "1/" stream)
       (display-formula stream (first operands))))
    (t (display-formula stream (first operands))
      (dopairs #'connect-division-operands operands (list stream)))))

(defun superscript-text-size (size)
  (assert (numberp size))
  (max 10 (round (* 0.5 size))))  ;; FIXME stream

(defmethod display-compound-form (stream (operator (eql 'expt)) operands)
  (display-formula stream (first operands))
  (write-string "^" stream)
  (display-formula-stream (second operands)))

(defmethod display-compound-form ((stream clim:extended-output-stream) (operator (eql 'expt)) operands)
  (unless (= (length operands) 2)
    (error "EXPT requires 2 operands"))
  (let ((base-or (clim:with-new-output-record (stream)
		   (with-parens (stream) (display-formula stream (first operands))))))
    ;(display-formula stream (first operands)))))
     (multiple-value-bind (cx cy)  (clim:stream-cursor-position stream)
      (let* ((*text-size* (superscript-text-size (clim:text-style-size (clim:medium-text-style stream))))
	      (exponent-or
		(clim:with-output-to-output-record (stream)
		  (display-formula stream (second operands))))
	      (h0 (clim:bounding-rectangle-height exponent-or))
	      (h1 (clim:bounding-rectangle-height base-or))
	      (dy (max 0.0 (- h0 (* 0.2 h1)))))
	(setf (clim:output-record-position exponent-or)
	  (values cx (- cy dy)))
        (clim:stream-add-output-record stream exponent-or)
        (clim:stream-close-text-output-record stream)))))

;; Magic CLIM bits

(defmethod display-compound-form ((stream clim:extended-output-stream) (operator (eql '/)) operands)
  (cond ((zerop (length operands))
	  (error "No arguments to division operator"))
    ((= 1 (length operands))  ;; FIXME
      (write-string "1/" stream)
      (display-formula stream (first operands)))
    ((= 2 (length operands)) ;; This is the pretty case which we should normalize toward.
      (multiple-value-bind (cx cy)  (clim:stream-cursor-position stream)
	(let* ((dividend-or (clim:with-output-to-output-record (stream)
			      (display-formula stream (first operands))))
		(divisor-or (clim:with-output-to-output-record (stream)
			      (display-formula stream (second operands))))
		(width (* 1.15 (max (clim:bounding-rectangle-width dividend-or)
				 (clim:bounding-rectangle-width divisor-or))))
		(sum-height (+ (clim:bounding-rectangle-height dividend-or)
			      (clim:bounding-rectangle-height divisor-or)))
		(split 1/4)
		(thickness-ratio 0.5)
		(size (* 0.04 sum-height))
		(thickness (* thickness-ratio size))
		(y0 (clim:bounding-rectangle-height dividend-or))
		(y1 (+ y0 (* split size)))
		(y2 (+ y0 size))
		(combined-or (clim:with-output-to-output-record (stream)
			       (setf (clim:output-record-position dividend-or)
				 (values (/ (- width (clim:bounding-rectangle-width dividend-or)) 2)
				   0)
				 (clim:output-record-position divisor-or)
				 (values (/ (- width (clim:bounding-rectangle-width divisor-or)) 2)
				   y2))
			       (clim:stream-add-output-record stream dividend-or)
			       (clim:draw-line* stream 0 y1 width y1
				 :line-thickness thickness
				 :line-cap-shape :round)
			       (clim:stream-add-output-record stream divisor-or))))
	  (setf (clim:output-record-position combined-or)
	    (values cx
	      (- cy 3 (clim:bounding-rectangle-height dividend-or)
		(- (/ (clim:text-style-height (clim:medium-text-style stream) stream) 2))))
	    #+nil (clim:stream-cursor-position stream) #+nil 
	    (values (+ cx (clim:bounding-rectangle-width combined-or)) cy))
	  (clim:stream-add-output-record stream combined-or)
	  (clim:stream-close-text-output-record stream)
	  #+nil (clim:replay-output-record combined-or stream))))
    (t (display-formula stream (first operands))
      (dopairs #'connect-division-operands operands (list stream)))))                                                                   

;; This :around method is where most of the CLIM magic occurs (output is captured into
;; presentations, and some formatting kludgery occurs)
(defmethod display-formula :around ((stream clim:extended-output-stream) form)
  (clim:with-text-size (stream *text-size*)
    (multiple-value-bind (cx cy)  (clim:stream-cursor-position stream)
      (let
	((record
	   (clim:with-output-to-output-record (stream) ;; FIXME why are forms seemingly not presented?
	     (clim:with-output-as-presentation (stream form (if (listp form) 'form (clim:presentation-type-of form))) ;; this is suspect..
	       (call-next-method stream form)))))
	(clim:with-bounding-rectangle* (x0 y0 x1 y1) record          
	  (setf (clim:output-record-position record) (values (+ x0 1 cx) (+ y0 1 cy))))
	(clim:stream-add-output-record stream record)
	(clim:stream-close-text-output-record stream)
	(multiple-value-bind (nx ny)  (clim:stream-cursor-position stream)
	  (setf (clim:stream-cursor-position stream)
	    (values (+ cx 3 (clim:bounding-rectangle-width record)) cy)))
	#+NIL
        (clim:with-bounding-rectangle* (x0 y0 x1 y1) record
	  (hef:debugf x0 y0 x1 y1)
	  (clim:draw-rectangle* stream x0 y0 x1 y1 :filled nil :ink clim:+blue+))
	(when (clim:stream-drawing-p stream)
	  (clim:replay-output-record record stream))(values)))))

(defun weight (operator)         ;Determine weight of operator.
  (case operator
    (= 0)
    (+ 1)
    (- 1)
    (* 2)
    (/ 2)
    (\\ 2)
    (\^ 3)
    (t 9)))                      ;Unrecognized operator.

(defun opcode (operator)           ;Get appropriate primitive
  (case operator
    (= 'setf)
    (+ '+)
    (- '-)
    (* '*)
    (/ '/ )
    (\\ 'rem)
    (\^ 'expt)
    (t operator)))                 ;Unrecognized operator.

(defun inf-to-pre (ae)
  (if (atom ae) ae                          ;Check for easy case.
      (inf-aux ae nil nil)))                ;Start with empty stacks.

(defun inf-aux (ae operators operands)
  (inf-iter (rest ae)                           ;Work on rest after
            operators
            (cons (inf-to-pre (first ae))       ; recursing on first.
                  operands)))                  

(defun inf-iter (ae operators operands)
  (cond ((and (endp ae) (endp operators))       ;Termination?
         (first operands))                      ;Result.
        ((and (not (endp ae))                   ;Not end of \sy{AE}?
              (or (endp operators)              ;Empty stack?
                  (> (weight (first ae))        ;Compare weights.
                     (weight (first operators)))))
         (inf-aux (rest ae)
                  (cons (first ae) operators)   ;Push operator
                  operands))                    ; and continue.
        (t (inf-iter ae
                     (rest operators)                  ;Pop operator,
                     (cons (list (opcode (first operators))
                                 (second operands)     ; construct
                                 (first operands))     ; result,
                           (rest (rest operands))))))) ; pop operands.


(defun pre-to-inf (l)
  (cond ((null l) nil)
        ((atom l) l)
        (t (list (pre-to-inf (second l))
                 (opsymbol (first l))
                 (pre-to-inf (third l))))))

(defun opsymbol (x)
  (case x
    (setf '=)
    (+ '+)
    (- '-)
    (* '*)
    (/ '/)
    (rem '|\\|)
    (expt '^)
    (t x)))

(defun precedence (x)
  (case x
    (setf 0)
    (+ 1)
    (- 1)
    (* 2)
    (/ 3)
    (rem 3)
    (expt 4)
    (t 9)))

(defun pre-to-inf (l)
  (pre-to-inf-aux l -1))

(defun pre-to-inf-aux (l win)
  (cond ((null l) l)
        ((atom l) (list l))
        (t (let ((wout (precedence (first l))))
             (if (< wout win)
                 (list (pre-to-inf-sub l wout))
                 (pre-to-inf-sub l wout))))))

(defun pre-to-inf-sub (l wout)
  (append (pre-to-inf-aux (second l) wout)
          (list (opsymbol (first l)))
          (pre-to-inf-aux (third l) wout)))

;; My Part !

(defun pre2in (expr)
  "translate prefix to infix expressions.
  handles operators with any number of args."
  
  (labels ((intersperse (thing list)
              "Put thing between each of the elements in list. 
              (intersperse '|,| '(1 2 3)) => (1 |,| 2 |,| 3)"
              
              (if (symbolp thing)
                  (rest (mapcan (lambda (x) (list thing x)) list))
                  (when (stringp thing)
                      (string-trim "()" (write-to-string (rest (mapcan (lambda (x) (list thing x)) list)))))))
           
           (remove-brackets (lst)
              "reduces lists with just one item to the item itself"
              (do ((result lst (car result))) ((or (not (consp result)) (not (null (cdr result)))) result)))

           (separate-list (lst separator test)
              "returns list of sub-sequences defined by separator"
              (if (not (consp lst))
                  lst
                  (let ((result (cons separator nil))
                        (end 0)
                        (sub)
                        (lst
                         (if (funcall test (car lst) separator)
                             (cdr lst)
                             lst)))
                    (do ()
                      ((null lst) result)
                      (setf end (position separator lst :test test))
                      (setf sub (cons (subseq lst 0 end) nil))
                      (setf result (append result sub))
                      (setf lst
                            (if end
                                (nthcdr (+ 1 end) lst)
                                nil)))
                    (setf (cdr result) (mapcar #'remove-brackets (cdr result)))
                    result)))

           (separate-tree (lst separator test)
              "apply separate-list on all sublists"
              (if (or (not (consp lst)) (eql (first lst) 'quote))
                  lst
                  (progn
                    (setf lst
                          (mapcar
                           #'(lambda (x)
                               (if (not (consp x))
                                   x
                                   (separate-tree x separator test)))
                           lst))
                    (if (not (find separator (rest lst)))
                        lst
                        (separate-list lst separator test))))))
                
           (let ((result
                  (if (atom expr)
                      expr
                      (intersperse (car expr) (mapcar #'pre2in (cdr expr)))))
                 (seps '((setf . =) (setq . =) (expt . ^) (rem . |\\|) (mod . |\\|))))
              (setf result (sublis seps result))
              result)))

(defun in2pre (infix-expr &key (test #'eql))
  "converts an infix expression to prefix"
    (labels ((intersperse (thing list)
              "Put thing between each of the elements in list.
              (intersperse '|,| '(1 2 3)) => (1 |,| 2 |,| 3)"
              
              (if (symbolp thing)
                  (rest (mapcan (lambda (x) (list thing x)) list))
                  (if (stringp thing)
                      (string-trim "()" (write-to-string (rest (mapcan (lambda (x) (list thing x)) list)))))))
           
           (remove-brackets (lst)
              "reduces lists with just one item to the item itself"
              (do ((result lst (car result))) ((or (not (consp result)) (not (null (cdr result)))) result)))

           (separate-list (lst separator test)
              "returns list of sub-sequences defined by separator"
              (if (not (consp lst))
                  lst
                  (let ((result (cons separator nil))
                        (end 0)
                        (sub)
                        (lst
                         (if (funcall test (car lst) separator)
                             (cdr lst)
                             lst)))
                    (do ()
                      ((null lst) result)
                      (setf end (position separator lst :test test))
                      (setf sub (cons (subseq lst 0 end) nil))
                      (setf result (append result sub))
                      (setf lst
                            (if end
                                (nthcdr (+ 1 end) lst)
                                nil)))
                    (setf (cdr result) (mapcar #'remove-brackets (cdr result)))
                    result)))

           (separate-tree (lst separator test)
              "apply separate-list on all sublists"
              (if (or (not (consp lst)) (eql (first lst) 'quote))
                  lst
                  (progn
                    (setf lst
                          (mapcar
                           #'(lambda (x)
                               (if (not (consp x))
                                   x
                                   (separate-tree x separator test)))
                           lst))
                    (if (not (find separator (rest lst)))
                        lst
                        (separate-list lst separator test))))))

      (let ((result infix-expr) 
            (separators `(+ - * / = ^ ** setf setq expt))
            (seps `((= . setf) (^ . expt) (|\\| . rem))))
        (dolist (sep separators) (setf result (separate-tree result sep test)))
        (sublis seps (remove-brackets result)))))

(defun m-repl (&optional (stream *standard-output*))
  (let ((medium (clim:sheet-medium stream))
        (text-style (clim:make-text-style :fix :bold :very-large))
        (foreground climi::+wheat1+))
    (block nil      
      (clim:with-drawing-options (stream :ink climi::+yellow+ :text-size :very-large)
       (print 'Infix->))
      (setf (slot-value medium 'clim::text-style) (clim:make-text-style nil :bold :very-large))
      (setf (slot-value medium 'climi::foreground) climi::+goldenrod+)
      (let* ((*package* #.*package*)
            (form (clim:accept t :prompt nil :stream *standard-input*)))
        (declare (special form))
        (clim:with-drawing-options (*standard-input* :ink climi::+red+ :text-style (clim:make-text-style nil :italic nil))
         (clear-output)
          (terpri)
          (clim:formatting-item-list (stream :initial-spacing t)
           (clim:formatting-cell (stream)
            (display-formula stream 
                             (if (eql (values form) 'quit)
                                 (progn
                                   (setf (slot-value medium 'clim::text-style) text-style)
                                   (setf (slot-value medium 'climi::foreground) foreground)
                                   (return-from nil))
                                 (eval (inf-to-pre (values form)))))))))
      (m-repl))))
