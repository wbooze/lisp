
;; CLIM needs to be loaded too, of course.

(defun load-maxima ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (cl-user::change-directory "/home/oleo2/maxima-code/src/")
    (load "maxima-build.lisp")
    (maxima-load)
    (cl-user::change-directory "/home/oleo2/")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (not (find-package :maxima))
    (load-maxima)))

(eval-when (:compile-toplevel :load-toplevel)
  (require :maxima))

(in-package :maxima)

;; Replace some of maxima's display routines with own.
(defparameter writefilep nil)
(defparameter *maxima-tempdir* "/home/oleo2/temp/")
(defparameter *gnuplot-stream* nil)
(defparameter *gnuplot_term* '$dumb)
(defparameter maxima::*maxima-infodir* "/usr/local/share/info/")
(maxima::initialize-runtime-globals)
(cl-info::load-primary-index)
(setf maxima::$linel 120)
(setf *maxima-initmac* "/home/oleo2/.maxima/maxima-init.mac")

(in-package :clim-user)

(defun make-random-color ()
  (make-rgb-color 
    (/ (+ 50 (random (- 255 50))) 255)
    (/ (+ 50 (random (- 255 50))) 255)
    (/ (+ 50 (random (- 255 50))) 255)))

(defparameter my-color (make-random-color))

(defparameter *listener-use-debug-io* nil)

(defclass listener-view (textual-view) ())

(defclass listener-pointer-documentation-view
  (listener-view pointer-documentation-view)
  ())

(defclass listener-interactor-pane (interactor-pane) ())

(defparameter +listener-view+ (make-instance 'listener-view))
(defparameter +listener-pointer-documentation-view+
  (make-instance 'listener-pointer-documentation-view))

(defparameter *listener-initial-function* nil)

(define-application-frame maxima-repl (standard-application-frame)
  ((system-command-reader :accessor system-command-reader
			    :initarg :system-command-reader
			    :initform t)
   (initial-content :accessor initial-content))
  
  (:panes
   (interactor :interactor :display-time t :incremental-redisplay t :scroll-bars t 
               :background +black+ :foreground +goldenrod+
               :end-of-line-action :wrap :end-of-page-action :scroll :display-function 'display-app))
  (:top-level (default-frame-top-level :prompt 'maxima-prompt))
  (:command-table (maxima-repl
                   :inherit-from (lisp-commands)
		   :menu (("Lisp" :menu lisp-commands))
		   ))
  (:layouts
   (default
       (vertically ()
	 interactor))))

(defun maxima-prompt (stream frame)
  (declare (ignore frame))
  (with-text-style (stream (make-text-style :fix :italic :very-large))
    (princ (maxima::main-prompt) stream)))

(defun display-app (frame pane)
  (with-drawing-options (pane :ink +green-yellow+ :text-style (make-text-style :sans-serif nil :very-large))
      (with-new-output-record (pane)
        (format pane "~a~%~%" (maxima::$load maxima::*maxima-initmac*)))))

(defmethod run-frame-top-level ((frame maxima-repl) &key listener-funcall &allow-other-keys)
  (let ((*debug-io* (if *listener-use-debug-io* *debug-io*
                        (get-frame-pane frame 'interactor)))
	;; Borrowed from OpenMCL.
	;; from CLtL2, table 22-7:
        (*listener-initial-function* listener-funcall)
	(*package* *package*)
	(*print-array* *print-array*)
	(*print-base* *print-base*)
	(*print-case* *print-case*)
	(*print-circle* *print-circle*)
	(*print-escape* *print-escape*)
	(*print-gensym* *print-gensym*)
	(*print-length* *print-length*)
	(*print-level* *print-level*)
	(*print-lines* *print-lines*)
	(*print-miser-width* *print-miser-width*)
	(*print-pprint-dispatch* *print-pprint-dispatch*)
	(*print-pretty* *print-pretty*)
	(*print-radix* *print-radix*)
	(*print-readably* *print-readably*)
	(*print-right-margin* *print-right-margin*)
	(*read-base* *read-base*)
	(*read-default-float-format* *read-default-float-format*)
	(*read-eval* *read-eval*)
	(*read-suppress* *read-suppress*)
	(*readtable* *readtable*))
    (loop while 
      (catch 'return-to-listener
	(restart-case (call-next-method)
	  (return-to-listener ()
	    :report "Return to listener."
	    (throw 'return-to-listener T)))))))

(defvar *boxify* nil)

(defmethod read-frame-command ((frame maxima-repl) &key (stream *standard-input*))  
  "Specialized for the listener, read a lisp form to eval, or a command."
  
  (with-text-style (stream (make-text-style :fix :bold :very-large))
  (let (object)
    (with-input-editing (stream :input-sensitizer
				(lambda (stream cont)
				  (with-output-as-presentation 
				      (stream object 'maxima-expression)
				    (funcall cont))))
      (setq object (accept 'maxima-expression :stream stream :prompt nil
			   :activation-gestures nil))
      (list 'com-eval object)))))

(defun maxima::maxima-read-eval-print-loop ()
  (in-package :maxima)
  (unless *debugger-hook*
    (setf *debugger-hook* #'maxima::maxima-lisp-debugger-repl))
  (let ((eof (gensym)))
    (loop
     (catch 'maxima::to-maxima-repl
       (with-drawing-options (t :ink +red+)
       (maxima::format-prompt t "~%~A> " (package-name *package*))
       (finish-output)
       (let ((input (accept t :stream *standard-input* :prompt nil)))
         (when (eq input eof)
           (fresh-line)
           (maxima::to-maxima))
         (format t "~{~&~12,16@T~S~}" (multiple-value-list (eval input)))))))))

(defun run-maxima-listener (&key (system-command-reader nil)
                            (new-process t)
                            (width 1200)
                            (height 550)
                            (process-name "Maxima Listener")
                            (eval nil))
  (flet ((run ()
           (run-frame-top-level
            (make-application-frame 'maxima-repl
                                    :width width
                                    :height height
                                    :system-command-reader system-command-reader)
	    
            ;:listener-funcall (cond ((null eval) nil)
		;		    ((functionp eval) (funcall eval))
            ; (t (lambda () (eval eval)))))))
           :listener-funcall nil)))
    (let ((*package* (find-package :maxima)))
    (if new-process
        (clim-sys:make-process #'run :name process-name)
        (run)))))

(defun untabify (input)
  (let ((line-posn 0))
    (with-output-to-string (output)
      (dotimes (i (length input))
	(let ((char (char input i)))
	  (case char
	    (#\Tab
	     (let ((spaces (- 4 (rem line-posn 4))))
	       (dotimes (k spaces)
		 (incf line-posn)
		 (write-char #\space output))))
	    (#\Newline
	     (setf line-posn 0)
	     (write-char #\Newline output))
	    (t
	     (incf line-posn)
	     (write-char char output))))))))

(define-presentation-type maxima-expression () :inherit-from t)

(define-command-table lisp-commands :inherit-from nil)


(define-presentation-method present (object (type maxima-expression)
					    stream (view textual-view)
					    &key &allow-other-keys)
  (with-text-style (stream (make-text-style :fix :bold :very-large))
    (cond
      (maxima::*alt-display2d*
	;; This method has problems with scrolling at the bottom of the
	;; screen.
	(let ((*standard-output* stream)
	       (maxima::$display2d t)
	       (maxima::*y-width* (text-style-height (medium-text-style stream)
				    stream))
	       (maxima::*x-width* (text-style-width (medium-text-style stream)
				    stream)))
	  (format *trace-output* "x,y width = ~A ~A~%"
	    maxima::*x-width* maxima::*y-width*)
	  (let ((r (with-new-output-record (t)
		     (maxima::displa object))))
            r)))
      (t
	;; This method works and scrolling is good.
	(princ (untabify (with-output-to-string (s)
			   (let ((*standard-output* s))
			     (if (eq (caar object) 'maxima::displayinput)
			       (let ((maxima::$display2d nil))
				 (maxima::displa `((maxima::mlabel) nil ,@(cddr object))))
			       (maxima::displa object)))))
	  stream)))))


(define-presentation-method accept ((type maxima-expression)
				    stream (view textual-view)
				    &key)
  (with-text-style (stream (make-text-style :fix :bold :very-large))
  (maxima::dbm-read stream nil stream)))

(define-command (com-eval :menu t :command-table lisp-commands)
  ((form 'clim:form :prompt "form") &key (batch-or-demo-flag nil))

  (let* ((r form)
	  (input-stream (get-frame-pane *application-frame* 'interactor))
	  (*standard-input* input-stream)
	  (*standard-output* input-stream)
	  (*terminal-io* input-stream)
	  (*query-io* input-stream)
	  (*debug-io* input-stream)
	  (*error-output* input-stream)
	  (*trace-output* input-stream)
	  (maxima::accumulated-time 0)
	  (maxima::time-before)
	  (maxima::time-after)
	  (maxima::time-used 0)
	  (eof (list nil))
	  (maxima::etime-before)
	  (maxima::etime-after)
	  (maxima::area-before)
	  (maxima::area-after)
	  (maxima::etime-used)
	  (maxima::c-tag)
	  (maxima::d-tag))
    
    (with-drawing-options (input-stream :text-size :very-large :ink (funcall (lambda () +green+)) 
                           :text-style (make-text-style :fix :bold :very-large))
      
      (declare (special *socket-connection*))

      (if (eql batch-or-demo-flag :demo)
	(format t (intl:gettext "~%At the '~A' prompt, type ';' and <enter> to get next demonstration.~&")
	  (maxima::print-invert-case (maxima::stripdollar maxima::$prompt))))

      (catch 'maxima::return-from-debugger

	(when (or (not (maxima::checklabel maxima::$inchar))
		(not (maxima::checklabel maxima::$outchar)))
	  (incf maxima::$linenum))

	(setq maxima::c-tag (maxima::makelabel maxima::$inchar))

	(let ((maxima::*mread-prompt* (if batch-or-demo-flag nil (maxima::main-prompt)))
	       (maxima::eof-count 0))
	  
	  ;; This is something of a hack. If we are running in a server mode
	  ;; (which we determine by checking *socket-connection*) and we get
	  ;; an eof on an input-stream that is not *standard-input*, switch
	  ;; the input stream to *standard-input*.
	  ;; There should probably be a better scheme for server mode.
	  ;; jfa 10/09/2002.
	  (cond ((and (consp r) (keywordp (car r)))
		  (maxima::break-call (car r) (cdr r) 'maxima::break-command))))
	
	(format t "~a~&" maxima::*general-display-prefix*)
	
	(if (eq r eof) (return maxima::$done))
	
        (setq maxima::$__ (caddr r))
	
	(unless maxima::$nolabels
	  (set  maxima::c-tag maxima::$__))

	(cond (batch-or-demo-flag
		(let ((maxima::$display2d nil))
		  (present `((maxima::mlabel) ,maxima::c-tag ,maxima::$__) 'maxima-expression))))
	
	(setq maxima::time-before (get-internal-run-time)
	  maxima::etime-before (get-internal-real-time))
	
	(setq maxima::area-before (maxima::used-area))
	
	(setq maxima::$% (maxima::toplevel-macsyma-eval maxima::$__))
	
	(setq maxima::etime-after (get-internal-real-time)
	  maxima::time-after (get-internal-run-time))
	
	(setq maxima::area-after (maxima::used-area))
	
	(setq maxima::time-used (/ 
				  (float (- maxima::time-after maxima::time-before))
				  internal-time-units-per-second)
	  maxima::etime-used (/ 
			       (float (- maxima::etime-after maxima::etime-before))
			       internal-time-units-per-second))
	
	(incf maxima::accumulated-time maxima::time-used)
	
	(setq maxima::d-tag (maxima::makelabel maxima::$outchar))
	
	(unless maxima::$nolabels
	  (set maxima::d-tag maxima::$%))
	
	(setq maxima::$_ maxima::$__)
	
	(unless maxima::$nolabels
	  (maxima::putprop maxima::d-tag (cons maxima::time-used  0) 'maxima::time))

	(if (eq (car r) :lisp)
            (format nil "~a~&" `(,maxima::$%))
            (present `((maxima::mlabel) ,maxima::d-tag ,maxima::$%) 'maxima-expression))
        
	(when (eq batch-or-demo-flag ':demo)
	  (princ (maxima::break-prompt))
	  (force-output)
	  (let (quitting)
	    (do ((char)) (nil)
	      ;;those are common lisp characters you're reading here
	      
	      (case (setq char (read-char *standard-output*))
		((#\page)
		  (fresh-line)
		  (princ (maxima::break-prompt))
		  (force-output))
		((#\?)
		  (format t
		    (intl:gettext    
		      "  Pausing. Type a ';' and <enter> to continue demo.~%")))
		((#\space #\; #\n #\e #\x #\t))
		((#\newline )
		  (if quitting (throw 'maxima::abort-demo input-stream) (return nil)))
		(t (setq quitting t))))))
	
	;; This is sort of a kludge -- eat newlines and blanks so that
	;; they don't echo

	(and batch-or-demo-flag
	  (do ((char)) (())
	    (setq char (read-char input-stream nil nil))
	    (when (null char)
	      (throw 'maxima::macsyma-quit input-stream))
	    (unless (member char '(#\space #\newline #\return #\tab) :test #'equal)
	      (unread-char char input-stream)
	      (return nil))))))))
