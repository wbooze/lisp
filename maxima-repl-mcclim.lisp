;;; Copyright (c) 2005 by Raymond Toy.  License: GPL
;;;
;;; A very gross hack to hook up maxima to mcclim.
;;;
;;; Make sure you have McCLIM and Maxima loaded.  Then compile and
;;; load this file.  Run (clim-user::run-listener) to start the
;;; window.  In the window, you can type typical maxima expressions
;;; and see the output.
;;;
;;; A couple of notes about input:
;;;   o The terminating ";" or "$" will terminate input---you don't need to press enter.
;;;   o If you have a syntax error like mismatched parentheses, the
;;;     parser will indicate it immediately in the gray area at the
;;;     bottom of the screen.  This is actually quite nice.
;;;
;;; One some systems, it seems that the input and output lines (%i and
;;; %o) are clickable and clicking on them will paste them in as if
;;; you had typed the expression.  This needs work, however.

;; Maxima needs to be loaded before this file can work!
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

(in-package :clim-user)
(load "/home/oleo2/lisp/formula.lisp")

(in-package :maxima)

;; Replace some of maxima's display routines with own.
(defparameter writefilep nil)
(defparameter *maxima-tempdir* "/home/oleo2/temp/")
(defparameter *gnuplot-stream* nil)
(defparameter *gnuplot_term* '$dumb)
(defparameter maxima::*maxima-infodir* "/usr/local/share/info/")
(defparameter *x-width* 2)
(defparameter *y-width* 2)
(maxima::initialize-runtime-globals)

(in-package :clim-user) 

(defparameter *boxify* nil)

(defparameter *listener-use-debug-io* nil)

(defclass listener-view (textual-view) ())

(defclass listener-pointer-documentation-view
  (listener-view pointer-documentation-view)
  ())

(defclass listener-interactor-pane (interactor-pane) ())

(defparameter +listener-view+ (make-instance 'listener-view))
(defparameter +listener-pointer-documentation-view+
  (make-instance 'listener-pointer-documentation-view))

(defclass showtime-pane (application-pane) ()
  (:default-initargs :background +gray15+ :foreground +wheat3+))

(defparameter *listener-initial-function* nil)
(defparameter *showtime* "")

(defun make-random-color ()
  (make-rgb-color 
    (/ (+ 50 (random (- 255 50))) 255)
    (/ (+ 50 (random (- 255 50))) 255)
    (/ (+ 50 (random (- 255 50))) 255)))

(defparameter my-color (make-random-color))

(defun maxima-prompt (stream frame)
  (declare (ignore frame))
  (with-drawing-options (stream :ink my-color)
    (with-text-style (stream (make-text-style :fix :italic :very-large))
      (princ (maxima::main-prompt) stream))))

(defun display-showtime (frame pane)
  (princ *showtime* pane))

(define-application-frame maxima-repl (standard-application-frame)
  ((system-command-reader :accessor system-command-reader
     :initarg :system-command-reader
     :initform t))
  (:panes
    (interactor :interactor :incremental-redisplay nil 
      :type 'listener-interactor-pane :default-view +listener-view+ 
      :scroll-bars :both :end-of-line-action :allow :end-of-page-action :scroll 
      :background +black+ :foreground (make-random-color)
      :width 1200 :height 800
      :margin 1200 :text-margin 1200)
    (doc :pointer-documentation :default-view +listener-pointer-documentation-view+ :height 100 :min-height 100)
    (showtime (make-pane 'showtime-pane :incremental-redisplay nil
		:min-height 2
		:height 2
		:display-function 'display-showtime :display-time :command-loop 
		:scroll-bars nil
		:end-of-line-action :wrap)))
  (:top-level (default-frame-top-level :prompt 'maxima-prompt))
  (:command-table (maxima-repl
		    :inherit-from (lisp-commands)
		    :menu (("Lisp" :menu lisp-commands))))
  (:layouts
    (default
      (vertically ()
	interactor
	doc
	showtime))))

(define-command-table lisp-commands :inherit-from nil)
(define-presentation-type maxima-expression () :inherit-from nil)

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
    
    (with-drawing-options (input-stream :text-size :very-large :ink (make-random-color))
      
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
	       (eof-count 0))
	  
	  ;; This is something of a hack. If we are running in a server mode
	  ;; (which we determine by checking *socket-connection*) and we get
	  ;; an eof on an input-stream that is not *standard-input*, switch
	  ;; the input stream to *standard-input*.
	  ;; There should probably be a better scheme for server mode.
	  ;; jfa 10/09/2002.
	  (cond ((and (consp r) (keywordp (car r)))
		  (maxima::break-call (car r) (cdr r) 'maxima::break-command))))
	
	(format t "~a" maxima::*general-display-prefix*)
	
	(if (eq r eof) (return maxima::$done))
	
	(setq maxima::$__ (caddr r))
	
	(unless maxima::$nolabels
	  (set  maxima::c-tag maxima::$__))

	(cond (batch-or-demo-flag
		(let ((maxima::$display2d nil))
		  (present `((maxima::mlabel ,maxima::c-tag ,maxima::$__) 'maxima-expression)))))
	
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
	
	(setq *showtime*
	  (with-output-to-string (s)
	    (format s "Evaluation took ~$ seconds (~$ elapsed)"
	      maxima::time-used maxima::etime-used )
	    #+(or cmu sbcl clisp)
	    (let ((total-bytes (- maxima::area-after maxima::area-before)))
	      (cond ((> total-bytes (* 1024 1024))
		      (format s " using ~,3F MB.~%"
			(/ total-bytes (* 1024.0 1024.0))))
		((> total-bytes 1024)
		  (format s " using ~,3F KB.~%" (/ total-bytes 1024.0)))
		(t
		  (format s " using ~:D bytes.~%" total-bytes))))))
	
	(unless maxima::$nolabels
	  (maxima::putprop maxima::d-tag (cons maxima::time-used  0) 'maxima::time))

        (when (not (maxima::checklabel maxima::$inchar))
          (setq maxima::$linenum (1+ maxima::$linenum)))
        (present `((maxima::mlabel) ,maxima::d-tag ,maxima::$%) 'maxima-expression)

	(when (eq batch-or-demo-flag ':demo)
	  (princ (maxima::break-prompt))
	  (force-output)
	  (let (quitting)
	    (do ((char)) (nil)
	      ;;those are common lisp characters you're reading here
	      
	      (case (setq char (read-char *terminal-io*))
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


(define-presentation-method present (object (type maxima-expression)
				      stream (view textual-view)
				      &key &allow-other-keys)
  (cond
    ((not maxima::*alt-display2d*)
      ;; This method has problems with scrolling at the bottom of the
      ;; screen.
      (let ((maxima::*y-width* (or 12 (text-style-height (medium-text-style stream)
					stream)))
	     (maxima::*x-width* (or 3725/256 (text-style-width (medium-text-style stream)
					       stream))))
        (let ((r
		(with-drawing-options (stream :text-size :huge :text-face :roman :ink (make-random-color))
		  (maxima::displa (caddr object)))))
          r)))
    (t
      ;; This method works and scrolling is good.
      (princ (untabify (with-output-to-string (s)
			 (cond
			   ((eq (caar object) 'maxima::displayinput)
			     (let ((maxima::$display2d nil))
			       (with-drawing-options (s :text-size :huge :text-face :roman :ink (make-random-color))
				 (maxima::displa `((maxima::mlabel) nil ,@(caddr object))))))
			   ((eq (caar object) :lisp)
			     (with-drawing-options (s :text-size :huge :text-face :roman :ink (make-random-color))
			       (maxima::displa `(,@(cadr object)))))
			   (t
			     (with-drawing-options (s :text-size :huge :text-face :roman :ink (make-random-color))
			       (maxima::displa `(,@(caddr object))))))))
	stream))))

(define-presentation-method accept ((type maxima-expression)
				     stream (view textual-view)
				     &key &allow-other-keys)
  (with-drawing-options (stream :text-size :very-large :ink (make-random-color))
    (maxima::dbm-read stream nil nil)))

(defmethod read-frame-command ((frame maxima-repl) &key stream)  
  "Specialized for the listener, read a lisp form to eval, or a command."
  (with-drawing-options (stream :text-size :very-large :ink (make-random-color))
    (let (object)
      (with-input-editing (stream :input-sensitizer
			    (lambda (stream cont)
			      (with-output-as-presentation (stream object 'maxima-expression)
				(funcall cont))))
	(setq object (accept 'maxima-expression :stream stream :prompt nil :activation-gestures nil))
	(list 'com-eval object)))))

(defun run-maxima (&key (system-command-reader nil)
		    (new-process t)
		    (width 1200)
		    (height 800)
		    (margin 1200)
		    (text-margin 1200)
		    (process-name "Maxima Listener")
		    (eval nil))
  (flet ((run ()
           (run-frame-top-level
	     (make-application-frame 'maxima-repl
	       :width width
	       :height height
	       :margin margin
	       :text-margin text-margin
	       :system-command-reader system-command-reader)
	     :listener-funcall nil)))
    (let ((*package* (find-package :maxima)))
      (if new-process
        (clim-sys:make-process #'run :name process-name)
        (run)))))

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
	    (throw 'return-to-listener *terminal-io*)))))))

(defun untabify (input)
  (let ((line-posn 0))
    (with-output-to-string (output)
      (dotimes (i (length input))
	(let ((char (char input i)))
	  (case char
	    (#\Tab
	      (let ((spaces (- 8 (rem line-posn 8))))
		(dotimes (k spaces)
		  (incf line-posn)
		  (write-char #\space output))))
	    (#\Newline
	      (setf line-posn 0)
	      (write-char #\Newline output))
	    (t
	      (incf line-posn)
	      (write-char char output))))))))
