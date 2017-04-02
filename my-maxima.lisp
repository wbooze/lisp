(in-package :clim-user)
;; CLIM needs to be loaded too, of course.

(defparameter *listener-use-debug-io* t)

(defclass showtime-pane (application-pane) ()
  (:default-initargs :background +gray20+ :foreground +wheat4+))

(define-application-frame maxima-repl (standard-application-frame)
  ((system-command-reader :accessor system-command-reader
			    :initarg :system-command-reader
			    :initform t))
  (:panes
   (interactor :interactor :scroll-bars t :background +gray20+ :foreground +wheat4+)
   (doc :pointer-documentation)
   (showtime (make-pane 'showtime-pane
			:min-height 18
			:display-function 'display-showtime
			:scroll-bars t
			:display-time :command-loop
			:end-of-line-action :allow)))
  (:top-level (default-frame-top-level :prompt 'maxima-prompt))
  (:command-table (maxima-repl
                   :inherit-from (lisp-commands)
		   :menu (("Lisp"        :menu lisp-commands))
		   ))
  (:layouts
   (default
       (vertically ()
	 interactor
	 doc
	 showtime))))


(defun maxima-prompt (stream frame)
  (declare (ignore frame))
  (with-text-style (stream (make-text-style :fix :italic :large))
    (princ (maxima::main-prompt) stream)))

(defparameter *listener-initial-function* nil)

(defvar *showtime* "")

(defun display-showtime (frame pane)
  (princ *showtime* pane))

(defmethod run-frame-top-level ((frame maxima-repl) &key listener-funcall &allow-other-keys)
  (let ((*debug-io* (if *listener-use-debug-io*
                        (get-frame-pane frame 'interactor)
			*debug-io*))
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
  
  (with-text-style (stream (make-text-style :fix :bold :large))
  (let (object)
    (with-input-editing (stream :input-sensitizer
				(lambda (stream cont)
				  (with-output-as-presentation 
				      (stream object 'maxima-expression)
				    (funcall cont))))
      (setq object (accept 'maxima-expression :stream stream :prompt nil
			   :activation-gestures nil))
      (list 'com-eval object)))))


(defclass redisplay-frame-mixin ()
  ())

(defmethod redisplay-frame-pane :after
    ((frame redisplay-frame-mixin) (pane application-pane) &key force-p)
  (declare (ignore force-p))
  (change-space-requirements
   pane :height (bounding-rectangle-height (stream-output-history pane))))

(defun run-listener (&key (system-command-reader nil)
                          (new-process t)
                          (width 760)
                          (height 550)
                          (process-name "Listener")
                          (eval nil))
  (flet ((run ()
           (run-frame-top-level
            (make-application-frame 'maxima-repl
                                    :width width
                                    :height height
                                    :system-command-reader system-command-reader)
            :listener-funcall (cond ((null eval) nil)
				    ((functionp eval) eval)
                                    (t (lambda () (eval eval)))))))
    (let ((*package* (find-package "MAXIMA")))
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
  (with-text-style (stream (make-text-style :fix :bold :large))
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
  (with-text-style (stream (make-text-style :fix :bold :large))
  (maxima::dbm-read stream nil stream)))

(define-command (com-eval :menu t :command-table lisp-commands)
    ((form 'clim:form :prompt "form"))
  (let ((r form)
	(time-before)
	(time-after)
	(time-used)
	(eof (list nil))
	(etime-before)
	(etime-after)
	(area-before)
	(area-after)
	(etime-used)
	(maxima::c-tag)
	(maxima::d-tag))

    (setq maxima::c-tag (maxima::makelabel maxima::$inchar))
    (setq maxima::myf r)
    (if (eq (car r) :lisp)
    (setq maxima::$__ (cadr r))
    (setq maxima::$__ (caddr r)))
    (set  maxima::c-tag maxima::$__)
    (setq time-before (get-internal-run-time)
	  etime-before (get-internal-real-time))
    (setq area-before (maxima::used-area))

    (setq maxima::$% (maxima::toplevel-macsyma-eval maxima::$__))

    (setq etime-after (get-internal-real-time)
	  time-after (get-internal-run-time))
    (setq area-after (maxima::used-area))
    (setq time-used (maxima::div 
		     (float (maxima::sub time-after time-before))
		     internal-time-units-per-second)
	  etime-used (maxima::div 
		      (float (maxima::sub etime-after etime-before))
		      internal-time-units-per-second))
    (setq maxima::accumulated-time (maxima::add maxima::accumulated-time time-used))
    (set (setq maxima::d-tag (maxima::makelabel maxima::$outchar)) maxima::$%)
    (setq maxima::$_ maxima::$__)
    (setq *showtime*
	  (with-output-to-string (s)
	    (format s "Evaluation took ~$ seconds (~$ elapsed)"
		    time-used etime-used )
	    #+(or cmu sbcl clisp)
	    (let ((total-bytes (- area-after area-before)))
	      (cond ((> total-bytes (* 1024 1024))
		     (format s " using ~,3F MB.~%"
			     (/ total-bytes (* 1024.0 1024.0))))
		    ((> total-bytes 1024)
		     (format s " using ~,3F KB.~%" (/ total-bytes 1024.0)))
		    (t
		     (format s " using ~:D bytes.~%" total-bytes))))))
    (unless maxima::$nolabels
      (maxima::putprop maxima::d-tag (cons time-used  0) 'maxima::time))

    (when (not (maxima::checklabel maxima::$inchar))
      (setq maxima::$linenum (1+ maxima::$linenum)))

    ;; I don't know why I need to do this, but with my showtime pane
    ;; enabled, *standard-output* is set to the showtime-pane so the
    ;; output here goes to the wrong place.  This sets
    ;; *standard-output* to the interactor pane that we want.
    (let ((*standard-output* (get-frame-pane *application-frame* 'interactor)))
      (with-drawing-options (t :ink +olivedrab+)
	(present `((maxima::mlabel) nil ,(if *boxify*
					     (maxima::boxify maxima::$%)
					     maxima::$%))
		 'maxima-expression)))))





