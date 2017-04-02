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

(in-package :CLIM-USER)
;; CLIM needs to be loaded too, of course.

;; Maxima needs to be loaded before this file can work!
(require :maxima)

(defparameter *listener-use-debug-io* nil)

(defclass showtime-pane (application-pane) ()
  (:default-initargs :background +gray90+))

(define-application-frame maxima-repl (standard-application-frame)
  ((system-command-reader :accessor system-command-reader
			    :initarg :system-command-reader
			    :initform t))
  (:panes
   (interactor :interactor :scroll-bars t)
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
  (with-text-face (stream :italic)
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


(define-command-table lisp-commands :inherit-from nil)

(define-command (com-eval :menu t :command-table lisp-commands)
    ((form 'clim:form :prompt "form"))
  (clim-listener::display-evalues
   (list (let ((result (with-output-to-string (s)
			 (let ((*standard-output* s))
			   (with-input-from-string (input form)
			     (let ((*standard-input* input))
			       (maxima::continue input)))))))
	   (format *trace-output* "~A~%" result)
	   result))))

(define-command (com-eval :menu t :command-table lisp-commands)
    ((form 'clim:form :prompt "form"))
  (clim-listener::display-evalues
   (list (let ((result (with-output-to-string (s)
			 (let* ((*standard-output* s)
				(eof (list nil))
				(d-tag (maxima::makelabel maxima::$outchar))
				(r
				 (with-input-from-string (input form)
				   (let ((*standard-input* input))
				     (maxima::dbm-read input nil eof)))))
			   (when (eq (caar r) 'maxima::displayinput)
			     (maxima::displa `((maxima::mlabel) nil
					       ,(maxima::toplevel-macsyma-eval (caddr r)))))))))
	   (format *trace-output* "~S" result)
	   (untabify result)))))

(eval-when (compile load eval)
  (define-presentation-type maxima-expression () :inherit-from t)
  )


(define-presentation-method present (object (type maxima-output-expression)
					    stream (view textual-view)
					    &key &allow-other-keys)
  (princ (untabify (with-output-to-string (s)
		     (let ((*standard-output* s)
			   (maxima::$display2d t))
		       (maxima::displa object))))
	 stream))

(define-presentation-method present (object (type maxima-input-expression)
					    stream (view textual-view)
					    &key &allow-other-keys)
  (princ (untabify (with-output-to-string (s)
		     (let ((*standard-output* s)
			   (maxima::$display2d nil))
		       (maxima::displa (caddr object)))))
	 stream))

;;(define-presentation-method present (object (type maxima-expression)
;;					    stream (view textual-view)
;;					    &key &allow-other-keys)
;;  (princ (untabify (with-output-to-string (s)
;;		     (let ((*standard-output* s))
;;		       (if (eq (caar object) 'maxima::displayinput)
;;			   (let ((maxima::$display2d nil))
;;			     (maxima::displa `((maxima::mlabel) nil ,@(cddr object))))
;;			   (maxima::displa object)))))
;;	 stream))

(define-presentation-method present (object (type maxima-expression)
					    stream (view textual-view)
					    &key &allow-other-keys)
  ;; First convert the maxima expression into dimension list.  Run
  ;; down the list and present each form.
  (labels
      ((draw-2d (dmstr row col))
       (do ((l dmstr))
	   ((null l))
	 (cond ((integerp (car l))
		(tyo* (car l))
		(pop l))
	       ((integerp (caar l))
		(setq col oldcol)
		(do ()
		    ((or (integerp (car l))
			 (not (integerp (caar l)))))
		  (cond
		    ((null (cddar l))
		     (setq col (+ col (caar l))))
		    (t
		     (draw-2d (reverse (cddar l))
			      (- row (cadar l))
			      (+ col (caar l)))
		     (setq col oldcol)))
		  (cursorpos* row col))
		(t
		 (apply (caar l) nil (cdar l))
		 (pop l)))))))
  (multiple-value-bind (oldrow oldcol)
      (stream-cursor-position stream)
    (let ((dimstr (maxima::convert-expr-to-dimensions object)))
      (draw-2d dimstr (- oldrow bkptdp 1) 0))))

(defun massage (object)
  (cond ((atom object)
	 (intern (maxima::print-invert-case (maxima::stripdollar object))))
	((eq (caar object) 'maxima::mplus)
	 `(+ ,@(mapcar #'massage (cdr object))))
	((eq (caar object) 'maxima::mtimes)
	 `(* ,@(mapcar #'massage (cdr object))))
	((eq (caar object) 'maxima::rat)
	 `(/ ,@(mapcar #'massage (cdr object))))
	((eq (caar object) 'maxima::mexpt)
	 `(expt ,@(mapcar #'massage (cdr object))))
	(t
	 `(,(massage (caar object)) ,@(mapcar #'massage (cdr object))))))

(define-presentation-method present (object (type maxima-expression)
					    stream (view textual-view)
					    &key &allow-other-keys)
  (format *trace-output* "stream = ~A~%" stream)
  (format *trace-output* "view   = ~A~%" view)
  (let ((form (massage (caddr object))))
    (format *trace-output* "obj    = ~A~%" (caddr object))
    (format *trace-output* "form   = ~A~%" form)
    (clim-user::display-formula stream form))
  )

(define-presentation-method accept ((type maxima-expression)
				    stream (view textual-view)
				    &key)
  (maxima::dbm-read stream nil stream))

(define-command (com-eval :menu t :command-table lisp-commands)
    ((form 'clim:form :prompt "form"))
  (clim-listener::display-evalues
   (list (let ((result (with-output-to-string (s)
			 (let* ((*standard-output* s))
			   (with-input-from-string (input form)
			     (let ((*standard-input* input))
			       (maxima::climaxima-repl input)))))))
	   (format *trace-output* "~S" result)
	   (untabify result)))))


(define-command (com-eval :menu t :command-table lisp-commands)
    ((form 'clim:form :prompt "form"))
  (let ((result (with-input-from-string (input form)
		  (maxima::climaxima-repl input))))
    ;;(format *trace-output* "result = ~A~%" result)
    (with-drawing-options (t :ink +olivedrab+)
      (present result 'maxima-expression))))

(define-command (com-eval :menu t :command-table lisp-commands)
    ((form 'clim:form :prompt "form"))
  (with-drawing-options (t :ink +olivedrab+)
    (present (caddr form) 'maxima-expression)))

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
    (setq maxima::$__ (caddr r))
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
	(present `((maxima::mlabel) nil ,maxima::$%) 'maxima-expression)))))

(defun com-eval (form)
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
    
    (setq maxima::$__ (caddr r))
    (set  maxima::c-tag maxima::$__)
    (setq time-before (get-internal-run-time)
	  etime-before (get-internal-real-time))
    (setq area-before (maxima::used-area))

    (setq maxima::$% (maxima::toplevel-macsyma-eval maxima::$__))

    (setq etime-after (get-internal-real-time)
	  time-after (get-internal-run-time))
    (setq area-after (maxima::used-area))
    (setq time-used (/ (float (- time-after time-before))
		       internal-time-units-per-second)
	  etime-used (/ (float (- etime-after etime-before))
			internal-time-units-per-second))
    (setq maxima::accumulated-time (+ maxima::accumulated-time time-used))
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
    (with-drawing-options (t :ink +olivedrab+)
      (present `((maxima::mlabel) ,maxima::d-tag ,maxima::$%) 'maxima-expression))))




(defmethod read-frame-command ((frame maxima-repl) &key (stream *standard-input*))  
  "Specialized for the listener, read a lisp form to eval, or a command."
  (let (object)
    (with-input-editing (stream :input-sensitizer
				(lambda (stream cont)
				  (with-output-as-presentation 
				      (stream object 'maxima-expression)
				    (funcall cont))))
      (setq object (accept 'maxima-expression :stream stream :prompt nil
			   :activation-gestures nil))
      (list 'com-eval object))))


(defmethod read-frame-command :around ((frame maxima-repl)
				       &key (stream *standard-input*))
  "Read a command or form, taking care to manage the input context
   and whatever else need be done."
  (multiple-value-bind (x y)
      (stream-cursor-position stream)    
    (with-input-context ('command) (object object-type)
            (call-next-method)
        (command
         ;; Kludge the cursor position - Goatee will have moved it all around
         (setf (stream-cursor-position stream) (values x y))
         (present object object-type
                  :view (stream-default-view stream)
                  :stream stream)
         object))))

(defun run-listener (&key (system-command-reader nil)
                          (new-process nil)
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


(in-package "MAXIMA")

(defun clim-display (form &rest args)
  (let ((displayp t)
	(linearray (if displayp (make-array 80.) linearray))
	(mratp (checkrat form))
	(#.writefilep #.writefilep)
	(maxht     1) (maxdp   0) (width   0)
	(height    0) (depth   0) (level   0) (size   2)
	(break     0) (right   0) (lines   1) bkpt
	(bkptwd    0) (bkptht  1) (bkptdp  0) (bkptout 0)
	(bkptlevel 0) (in-p)
	(more-^w))
    (unwind-protect
	 (let ((form (dimension form
				nil 'mparen 'mparen 0 0)))
	   (checkbreak form width)
	   (output form (if (and (not $leftjust) (= 2 lines))
			    (f- linel (f- width bkptout))
			    0)))
      ;; make sure the linearray gets cleared out.
      (fill linearray nil))))

(defun convert-expr-to-dimensions (form)
  (let ((displayp t)
	(linearray (if displayp (make-array 80.) linearray))
	(mratp (checkrat form))
	(#.writefilep #.writefilep)
	(maxht     1) (maxdp   0) (width   0)
	(height    0) (depth   0) (level   0) (size   2)
	(break     0) (right   0) (lines   1) bkpt
	(bkptwd    0) (bkptht  1) (bkptdp  0) (bkptout 0)
	(bkptlevel 0) (in-p)
	(more-^w))
    (unwind-protect
	 (let ((form (dimension form
				nil 'mparen 'mparen 0 0)))
	   (checkbreak form width)
	   form))))

(setf maxima::*alt-display2d* #'clim-display)
#||
;; Basically a copy of maxima's CONTINUE function, with the stuff we
;; don't need ripped out.
(defun climaxima-repl (input-stream &optional batch-or-demo-flag)
  (if (eql batch-or-demo-flag :demo)
      (format t "~% At the _ prompt, type ';' followed by enter to get next demo~&"))
  (catch 'abort-demo
    (do ((r)
	 (time-before)
	 (time-after)
	 (time-used)
	 (eof (list nil))
	 (etime-before)
	 (etime-after)
	 (area-before)
	 (area-after)
	 (etime-used)
	 (c-tag)
	 (d-tag))
	(nil)
      (catch 'return-from-debugger
	(when (not (checklabel $inchar))
	  (setq $linenum (f1+ $linenum)))
	(setq c-tag (makelabel $inchar))
	(let* ((*prompt-prefix* "")
	       (*mread-prompt* nil (if batch-or-demo-flag nil (main-prompt)))
	       (eof-count 0))
	  (tagbody
	   top
	     (setq r (dbm-read input-stream nil eof))
	     ;; This is something of a hack. If we are running in a server mode
	     ;; (which we determine by checking *socket-connection*) and we get
	     ;; an eof on an input-stream that is not *standard-input*, switch
	     ;; the input stream to *standard-input*.
	     ;; There should probably be a better scheme for server mode.
	     ;; jfa 10/09/2002.
	     (if (and
		  (eq r eof)
		  (not (eq input-stream *standard-input*))
		  (boundp '*socket-connection*))
		 (progn
		   (setq input-stream *standard-input*)
		   (setq *mread-prompt* nil)
		   (setq r (dbm-read input-stream nil eof))))

	     (cond ((and (eq r eof) (boundp '*socket-connection*)
			 (eq input-stream *socket-connection*))
		    (cond ((>=  (setq eof-count (+ 1 eof-count)) 10)
			   (print "exiting on eof")
			   ($quit))
			  (t (go top)))))
	     (cond ((and (consp r) (keywordp (car r)))
		    (break-call (car r) (cdr r) 'break-command)
		    (go top)))))
    	(format t "~a" *general-display-prefix*)
	(cond (#.writefilep  ;write out the c line to the dribble file
	       (let ( (#.ttyoff t) smart-tty  $linedisp)
		 (displa `((mlabel) , c-tag , $__)))))
	(if (eq r eof) (return (values $% '$done)))
	(setq $__ (caddr r))
	(set  c-tag $__)
	(cond (batch-or-demo-flag
	       (displa `((mlabel) ,c-tag , $__))))
	(setq time-before (get-internal-run-time)
	      etime-before (get-internal-real-time))
	(setq area-before (used-area))
	(setq $% (toplevel-macsyma-eval $__))
	;;(format *trace-output* "~A~%" $%)
	(setq etime-after (get-internal-real-time)
	      time-after (get-internal-run-time))
	(setq area-after (used-area))
	(setq time-used (maxima::div 
			 (float (maxima::sub time-after time-before))
			 internal-time-units-per-second)
	      etime-used (maxima::div 
			  (float (maxima::sub etime-after etime-before))
			  internal-time-units-per-second))
	(setq accumulated-time (maxima::add accumulated-time time-used))
	(set (setq d-tag (makelabel $outchar)) $%)
	(setq $_ $__)
	(when $showtime
	  (format t "Evaluation took ~$ seconds (~$ elapsed)"
		  time-used etime-used )
	  #+(or cmu sbcl clisp)
	  (let ((total-bytes (- area-after area-before)))
	    (cond ((> total-bytes (* 1024 1024))
		   (format t " using ~,3F MB.~%"
			   (/ total-bytes (* 1024.0 1024.0))))
		  ((> total-bytes 1024)
		   (format t " using ~,3F KB.~%" (/ total-bytes 1024.0)))
		  (t
		   (format t " using ~:D bytes.~%" total-bytes)))))
	(unless $nolabels
	  (putprop d-tag (cons time-used  0) 'time))
	;;(format *trace-output* "~A~%" $%)
	;;(format *trace-output* "d-tag = ~A~%" d-tag)
	(if (eq (caar r) 'displayinput)
	    (displa `((mlabel) nil ,$%)))
	(when (eq batch-or-demo-flag ':demo)
	  (mtell "~A_~A" *prompt-prefix* *prompt-suffix*)
	  (let (quitting)	  
	    (do ((char)) (nil)
	      ;;those are common lisp characters you're reading here
	      (case
		  (setq char (read-char *terminal-io*))
		((#\page) (unless (cursorpos 'c input-stream) 
			    (terpri *standard-output*))
		 (princ "_" *standard-output*))
		((#\?) (mtell "  Pausing.  Type a ';' and Enter to continue demo.~%_"))
		((#\space #\; #\n #\e #\x #\t))
		((#\newline )
		 (if quitting (throw 'abort-demo nil) (return nil))) 
		(t (setq quitting t)
		   )))))
	;; This is sort of a kludge -- eat newlines and blanks so that
	;; they don't echo
	(and batch-or-demo-flag
	     (do ((char)) (())
	       (setq char (read-char input-stream nil #+cl nil))
	       (when (null char) 
		 (throw 'macsyma-quit nil)) 
	       (unless (zl-member char '(#\space #\newline #\return #\tab))
		 (unread-char char input-stream)  
		 (return nil))))
	;;(format *trace-output* "climaxima returns ~A~%" $%)
	$%))))
||#
