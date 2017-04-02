(defun load-maxima ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (cl-user::change-directory "/home/oleo/maxima-code/src/")
    (load "maxima-build.lisp")
    (maxima-load)
    (cl-user::change-directory "/home/oleo/")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (not (find-package :maxima))
    (load-maxima)))

(eval-when (:compile-toplevel :load-toplevel)
  (require :maxima))

(in-package :maxima)
;; Replace some of maxima's display routines with own.

(defparameter writefilep nil)
(defparameter *maxima-tempdir* "/home/oleo/temp/")
(defparameter *x-width* 0)
(defparameter *y-width* 0)
(defparameter *gnuplot-stream* nil)
(defparameter *gnuplot_term* '$dumb)
(defparameter maxima::*maxima-infodir* "/usr/local/share/info/")

(defun mbreak-loop ()
  (let ((*standard-input* *debug-io*)
        (*standard-output* *debug-io*))
    (catch 'break-exit
      (format t (intl:gettext "~%Entering a Maxima break point. Type 'exit;' to resume."))
      (do ((r)) (nil)
        (fresh-line)
        (setq r (caddr (let ((*mread-prompt* (break-prompt)))
			 (climi::with-drawing-options (*standard-output* :ink climi::+white+)
                         (mread *standard-input*)))))
        (case r
          (($exit) (throw 'break-exit t))
          (t (errset (displa (meval r)) t)))))))

(defun default-format-prompt (destination control-string arguments)
  "Like AFORMAT, but add the prefix and suffix configured for a prompt. This
function deals correctly with the ~M control character, but only when
DESTINATION is an actual stream (rather than nil for a string)."
  (let ((*print-circle* nil))
    (if (null destination)
      ;; return value string is important
      (concatenate 'string
	*prompt-prefix*
	(apply #'aformat destination
	  control-string
	  arguments)
	*prompt-suffix*)
      (progn
        (format destination "~A~A~A"
	  *prompt-prefix*
	  (apply #'aformat nil
	    control-string
	    arguments)
	  *prompt-suffix*)))))

(defun macsyma-top-level (&optional (input-stream *standard-input*) batch-flag)
  (let ((*package* (find-package :maxima)))
    (if *maxima-started*
        (format t (intl:gettext "Maxima restarted.~%"))
        (progn
          (if (not *maxima-quiet*) (maxima-banner))
          (setq *maxima-started* t)))

    (if ($file_search *maxima-initlisp*) ($load ($file_search *maxima-initlisp*)))
    (if ($file_search *maxima-initmac*) ($batchload ($file_search *maxima-initmac*)))

    (catch 'quit-to-lisp
      (in-package :maxima)
      (loop
         do
         (catch #+kcl si::*quit-tag*
                #+(or cmu scl sbcl openmcl lispworks) 'continue
                #-(or kcl cmu scl sbcl openmcl lispworks) nil
                (catch 'macsyma-quit
                  (continue input-stream batch-flag)
                  (format t *maxima-epilog*)
                  (bye)))))))

(defun continue (&optional (input-stream *standard-input*)
                 batch-or-demo-flag)
  (declare (special *socket-connection*))
  (if (eql batch-or-demo-flag :demo)
      (format t
        (intl:gettext
          "~%At the '~A' prompt, type ';' and <enter> to get next demonstration.~&")
        (print-invert-case (stripdollar $prompt))))
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
        (when (or (not (checklabel $inchar))
                  (not (checklabel $outchar)))
          (incf $linenum))
        (setq c-tag (makelabel $inchar))
        (let ((*mread-prompt* (if batch-or-demo-flag nil (main-prompt)))
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
                   (if batch-or-demo-flag
                       (return '$done)
                       (progn
                         (setq *mread-prompt* nil)
                         (setq r (dbm-read input-stream nil eof))))))

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
        (if (eq r eof) (return '$done))
        (setq $__ (caddr r))
        (unless $nolabels (set  c-tag $__))
        (cond (batch-or-demo-flag
          (let (($display2d nil))
            (displa `((mlabel) ,c-tag , $__)))))
        (setq time-before (get-internal-run-time)
              etime-before (get-internal-real-time))
        (setq area-before (used-area))
        (setq $% (toplevel-macsyma-eval $__))
        (setq etime-after (get-internal-real-time)
              time-after (get-internal-run-time))
        (setq area-after (used-area))
        (setq time-used (quotient
                         (float (- time-after time-before))
                         internal-time-units-per-second)
              etime-used (quotient
                          (float (- etime-after etime-before))
                          internal-time-units-per-second))
        (incf accumulated-time time-used)
        (setq d-tag (makelabel $outchar))
        (unless $nolabels (set d-tag $%))
        (setq $_ $__)
        (when $showtime ;; we don't distinguish showtime:all?? /RJF
	  (format t (intl:gettext "Evaluation took ~,4F seconds (~,4F elapsed)")
                  time-used etime-used )
          #+(or gcl ecl openmcl)
          (format t "~%")
          #+(or cmu scl sbcl clisp)
          (let ((total-bytes (- area-after area-before)))
            (cond ((> total-bytes (* 1024 1024))
                   (format t (intl:gettext " using ~,3F MB.~%")
                           (/ total-bytes (* 1024.0 1024.0))))
                  ((> total-bytes 1024)
                   (format t (intl:gettext " using ~,3F KB.~%") (/ total-bytes 1024.0)))
                  (t
                   (format t (intl:gettext " using ~:D bytes.~%") total-bytes))))
          #+allegro
          (let ((conses (- (car area-after) (car area-before)))
                (other (- (cadr area-after) (cadr area-before)))
                (gctime (- (caddr area-after) (caddr area-before))))
            (if (= 0 gctime) nil (format t (intl:gettext " including GC time ~s s,") (* 0.001 gctime)))
            (format t (intl:gettext " using ~s cons-cells and ~s other bytes.~%") conses other)))
        (unless $nolabels
          (putprop '$% (cons time-used 0) 'time)
          (putprop d-tag (cons time-used  0) 'time))
        (if (eq (caar r) 'displayinput)
            (displa `((mlabel) ,d-tag ,$%))) ;; consistently misspelling label.
	(when (eq batch-or-demo-flag ':demo)
          (princ (break-prompt))
          (force-output)
          (let (quitting)
            (do ((char)) (nil)
              ;;those are common lisp characters you're reading here
	      (case (setq char (read-char *terminal-io*))
                ((#\page)
                 (fresh-line)
                 (princ (break-prompt))
                 (force-output))
                ((#\?)
                 (format t
                   (intl:gettext
                     "  Pausing. Type a ';' and <enter> to continue demo.~%")))
                ((#\space #\; #\n #\e #\x #\t))
                ((#\newline )
                 (if quitting (throw 'abort-demo nil) (return nil)))
                (t (setq quitting t))))))
        ;; This is sort of a kludge -- eat newlines and blanks so that
	;; they don't echo
	(and batch-or-demo-flag
             (do ((char)) (())
               (setq char (read-char input-stream nil nil))
               (when (null char)
                 (throw 'macsyma-quit nil))
               (unless (member char '(#\space #\newline #\return #\tab) :test #'equal)
                 (unread-char char input-stream)
                 (return nil))))))))

(defun dbm-read (&optional (stream *standard-input*) (eof-error-p t)
		  (eof-value nil) repeat-if-newline  &aux tem  ch
		  (mprompt *mread-prompt*) (*mread-prompt* ""))
  (if (and *need-prompt* (> (length mprompt) 0))
    (progn
      (fresh-line *standard-output*)
      (princ mprompt *standard-output*)
      (force-output *standard-output*)
      (setf *prompt-on-read-hang* nil))
    (progn
      (setf *prompt-on-read-hang* t)
      (setf *read-hang-prompt* mprompt)))
  
  ;; Read a character to see what we should do.
  (tagbody
    top
    (setq ch (read-char stream eof-error-p eof-value))
    (cond ((or (eql ch #\newline) (eql ch #\return))
            (if (and repeat-if-newline *last-dbm-command*)
	      (return-from dbm-read *last-dbm-command*))
            (go top))
      ((eq ch eof-value)
	(return-from dbm-read eof-value)))
    ;; Put that character back, so we can reread the line correctly.
    (unread-char ch stream))
  
  ;; Figure out what to do
  (cond ((eql #\: ch)
	  ;; This is a Maxima debugger command (I think)
	  (let* ((line (read-line stream eof-error-p eof-value))
		  fun)
	    (multiple-value-bind
	      (keyword n)
	      (read-from-string line)
	      (setq fun (complete-prop keyword 'keyword 'break-command))
	      (and (consp fun) (setq fun (car fun)))
	      ;;(print (list 'line line))
	      (setq *last-dbm-command*
		(cond ((null fun) '(:_none))
		  ((get fun 'maxima-read)
		    (cons keyword (mapcar 'macsyma-read-string
				    (split-string line " " n))))
		  (t (setq tem
		       ($sconcat "(" (string-right-trim  ";" line)
			 ")"))
		    ;;(print (list 'tem tem))                                                                         
		    (read  (make-string-input-stream tem)
		      eof-error-p eof-value)))))))
    ((eql #\? ch)
      ;; Process "?" lines.  This is either a call to describe or a                                                        
      ;; quick temporary escape to Lisp to call some Lisp function.                                                        
      
      ;; First, read and discard the #\? since we don't need it anymore.                                                   
      (read-char stream)
      (let ((next (peek-char nil stream nil)))
	(cond ((member next '(#\space #\tab #\!))
		;; Got "? <stuff>" or "?! <stuff>".                                                                         
		;; Invoke exact search on <stuff>.                                                                          
		(let* ((line (string-trim
			       '(#\space #\tab #\; #\$)
			       (subseq
                                 (read-line stream eof-error-p eof-value) 1))))
		  `((displayinput) nil (($describe) ,line $exact))))
	  ((equal next #\?)
	    ;; Got "?? <stuff>". Invoke inexact search on <stuff>.
	    (let* ((line (string-trim
			   '(#\space #\tab #\; #\$)
			   (subseq
			     (read-line stream eof-error-p eof-value) 1))))
	      `((displayinput) nil (($describe) ,line $inexact))))
	  (t
	    ;; Got "?<stuff>" This means a call to a Lisp
	    ;; function.  Pass this on to mread which can handle
	    ;; this.
	    ;;
	    ;; Note: There appears to be a bug in Allegro 6.2
	    ;; where concatenated streams don't wait for input
	    ;; on *standard-input*.
	    (mread (make-concatenated-stream
		     (make-string-input-stream "?") stream)
	      eof-value)))))
    (t
      (setq *last-dbm-command* nil)
      (let ((result (mread stream eof-value))
	     (next-char (read-char-no-hang stream eof-error-p eof-value)))
	(cond
	  ((or (eql next-char nil) (equal next-char '(nil)))
	    (setf *need-prompt* t))
	  ((member next-char '(#\newline #\return))
	    (setf *need-prompt* t))
	  (t
	    (setf *need-prompt* nil)
	    (unread-char next-char stream)))
	result))))

(defun break-dbm-loop (at)
  (let* ((*quit-tags* (cons (cons *break-level* *quit-tag*) *quit-tags*))
	  (*break-level* (if (not at) *break-level* (cons t *break-level*)))
	  (*quit-tag* (cons nil nil))
	  (*break-env* *break-env*)
	  (*mread-prompt* "")
	  (*diff-bindlist* nil)
	  (*diff-mspeclist* nil)
	  val)
    (declare (special *mread-prompt*))
    (and (consp at) (set-env at))
    (cond ((null at)
	    (break-frame 0 nil)))
    (catch 'step-continue
      (catch *quit-tag*
        (unwind-protect
	  (do () (())
	    (format-prompt *debug-io* "~a"
	      (format nil "~&~@[(~a:~a) ~]"
		(unless (stringp at) "dbm")
		(length *quit-tags*)))
	    (finish-output *debug-io*)
	    (setq val
	      (catch 'macsyma-quit
		(let ((res (dbm-read *debug-io*  nil *top-eof* t)))
		  (declare (special *mread-prompt*))
		  (cond ((and (consp res) (keywordp (car res)))
			  (let ((value (break-call (car res)
					 (cdr res)
					 'break-command)))
			    (cond ((eq value :resume) (return)))))
		    ((eq res *top-eof*)
		      (funcall (get :top 'break-command)))
		    (t
		      (setq $__ (nth 2 res))
		      (setq $% (meval* $__))
		      (setq $_ $__)
		      (displa $%)))
		  nil)))
	    (and (eql val 'top)
	      (throw-macsyma-top)))
          (restore-bindings))))))

(in-package :clim-user)

(defclass listener-view (textual-view) ())

(defclass listener-pointer-documentation-view
  (listener-view pointer-documentation-view)
  ())

(defclass listener-interactor-pane (application-pane) ())

(defparameter +listener-pointer-documentation-view+
  (make-instance 'listener-pointer-documentation-view))

(defun make-random-color ()
  (make-rgb-color 
    (/ (+ 40 (random (- 255 40))) 255)
    (/ (+ 40 (random (- 255 40))) 255)
    (/ (+ 40 (random (- 255 40))) 255)))

(defparameter my-color (make-random-color))

(defun maxima-prompt (stream frame)
  (let ((frame *application-frame*))
    (with-text-style (stream (make-text-style :fix :italic :huge))
      (princ (maxima::main-prompt) stream))))

(defun display-maxima (frame stream)
  (let ((frame *application-frame*))
    (with-drawing-options (stream :text-size :huge)
      (maxima::initialize-runtime-globals)
      (princ (maxima::continue stream nil) stream))))

(define-presentation-type maxima-expression () :inherit-from nil)

(define-application-frame maxima-repl (standard-application-frame)
  ()
  (:panes
    (interactor :interactor :incremental-redisplay nil 
      :type 'listener-interactor-pane
      :scroll-bars :both
      :background +black+ :foreground +goldenrod+  
      :width 1200 :height 800
      :margin 1000 :text-margin 1000)
    (doc :pointer-documentation :default-view +listener-pointer-documentation-view+ :height 100 :min-height 100))
  (:top-level (default-frame-top-level) :prompt 'maxima-prompt)
  (:layouts
    (default
      (vertically ()
	interactor
	doc))))

(define-presentation-method accept ((type maxima-expression)
				     stream (view textual-view)
				     &key &allow-other-keys)
  (let ((*terminal-io* (get-frame-pane *application-frame* 'interactor)))
    (with-drawing-options (t :ink (make-random-color))
      (maxima::continue stream nil))))

(defmethod read-frame-command ((frame maxima-repl) &key (stream (get-frame-pane *application-frame* 'interactor)))  
  "Specialized for the listener, read a lisp form to eval, or a command."
  (with-drawing-options (stream :text-size :huge)
    (let (object)
      (with-input-editing (stream :input-sensitizer
			    (lambda (stream cont)
			      (with-output-as-presentation (stream object 'maxima-expression)
				(funcall cont))))
        (setq object (accept 'maxima-expression :stream stream :prompt nil 
		       :activation-gestures nil))
        object))))

(defun run-maxima (&key (new-process t)
		    (width 1200)
		    (height 800)
		    (margin 1000)
		    (text-margin 1000)
		    (process-name "Maxima Listener"))
  (flet ((run ()
           (run-frame-top-level
	     (make-application-frame 'maxima-repl
	       :width width
	       :height height
	       :margin margin
	       :text-margin text-margin))))
    (let ((*package* (find-package :maxima)))
      (if new-process
        (clim-sys:make-process #'run :name process-name)
        (run)))))
