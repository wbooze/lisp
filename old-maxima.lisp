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

(in-package :maxima)
;; Replace some of maxima's display routines with own.


(defparameter writefilep nil)
(defparameter *maxima-tempdir* "/home/oleo2/temp/")
(defparameter *x-width* 0)
(defparameter *y-width* 0)
(defparameter *gnuplot-stream* nil)
(defparameter *gnuplot_term* '$dumb)
(defparameter maxima::*maxima-infodir* "/usr/local/share/info/")
(maxima::initialize-runtime-globals)
 
(defun clim-display (form &rest args)
  (let ((displayp t)
	  (linearray (if displayp (make-array 80.) linearray))
	  (mratp (maxima::checkrat form))
	  (maxht     1) (maxdp   0) (width   0)
	  (height    0) (depth   0) (level   0) (size   2)
	  (break     0) (right   0) (lines   1) bkpt
	  (bkptwd    0) (bkptht  1) (bkptdp  0) (bkptout 0)
	  (bkptlevel 0) (in-p)
	  (more-^w))
    (unwind-protect
      (let ((form (dimension form
		        nil 'mparen 'mparen 0 0)))
	(maxima::checkbreak form width)
	(output form (if (and (not $leftjust) (= 2 lines))
		              (f- linel (f- width bkptout))
		              0)))
      ;; make sure the linearray gets cleared out.
      (fill linearray nil))))

(defun boxify (form)
  "Takes a maxima internal form and adds boxes everywhere, as if dpart
  was done on all possible places"
  (labels ((boxify-internal (f)
	     (cond ((atom f)
		     `((mbox simp) ,f))
	       ((eq (caar f) 'rat)
		 ;; We have ((rat) n m).  Convert to ((mtimes) n ((mexpt) m -1)).
		 (destructuring-bind (r n m)
		   f
		   (declare (ignore r))
		   `((mtimes simp) ,(if (= n -1) -1
				      (boxify-internal n))
		      ((mexpt simp) ,(boxify-internal m) -1))))
	       ((and (eq (caar f) 'mplus)
		  (eql (second f) -1))
		 `(,(car f) -1 ,@(mapcar #'boxify-internal (cddr f))))
	       ((eq (caar f) 'mtimes)
		 (destructuring-bind (r a &rest b)
		   f
		   (cond ((eql a -1)
			   ;; Need to handle multiplication by -1 specially.
			   `(,r ,a ,@(mapcar #'boxify-internal b)))
		     ((and (listp a)
			(eq (caar a) 'rat)
			(or (eql (second a) 1)
			  (eql (second a) -1)))
		       ;; Handle multiplication by 1/n or -1/n specially
		       `(,r ,a ,@(mapcar #'boxify-internal b)))
		     (t
		       `(,r ,@(mapcar #'boxify-internal (cdr f)))))))
	       ((and (eq (caar f) 'mexpt)
		  (= (length f) 3)
		  (or (alike1 (third f) 1//2)
		    (alike1 (third f) -1//2)))
		 (cond ((alike1 (third f) 1//2)
			 ;; Handle sqrt specially
			 `((mbox simp) (,(car f) ,(boxify-internal (second f)) ,(third f))))
		   ((alike1 (third f) -1//2)
		     `((mexpt simp) ((mbox simp) (,(car f)
						   ,(boxify-internal (second f)) 
						   ((rat simp) 1 2)))
			-1))))
	       (t
		 `((mbox simp) (,(car f) ,@(mapcar #'boxify-internal (cdr f))))))))
    (if (listp form)
      `(,(car form) ,@(mapcar #'boxify-internal (cdr form)))
      ;;(boxify-internal form)
      form)))

(in-package :clim-user)

(defparameter *boxify* nil)

(defun cursorpos ()
  (multiple-value-bind (x y)
    (clim:stream-cursor-position *standard-output*)
    ;;(format *trace-output* " cursorpos = ~A ~A~%" x y)
    (list (round y *y-width*) (round x *x-width*))))

(defun cursorpos* (row col)
  ;;(format *trace-output* " set row,col = ~A ~A~%" row col)
  (setf (clim::stream-cursor-position *standard-output*)
    (values (* col *x-width*) (* row *y-width*)))
  (setq oldrow row
    oldcol col))

(defun clim-display2d (form &optional (w 0))
  (let ((displayp t)
	 (linearray (if displayp (make-array 80.) linearray))
	 (mratp (maxima::checkrat form))
	 (maxht     1) (maxdp   0) (width   0)
	 (height    0) (depth   0) (level   0) (size   2)
	 (break     0) (right   0) (lines   1) bkpt
	 (bkptwd    0) (bkptht  1) (bkptdp  0) (bkptout 0)
	 (bkptlevel 0) (in-p)
	 (more-^w))
    (unwind-protect
      (progn (setq form (dimension form
			  nil 'mparen 'mparen 0 0))
	(maxima::checkbreak form width)
	(clim-output form (if (and (not $leftjust) (= 2 lines))
			    (f- linel (f- width bkptout))
			    0))
	)
      ;; make sure the linearray gets cleared out.
      (fill linearray nil))))

(defun convert-expr-to-dimensions (form)
  (let ((displayp t)
	 (linearray (if displayp (make-array 80.) linearray))
	 (mratp (maxima::checkrat form))
	 (maxht     1) (maxdp   0) (width   0)
	 (height    0) (depth   0) (level   0) (size   2)
	 (break     0) (right   0) (lines   1) bkpt
	 (bkptwd    0) (bkptht  1) (bkptdp  0) (bkptout 0)
	 (bkptlevel 0) (in-p)
	 (more-^w))
    (unwind-protect
      (let ((form (dimension form
		    nil 'mparen 'mparen 0 0)))
	(maxima::checkbreak form width)
	form))))

;; Set this to #'clim-display2d to use 2D stream I/O instead of just
;; outputing a string.  Otherwise, set it to nil
(setf *alt-display2d* #'clim-display2d)

(defun clim-output (result w)
  (declare (fixnum w))
  ;;(format *trace-output* "stdout = ~A~%" *standard-output*)
  (clim-output-2d (nreverse result) w))

(defun clim-output-2d (result w &aux (h 0))
  (declare (fixnum w h ))
  (setq oldrow (car (cursorpos))
    oldcol 0
    h (+ oldrow bkptht bkptdp))
  (cursorpos* oldrow 0)
  ;; Move the cursor vertically until we are at the bottom line of the
  ;; new expression.
  (do ()
    ((= h oldrow))
    (tyo* #\newline)
    (incf oldrow))
  (cursorpos* oldrow 0)
  (draw-2d result (f- oldrow bkptdp 1) w)
  (cursorpos* (setq h (min (f- ttyheight 2) h)) 0))

(defun draw-2d (dmstr row col)
  (declare (fixnum row col))
  ;;(format *trace-output* "draw-2d at ~A ~A~%" row col)
  (cursorpos* row col)
  (do ((l dmstr))
    ((null l))
    ;;(format *trace-output* "l = ~S~%" l)
    (cond ((characterp (car l))
	    (clim-tyo* (car l))
	    (pop l))
      ((and (listp (car l))
	 (integerp (caar l)))
	(setq col oldcol)
	(do ()
	  ((or (characterp (car l))
	     (not (and (listp (car l))
		    (integerp (caar l))))))
	  (cond
	    ((null (cddar l))
	      (setq col (+ col (caar l))))
	    (t (draw-2d (reverse (cddar l))
		 (-  row (cadar l)) (+ col (caar l)))
	      (setq col oldcol)))
	  (pop l))
	(cursorpos* row col))
      (t
	;;(format *trace-output* " T: ~S~%" l)
	(apply (caar l) nil (cdar l))
	(pop l)))))

(defun clim-tyo* (char)
  (cond ((char= #\backspace char)
	  (decf oldcol))			;Backspace
    ((char< char #.(code-char 128))
      (incf oldcol)))		;Printing graphic
  ;;(format *trace-output* "  tyo* ~S at ~A~%" char oldcol)
  (princ char))

(defun d-box (linear? h d w body &aux (char 0) dmstr)
					;char a char?
  (declare (fixnum h d w ))
  (cond		     
    ((and (not linear?) line-graphics-tty $linedisp)
      (let ((x-min (f* lg-character-x oldcol))
	     (x-max (f* lg-character-x (f+ oldcol w 2)))
	     (y-min (f+ (f* lg-character-y (f- oldrow h)) 2))
	     (y-max (f- (f* lg-character-y (f+ oldrow d 2)) 2)))
	(declare (fixnum x-min x-max y-min y-max))
	(lg-set-point x-min y-min)
	(lg-draw-vector x-max y-min)
	(lg-draw-vector x-max y-max)
	(lg-draw-vector x-min y-max)
	(lg-end-vector  x-min y-min))
      (cursorpos* oldrow (f1+ oldcol))
      (draw-2d body oldrow oldcol)
      (cursorpos* oldrow (f+ oldcol 1)))
    ((and (not linear?) character-graphics-tty $linedisp)
      (d-matrix nil 'left (f1+ h) (f1+ d))
      (cursorpos* (f- oldrow h) oldcol)
      (d-hbar nil w)
      (cursorpos* (f+ oldrow h) (f- oldcol w))
      (draw-2d body oldrow oldcol)
      (cursorpos* (f+ oldrow d 1) (f- oldcol w))
      (d-hbar nil w)
      (cursorpos* (f- oldrow d 1) oldcol)
      (d-matrix nil 'right (f1+ h) (f1+ d)))
    (t (setq char #\- (getcharn $boxchar 2))
      (setq dmstr
	`((0 ,h (d-hbar ,(f+ 2 w) ,char))
	   (,(f- (f+ w 2)) 0)
	   (d-vbar ,h ,d #\|)
	   ,@body
	   (,(f- (f1+ w)) ,(f- (f1+ d)) (d-hbar ,(f+ w 2) ,char))
	   (-1 0)
	   (d-vbar ,h ,d #\|)))
      (if linear?
	(draw-linear dmstr oldrow oldcol)
	(draw-2d dmstr oldrow oldcol)))))

(defparameter *listener-use-debug-io* nil)

(defclass listener-view (textual-view) ())

(defclass listener-pointer-documentation-view
  (listener-view pointer-documentation-view)
  ())

(defclass listener-interactor-pane (interactor-pane) ())

(defparameter +listener-view+ (make-instance 'listener-view))
(defparameter +listener-pointer-documentation-view+
  (make-instance 'listener-pointer-documentation-view))

#+nil (define-presentation-method present :around
  ((object sequence) (type sequence) stream (view listener-view)
    &key acceptably for-context-type)
  (with-drawing-options (stream :text-size 16)
    (present object 'expression :stream stream :view view
      :acceptably acceptably :for-context-type for-context-type)))

#+nil (define-presentation-method accept :around
  ((type sequence) stream (view listener-view) &key default default-type)
  (declare (ignorable default default-type))
  ;; oh, my word.  although TYPE here might look like it's bound to 
  ;; the presentation type itself, in fact it is bound to the 
  ;; parameter of the SEQUENCE presentation type.  We need the 
  ;; presentation type itself, so we reconstruct it.
  (let ((ptype (list 'sequence type)))
    (let* ((token (read-token stream))
	    (result (handler-case (read-from-string token)
		      (error (c)
			(declare (ignore c))
			(simple-parse-error
			  "Error parsing ~S for presentation type ~S"
			  token ptype)))))
      (if (presentation-typep result ptype)
	(values result ptype)
	(input-not-of-required-type result ptype)))))

#+nil (defmethod stream-present :around
  ((stream listener-interactor-pane) object type
    &rest args &key (single-box sbp) &allow-other-keys)
  (declare (ignore single-box sbp))
  ;;; here seems the place to increase the text-size of the stream font 
  (with-drawing-options (stream :text-size 16)
    ;; (apply #'call-next-method stream object type :single-box t args) 
    ;; we would do this, but CLIM:PRESENT calls STREAM-PRESENT with all 
    ;; the keyword arguments explicitly.  *sigh*.
    (if sbp (call-next-method)
      (apply #'call-next-method stream object type :single-box t args))))

(defclass showtime-pane (application-pane) ()
  (:default-initargs :background +gray15+ :foreground +wheat3+))

(defmethod compose-space ((pane showtime-pane) &key width height)
  (declare (ignore width height))
  (let ((h (* 1.5 (text-style-height (medium-text-style pane) pane)))) ; magic padding
    (make-space-requirement :height h
      :min-height h
      :max-height h)))

(defmethod allocate-space :after ((pane showtime-pane) width height)
  (repaint-sheet pane (sheet-region pane)))

(defmethod handle-repaint ((pane showtime-pane) region)
  (declare (ignore region))
  (with-output-recording-options (pane :draw t :record nil)
    (with-bounding-rectangle* (x0 y0 x1 y1) (sheet-region pane)
      (draw-rectangle* pane x0 y0 x1 y1 :filled t :ink (pane-background pane))
      (climi::draw-bordered-rectangle* (sheet-medium pane)
	x0 y0 x1 y1
	:style :mickey-mouse-inset))
    (replay-output-record (stream-output-history pane) pane)))

(defmethod window-clear ((pane showtime-pane))
  (call-next-method)
  (handle-repaint pane (sheet-region pane)))

(defun invoke-and-center-output (stream-pane continuation
				  &key (horizontally t) (vertically t) (hpad 0) (vpad 0))

  (let ((record (with-output-to-output-record (stream-pane)
                  (funcall continuation))))
    (with-bounding-rectangle* (sx0 sy0 sx1 sy1) (sheet-region stream-pane)
      (with-bounding-rectangle* (rx0 ry0 rx1 ry1) (bounding-rectangle record)
        (setf (output-record-position record)
	  (values (if horizontally
		    (+ rx0 (/ (- (- sx1 sx0)
				(- rx1 rx0))
			     2))
		    (+ rx0 hpad))
	    (if vertically
	      (+ ry0 (/ (- (- sy1 sy0)
			  (- ry1 ry0))
		       2))
	      (+ ry0 vpad))))))
    (add-output-record record (stream-output-history stream-pane))
    (repaint-sheet stream-pane record)))

(defun display-showtime (frame pane)
  (invoke-and-center-output pane
    (lambda () (princ *showtime* pane))
    :horizontally nil :hpad 5))

(defun make-random-color ()
  (make-rgb-color 
    (/ (+ 40 (random (- 255 40))) 255)
    (/ (+ 40 (random (- 255 40))) 255)
    (/ (+ 40 (random (- 255 40))) 255)))

(defparameter my-color (make-random-color))

(defun maxima-prompt (stream frame)
  (declare (ignore frame))
  (with-output-as-presentation (stream (maxima::main-prompt) 'string :single-box t)
    (with-drawing-options (stream :ink my-color)
      (with-text-style (stream (make-text-style :fix :italic :huge))
	(princ (maxima::main-prompt) stream)))))

(define-application-frame maxima-repl (standard-application-frame)
  ((system-command-reader :accessor system-command-reader
     :initarg :system-command-reader
     :initform t))
  (:panes
    (interactor :interactor 
      :type 'listener-interactor-pane :default-view +listener-view+ :incremental-redisplay nil 
      :scroll-bars :both :end-of-line-action :allow :end-of-page-action :allow
      :background +black+ :foreground (make-random-color)
      :width 1200 :height 800
      :margin 1000 :text-margin 1000)
    (doc :pointer-documentation :default-view +listener-pointer-documentation-view+ :height 100 :min-height 100)
    (showtime (make-pane 'showtime-pane :incremental-redisplay nil
		:min-height 2
		:height 2
		:display-function 'display-showtime
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
	doc))))

(defparameter *listener-initial-function* nil)

(defparameter *showtime* "")

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
	(restart-case (with-drawing-options (*standard-output* :ink (make-random-color)) (call-next-method))
	  (return-to-listener ()
	    :report "Return to listener."
	    (throw 'return-to-listener *terminal-io*)))))))

(define-command-table lisp-commands :inherit-from nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-presentation-type maxima-expression () :inherit-from nil))

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
		(with-drawing-options (stream :text-size :huge :text-face :bold :ink (make-random-color))
		  (maxima::displa (caddr object)))))
          r)))
    (t
      ;; This method works and scrolling is good.
      (princ (untabify (with-output-to-string (s)
			 (cond
			   ((eq (caar object) 'maxima::displayinput)
			     (let ((maxima::$display2d nil))
			       (with-drawing-options (s :text-size :huge :text-face :bold :ink (make-random-color))
				 (maxima::displa `((maxima::mlabel) nil ,@(caddr object))))))
			   ((eq (caar object) :lisp)
			     (with-drawing-options (s :text-size :huge :text-face :bold :ink (make-random-color))
				 (maxima::displa `(cadr object))))
			   (t
			   (with-drawing-options (s :text-size :huge :text-face :bold :ink (make-random-color))
			     (maxima::displa `(caddr object)))))))
	stream))))


#+nil (defclass redisplay-frame-mixin ()
  ())

#+nil (defmethod redisplay-frame-pane :after
  ((frame redisplay-frame-mixin) (pane application-pane) &key (force-p nil))
  (declare (ignore force-p))
  (change-space-requirements
    pane :height (bounding-rectangle-height (stream-output-history pane))))

(define-presentation-method accept ((type maxima-expression)
				     stream (view textual-view)
				     &key &allow-other-keys)
  (with-drawing-options (stream :ink (make-random-color) :text-size :huge :ink (make-random-color))
    (maxima::dbm-read stream nil nil)))

(defmethod read-frame-command ((frame maxima-repl) &key (stream (get-frame-pane *application-frame* 'interactor)))  
  "Specialized for the listener, read a lisp form to eval, or a command."
  (with-drawing-options (stream :text-size :huge :ink (make-random-color))
  (let (object)
  (with-input-editing (stream :input-sensitizer
			(lambda (stream cont)
			  (with-output-as-presentation (stream object 'maxima-expression)
			    (funcall cont))))
    (setq object (accept 'maxima-expression :stream stream :prompt nil :activation-gestures nil))
    (list 'com-eval object)))))

(define-command (com-eval :menu t :command-table lisp-commands)
  ((form 'clim:form :prompt "form") &key (batch-or-demo-flag nil))
  (declare (special *socket-connection*))
  (let ((r form)
	 (input-stream *standard-input*)
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
    
    (let* ((input-stream (get-frame-pane *application-frame* 'interactor)) (*terminal-io* input-stream))
      (with-drawing-options (input-stream :text-size :huge :ink (make-random-color))

	(catch 'return-from-debugger
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
		      (with-translation (input-stream 200 0)
		      (maxima::break-call (car r) (cdr r) 'maxima::break-command)))))
	  
	  (if (eq r eof) (return maxima::$done))
	  
	  (format t "~a" maxima::*general-display-prefix*)
	  
	  (setq maxima::$__ (caddr r))
	  
	  (setq maxima::c-tag (maxima::makelabel maxima::$inchar))
	  
	  (unless maxima::$nolabels
	    (set  maxima::c-tag maxima::$__))
	  
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
	  
	  (with-translation (t 200 0)
	  (present `((maxima::mlabel) ,maxima::d-tag ,maxima::$%) 'maxima-expression))
	  
	  (when (eq batch-or-demo-flag ':demo)
	    (princ (maxima::break-prompt))
	    (force-output)
	    (let (quitting)
	      (do ((char)) (nil)
		;;those are common lisp characters you're reading here                                                          
		(case (setq char (read-char *terminal-io*))
		  ((#\page)
		    (fresh-line)
		    (with-translation (*terminal-io* 200 0)
		    (princ (maxima::break-prompt)))
		    (force-output))
		  ((#\?)
		    (format t
		      (intl:gettext    
			"  Pausing. Type a ';' and <enter> to continue demo.~%")))
		  ((#\space #\; #\n #\e #\x #\t))
		  ((#\newline )
		    (if quitting (throw 'maxima::abort-demo nil) (return nil)))
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
		(return nil)))))))))

(defun run-maxima (&key (system-command-reader nil)
		    (new-process t)
		    (width 1200)
		    (height 800)
		    (margin 1000)
		    (text-margin 1000)
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

