;;; I wondered yesterday night how hard it could be to write a CLIM
;;; frontend for RT. For those of you wondering the same thing, the answer
;;; is "not very". After 3 hours of hacking, this code was done (thanks to
;;; the clim listener for examples (-:).

;;; Requires mcclim and the RT package from <URL:http://cliki.net/rt>.

;;; Load your tests and invoke the front-end with (rt-clim:rt)

;;; Bugs: few redisplay issues. I don't know if these are mcclim's or
;;; mine. Will have to ask tim moore (-:

;;; Get the Latest version from: http://boinkor.net/lisp/rt-clim.lisp

;;; Written by and Copyright 2004: Andreas Fuchs <asf@boinkor.net>
;;; License: MIT


;; to ease testing
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl-user::ql :rt)
  (cl-user::ql :mcclim)
  (load "/home/oleo2/quicklisp/dists/quicklisp/software/rt-20101006-git/rt-test.lisp"))

(cl:defpackage :rt-clim
  (:use :clim :clim-lisp :rt)
  (:import-from :rt #:*entries* #:name #:pend #:vals #:do-entry
		#:*optimization-settings* #:equalp-with-case #:*compile-tests*
		#:*catch-errors*)
  (:export #:rt))

(cl:in-package :rt-clim)

(define-command-table rt-cmds)

;;; XXX: this is silly. RT should have a function that does just that
(defun run-entry (entry)
  (let (r
	(aborted nil))
    (block aborted
      (setf r
	    (flet ((%do
		       ()
		     (if *compile-tests*
			 (multiple-value-list
			  (funcall (compile
				    nil
				    `(lambda ()
				       (declare
					(optimize ,@*optimization-settings*))
				       ,(rt::form entry)))))
			 (multiple-value-list
			  (eval (rt::form entry))))))
	      (if *catch-errors*
		  (handler-bind
		      ((style-warning #'muffle-warning)
		       (error #'(lambda (c)
				  (setf aborted t)
				  (setf r (list c))
				  (return-from aborted nil))))
		    (%do))
		  (%do)))))
    (values (equalp-with-case r (vals entry))
	    r
	    (vals entry))))

(defclass test ()
     ((entry :initarg :entry :reader entry)
      (result :initform nil :accessor result)
      (expected :initform nil :accessor expected)
      (success :initform nil :accessor success)))

;;; ptypes

(define-presentation-type test () :inherit-from 'expression)
(define-presentation-type run-test () :inherit-from 'test)
(define-presentation-type tests ())

;;; views

(defclass bar-view (view)
     ())
(defclass detail-view (textual-view)
     ())
(defconstant +bar-view+ (make-instance 'bar-view))
(defconstant +detail-view+ (make-instance 'detail-view))

;;; presentation methods

(define-presentation-method presentation-typep (object (type run-test))
  (and (typep object 'test)
       (not (null (success object)))))

(define-presentation-method present  (object (type tests)
				      stream (view textual-view)
				      &key &allow-other-keys)
  (loop for test in object
	do (present test 'test :stream stream)
	do (terpri stream)))

(define-presentation-method present (object (type tests)
					    stream (view bar-view)
					    &key &allow-other-keys)
  (let ((overall-result t))
    (block find-failures
      (dolist (test object)
	(cond
	  ((eql (success test) :failure)
	   (setf overall-result :failure)
	   (return-from find-failures))
	  ((and (eql overall-result t) (eql (success test) :success))
	   (setf overall-result :success)))))
    (case overall-result
      (:failure (setf (medium-background stream) +RED+))
      (:success (setf (medium-background stream) +green+))
      (t (setf (medium-background stream) +gray50+)))))

(define-presentation-method present (object (type test)
				     stream (view textual-view)
				     &key &allow-other-keys)
  (with-drawing-options (stream :ink (case (success object)
				       (:success +black+)
				       (:failure +red+)
				       (t +grey40+)))
    (present (name (entry object))
	     (presentation-type-of (name (entry object)))
	     :stream stream :view view)))

(define-presentation-method present (test (type run-test)
				     stream (view detail-view)
				     &key &allow-other-keys)
  (case (success test)
    (:success (format stream "~&Test succeeded: "))
    (:failure (format stream "~&Test failed: "))
    (t (format stream "~&Test not run yet: ")))
  (present test 'test :stream stream)
  (terpri stream)
  (format stream "Form: ")
  (present (rt::form (entry test)) 'expression :stream stream)
  (when (success test)
    (format stream "~%Expected value~P: " (length (expected test)))
    (present (expected test) (presentation-type-of (expected test)) :stream stream)
    (format stream "~%Actual value~P: " (length (result test)))
    (with-drawing-options (stream :ink (case (success test)
					 (:success +black+)
					 (:failure +red+)))
      (present (result test) (presentation-type-of (result test)) :stream stream)))
  (terpri))

;;; commands

(defun run-test (test)
  (multiple-value-bind (success result expected) (run-entry (entry test))
    (setf (success test) (if success :success :failure))
    (setf (result test) result)
    (setf (expected test) expected)))

(defun intern-test (entry)
  (let ((tests (test-hash *application-frame*)))
    (multiple-value-bind (val found) (gethash entry tests)
      (if found
	  val
	  (setf (gethash entry tests)
		(make-instance 'test
		   :entry entry))))))

(define-command (com-run-tests :name "Run All Tests"
			       :command-table rt-cmds
			       :menu t)
    ()
  (loop for test in (rest rt::*entries*)
	do (run-test (intern-test test)))
  (redisplay-frame-pane *application-frame* (get-frame-pane *application-frame* 'bar))
  (redisplay-frame-pane *application-frame* (get-frame-pane *application-frame* 'test-results)))

(define-command (com-run-test :name "Run Test"
			      :command-table rt-cmds
			      :menu t)
    ((test 'test :prompt "Test"))
  (run-test test)
  (redisplay-frame-pane *application-frame* (get-frame-pane *application-frame* 'bar)))

(define-command (com-display-test :name "Display Test"
				  :command-table rt-cmds
				  :menu t)
    ((test 'run-test :prompt "Test"))
  (setf (displaying-result *application-frame*) test))

(define-presentation-to-command-translator t-display-test
  (test com-display-test rt-cmds :gesture :select)
    (object)
  (list object))

(define-gesture-name :rerun-test :pointer-button-press
  (:middle))

(define-presentation-to-command-translator run-test
  (test com-run-test rt-cmds :gesture :rerun-test)
    (object)
  (list object))

(define-command (com-exit :name "Exit"
			  :command-table rt-cmds)
    ()
  (frame-exit *application-frame*))

;;; app frame

(define-application-frame rt (standard-application-frame)
    ((test-hash :accessor test-hash :initform (make-hash-table))
     (displaying-result :accessor displaying-result :initform nil))
  (:panes (start-button :push-button :label ">" :id 'start)
	  (exit-button :push-button :label "X" :id 'exit)
	  (bar          :application
			:display-function #'display-bar
			:default-view +bar-view+
			:scroll-bars nil)
	  (test-details :application
			:display-function #'show-test
			:incremental-redisplay t
			:scroll-bars t
			:end-of-page-action :scroll
			:default-view +detail-view+
			:initial-cursor-visibility nil)
	  (test-results :application
			:display-function #'display-tests
			:incremental-redisplay t
			:scroll-bars t
			:end-of-page-action :scroll
			:initial-cursor-visibility nil)
	  (status-line  :application
			:display-function #'display-test-status
			:scroll-bars nil)
	  (doc :pointer-documentation))
  (:command-table (rt :inherit-from (rt-cmds)))
  (:layouts (default
 	       (vertically ()
		(horizontally ()
		   +fill+
		   start-button
		   exit-button
		   +fill+))
		(20 bar)
		(+fill+
		 (horizontally ()
                    (1/5 test-results)
		    (4/5 test-details)))
		(20 status-line)
		doc)))

;;; app frame redisplay functions

(defun display-tests (frame stream)
  (declare (ignore frame))
  (present (mapcar #'intern-test (rest *entries*)) 'tests
	   :stream stream))

(defun show-test (frame stream)
  (declare (ignore frame))
  (when (displaying-result *application-frame*)
    (present (displaying-result *application-frame*) 'run-test
	     :stream stream))
  (force-output stream))

(defun display-bar (frame stream)
  (declare (ignore frame))
  (present (mapcar #'intern-test (rest *entries*)) 'tests
	   :stream stream))

(defun display-test-status (frame stream)
  (with-text-family (stream :serif)
    (format stream "~A tests. ~A/~A executed tests successful. This is ~A ~A."
	    (length (rest *entries*))
	    (count :success (rest *entries*) :key (lambda (entry) (success (intern-test entry))))
	    (count-if #'success (rest *entries*) :key #'intern-test)
	    (lisp-implementation-type) (lisp-implementation-version))))

;;; gadget functions

(defmethod activate-callback ((button push-button) (client rt) (gadget-id (eql 'start)))
  (com-run-tests)
  (redisplay-frame-panes client))

(defmethod activate-callback ((button push-button) (client rt) (gadget-id (eql 'exit)))
  (com-exit))

(defun rt ()
  (run-frame-top-level (make-application-frame 'rt) :new-process t))
