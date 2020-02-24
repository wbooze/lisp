;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;
;;;; Created: 13 June 1990
;;;; Purpose: Support for genetic algorithms discussion in AI3

#|
The chaining portion of this program is largely drawn from Lisp,
Third Edition, with a serious bug fixed in the STREAM-APPEND, and with
the syntax for rules now including markers like this:

(NAME IF <antecedant 1>
	 <antecedant n>
      THEN <consequent 1>)

|#

;;; Load missing Common Lisp functions:

#-:common-lisp
(require 'cl "/phw/smlh/cl")

;;; Get auxiliary modules:

(require 'matching "/phw/ai3/matching")
(require 'streams "/phw/ai3/streams")
(require 'test "/phw/ai3/test")

;;;; Borrowed from forward chainer:

(defun apply-filters (patterns initial-input-stream)
  (if (endp patterns)
      initial-input-stream
      (apply-filters (rest patterns)
		     (filter-binding-stream (first patterns)
					    initial-input-stream))))

;;;; Basic Procedures for Prolog

(defun filter-binding-stream (pattern stream)
  (stream-concatenate
    (stream-map
      #'(lambda (bindings)
	  (stream-concatenate
	    (stream-cons
	      (match-pattern-to-assertions pattern bindings)
	      (stream-cons (match-pattern-to-rules pattern bindings)
			   (make-empty-stream)))))
      stream)))

(defun match-pattern-to-rules (pattern bindings)
  (stream-concatenate
    (stream-map
      #'(lambda (rule) (try-rule pattern rule bindings))
      *rules*)))

(defun match-pattern-to-assertions (pattern bindings)
  (stream-concatenate
    (stream-map
      #'(lambda (assertion) (try-assertion pattern assertion bindings))
      *assertions*)))

(defun try-rule (pattern rule bindings)
  (let* ((rule (make-variables-unique rule))
	 (result (unify pattern (rule-then rule) bindings)))
    (if (eq 'fail result)
	(format *standard-output* "~%~a rejects consequent ~a."
		pattern (rule-then rule))
      (format *standard-output*
	      "~%~a matches consequent ~a;~%	given ~a;~%	yielding ~a"
	      pattern (rule-then rule) bindings result))
    (if (eq 'fail result)
	(make-empty-stream)
	(apply-filters
		 (rule-ifs rule)
		 (stream-cons result (make-empty-stream))))))

(defun try-assertion (pattern assertion bindings)
  (let ((result (match pattern assertion bindings)))
    (if (eq 'fail result)
	(format *standard-output*
		"~%~a rejects assertion ~a." pattern assertion)
      (format *standard-output*
	      "~%~a matches assertion ~a;~%	given ~a;~%	yielding ~a"
	      pattern assertion bindings result))
    (if (eq 'fail result)
        (make-empty-stream)
        (stream-cons result (make-empty-stream)))))

(defun backward-chain (&rest patterns)
  (let ((binding-stream (apply-filters patterns
				       (stream-cons nil (make-empty-stream))))
	(variables (list-variables patterns))
	(displayed-answers nil))
    (if (endp variables)
	(if (stream-endp binding-stream)
	    'no
	  'yes)
      (do ((binding-stream binding-stream (stream-rest binding-stream)))
	  ((stream-endp binding-stream) 'no-more)
	(let ((answer (make-answer variables (stream-first binding-stream))))
	  (unless (member answer displayed-answers :test #'equal)
	    (display-answer answer)
	    (push answer displayed-answers)
	    (unless (char= #\, (read-char))
	      (return 'no-more))))))))

(defun list-variables (tree &optional names)
  (cond ((atom tree) names)
	((eq '? (first tree))
	 (if (member (second tree) names)
	     names
	     (append names (rest tree))))
	(t (list-variables (rest tree)
			   (list-variables (first tree)
					   names)))))

(defun list-variables (tree &optional names)
  (cond ((atom tree) names)
	((eq '? (first tree))
	 (if (or (eq '_ (second tree)) (member (second tree) names))
	     names
	     (append names (rest tree))))
	(t (list-variables (rest tree)
			   (list-variables (first tree)
					   names)))))

(defun make-answer (variables bindings)
  (instantiate-variables 
    (mapcar #'(lambda (variable) (list variable (list '? variable)))
	    variables)
    bindings))

(defun instantiate-variables (tree a-list)
  (cond ((atom tree) tree)
	((eq '? (first tree))
	 (let ((binding (assoc (second tree) a-list)))
	   (if binding
	       (instantiate-variables (second binding) a-list)
	       tree)))
	(t (cons (instantiate-variables (first tree) a-list)
		 (instantiate-variables (rest tree) a-list)))))

(defun display-answer (answers)
  (format *standard-output* "~&-->")
  (dolist (answer answers)
    (format *standard-output* "	~a = ~a" (first answer) (second answer))))

(defun make-variables-unique (rule)
  (let ((variables (list-variables rule)))
    (dolist (variable (reverse variables) rule)
      (setf rule (instantiate-variables
		   rule
		   (list (list variable (list '? (gensym variable)))))))))

;;; Create some data:

#|

Data for testing:

()

(progn
		
  (setf *rules* (make-empty-stream) *assertions* (make-empty-stream))
  (remember-rule '(IDENTIFY1
		    IF	((? x) is-a horse)
			((? x) is-a-parent-of (? y))
			((? y) is fast)
		    THEN	((? x) is valuable)))
  (remember-assertion '(comet is-a horse))
  (remember-assertion '(prancer is-a horse))
  (remember-assertion '(comet is-a-parent-of dasher))
  (remember-assertion '(comet is-a-parent-of prancer))
  (remember-assertion '(prancer is fast))
  (remember-assertion '(dasher is-a-parent-of thunder))
  (remember-assertion '(thunder is fast))
  (remember-assertion '(thunder is-a horse))
  (remember-assertion '(dasher is-a horse)))

(examine *assertions*)
(examine *rules*)

(stream-first (stream-rest *rules*))

(setf e '(backward-chain '((? x) is valuable)))

(defexperiment "backward.000"
	       (let ((*standard-output* *output*))
		 (print (backward-chain '((? z) is valuable)))))

(defexperiment "backward.001"
	       (let ((*standard-output* *output*)
		     (*rules* *rules*)
		     (*assertions* *assertions*))
		 (remember-rule '(IDENTIFY2
				   IF	((? w) is-a winner)
				   THEN	((? w) is fast)))
		 (remember-assertion '(dasher is-a winner))
		 (remember-assertion '(prancer is-a winner))
		 (print (backward-chain '((? z) is valuable)))))

(experiment)

|#
