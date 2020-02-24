;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; REMARKS

#|

This program is largely drawn from Lisp, Third Edition, but with
important changes.

o First, the syntax includes IF and THEN markers.  There can be
  any number of THENs.

o Next, the syntax includes an AND-IF section, which consists of
  predicate forms that have to evaluate to nonNIL, and an
  EVALUATING section, which is a list of forms to be evaluated
  if the rule actually manages to assert something.

(NAME IF <antecedant 1>
	 <antecedant n>
      AND-IF <predicate form 1>
	     <predicate form n>
      THEN <consequent 1>
	   <consequent n>
      EVALUATING <form 1>
		 <form n>)

o Alternatively, if ADD/DELETE syntax is used, only the first bindings
  that produce a new assertion are used; the others are ignored

(NAME IF <antecedant 1>
	 <antecedant n>
      AND-IF <predicate form 1>
	     <predicate form n>
      ADD <new assertion 1>
	  <new assertion n>
      DELETE <old assertion 1>
	     <old assertion n>
      EVALUATING <form 1>
		 <form n>)

o Finally, CHAIN starts over on rules as soon as one triggers and fires.

|#

;;;; SPECIAL VARIABLES

(defvar *assertions*)

(defvar *rules*)

;;;; FORWARD CHAINING

(defun chain ()
  "
  Purpose:	Initiate forward chaining.
  "
    (do ((rule-stream *rules*))
        ((stream-endp rule-stream))
      (if (use-rule (stream-first rule-stream))
	(setf rule-stream *rules*)
	(setf rule-stream (rest rule-stream))))
    (values))

(defun use-rule (rule &aux success-switch)
  "
  Purpose:	Controls forward chaining.
  Remarks:	Handles IF, THEN, ADD, DELETE, AND-IF, EVALUATING,
		and SAYING.  The SAYING marker arranges for printing
		helpful notes for the user.
  " 
  #+comment
  (format t "~%Trying rule ~a." (rule-name rule))
  (let ((binding-stream
	  (apply-filters (rule-ifs rule)
			 (stream-cons nil (make-empty-stream)))))
    (do ((binding-stream binding-stream (stream-rest binding-stream)))
        ((stream-endp binding-stream) success-switch)
      (let* ((bindings (stream-first binding-stream))
	     (deletes (instantiate-variables (rule-deletes rule) bindings))
	     (adds (instantiate-variables (rule-adds rule) bindings))
	     (thens (instantiate-variables (rule-thens rule) bindings))
	     (and-if (instantiate-variables (rule-and-if rule) bindings))
	     (saying (instantiate-variables (rule-saying rule) bindings))
	     (evaluating
	       (instantiate-variables (rule-evaluating rule) bindings)))
	(if thens
	    ;;Deduction option:
	    (dolist (a thens)
	      (when (remember-assertion a)
		(setf success-switch t)
		(unless saying
		  (format t "~%Rule ~a indicates ~a." (rule-name rule) a))))
	  ;;Add-delete option:
	  (when (every #'identity (mapcar #'eval and-if))
	    (dolist (a adds)
	      (when (remember-assertion a)
		#+comment
		(format t "~%Rule ~a indicates ~a." (rule-name rule) a)
		(setf success-switch t)))
	    (when success-switch
	      (dolist (d deletes) (delete-assertion d))
	      (dolist (e evaluating) (eval e))
	      (when saying
		(format t "~%Rule ~a says: " (rule-name rule))
		(dolist (s saying)
		  (apply #'format t s)))
	      (return t))))))))

(defun apply-filters (patterns initial-input-stream)
  "
  Purpose:	Tries to match all patterns to all assertions using
		all binding lists.
  "
  (if (endp patterns)
      initial-input-stream
      (apply-filters (rest patterns)
		     (filter-binding-stream (first patterns)
					    initial-input-stream))))

(defun filter-binding-stream (pattern stream)
  "
  Purpose:	Tries to match one pattern to all assertions using
		all binding lists.
  "
  (stream-concatenate
    (stream-map
      #'(lambda (bindings)
	  (match-pattern-to-assertions pattern bindings))
      stream)))

(defun match-pattern-to-assertions (pattern bindings)
  "
  Purpose:	Tries to match one pattern to all assertions using
		one binding list.
  "
  (stream-concatenate
    (stream-map
      #'(lambda (assertion) (try-assertion pattern assertion bindings))
      *assertions*)))

(defun try-assertion (pattern assertion bindings)
  "
  Purpose:	Tries to match one pattern to one assertion.
  "
  (let ((result (match pattern assertion bindings)))
    (if (eq 'fail result)
        (make-empty-stream)
        (stream-cons result (make-empty-stream)))))

(defun instantiate-variables (pattern a-list)
  "
  Purpose:	Replaces variables by their bindings.
  "
  (cond ((atom pattern) pattern)
	((eq '? (first pattern))
	 (if (eq '_ (second pattern))
	     pattern
	   (second (assoc (second pattern) a-list))))
	(t (cons (instantiate-variables (first pattern) a-list)
		 (instantiate-variables (rest pattern) a-list)))))

;;;; ASSERTION AND RULE ACCESS FUNCTIONS

(defun delete-assertion (pattern &aux success)
  "
  Purpose:	Deletes assertions.
  Remarks:	Handles delete commands with variables.
  "
  (dolist (assertion *assertions* success)
      (let ((result (match pattern assertion)))
	(unless (eq result 'fail)
	  (setf success t *assertions* (remove assertion *assertions*))))))

(defun count-assertions (pattern &aux (count 0))
  "
  Purpose:	Counts assertions that match the pattern.
  "
  (dolist (assertion *assertions* count)
      (let ((result (match pattern assertion)))
	(unless (eq result 'fail)
	  (incf count)))))

(defun remember-assertion (assertion)
  (stream-remember assertion *assertions*))

(defun remember-rule (rule)
  (stream-remember rule *rules*))

(defun clear-assertions () (setf *assertions* (make-empty-stream)))

(defun clear-rules () (setf *rules* (make-empty-stream)))

(defun display-assertions (&optional (stream *assertions*))
  (unless (stream-endp stream)
    (print (stream-first stream))
    (display-assertions (stream-rest stream))))

;;;; ACCESS FUNCTIONS FOR RULE ELEMENTS

(defun rule-name (rule) (first rule))

(defun rule-ifs (rule) (extract-from-rule 'if rule))

(defun rule-thens (rule) (extract-from-rule 'then rule))

(defun rule-then (rule) (first (rule-thens rule)))

(defun rule-adds (rule) (extract-from-rule 'add rule))

(defun rule-deletes (rule) (extract-from-rule 'delete rule))

(defun rule-and-if (rule) (extract-from-rule 'and-if rule))

(defun rule-evaluating (rule) (extract-from-rule 'evaluating rule))

(defun rule-saying (rule) (extract-from-rule 'saying rule))

(defun extract-from-rule (marker rule)
  (up-to-atom (rest (member marker rule))))

(defun up-to-atom (rule)
  (cond ((atom (first rule)) nil)
	(t (cons (first rule) (up-to-atom (rest rule))))))



