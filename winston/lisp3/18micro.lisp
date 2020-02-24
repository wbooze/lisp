;;;; -*- mode:Lisp; package:user -*- ;;;;
;;;; Created: 10 December 1992
;;;; Copyright 1992 Patrick H. Winston and Berthold K. P. Horn.
;;;; All rights reserved.
;;;;
;;;; Version 1.0.1, copied from master file on 23 Apr 93       
;;;; 
;;;; This software is licensed by Patrick H. Winston and Berthold K. P. Horn
;;;; (licensors) for instructional use with the textbooks ``Lisp,'' by Patrick
;;;; H. Winston and Berthold K. P. Horn, and ``Artificial Intelligence,'' by
;;;; Patrick H. Winston.  Your are free to make copies of this software and
;;;; modify it for such instructional use as long as:
;;;; 1. You keep this notice intact.
;;;; 2. You cause any modified files to carry a prominent notice stating
;;;;    that you modified the files and the date of your modifications.
;;;; This software is licensed ``AS IS'' without warranty and the licensor
;;;; shall have no liability for any alleged defect or damages.

;;;; REMARKS

#|

This version uses lexical variables unless you plac the symbol DYNAMIC on
the *FEATURES* list before reading the file.

|#

;;;; PROCEDURES

(defun micro-eval (form &optional environment)
  (if (atom form)
      (cond ((numberp form) form)
	    ((eq t form) t)
	    ((eq nil form) nil)
	    (t (let ((binding (assoc form environment)))
		 (if binding
		     (second binding)
		     (error "I could find no variable binding for ~a."
			    form)))))
      (case (first form)
	(m-quote (second form))
	(m-if (if (micro-eval (second form) environment)
		  (micro-eval (third form) environment)
		  (micro-eval (fourth form) environment)))
	(m-defun (setf (get (second form) 'm-lambda)
		       `(m-lambda ,(third form) ,(fourth form)))
		 (second form))
	(m-apply (micro-apply (micro-eval (second form) environment)
			      (micro-eval (third form) environment)
			      #+dynamic environment))
	(m-function `(m-closure ,(second (second form))
				,(third (second form))
				,environment))
	(m-setq (setf (second (assoc (second form) environment))
		      (micro-eval (third form) environment)))
	(t (micro-apply
	     (first form)                    
	     (mapcar #'(lambda (x) (micro-eval x environment))
		     (rest form))
	     #+dynamic environment)))))

(defun micro-apply (procedure argument-values #+dynamic environment)
  (if (symbolp procedure)
      (case procedure
	(m-first (first (first argument-values)))
	(m-rest (rest (first argument-values)))
	(m-cons (cons (first argument-values) (second argument-values)))
	(m-endp (endp (first argument-values)))
	(m-not (not (first argument-values)))
	(m-eq (eq (first argument-values) (second argument-values)))
	(t (let ((procedure-description (get procedure 'm-lambda)))
	     (if procedure-description
		 (micro-apply procedure-description argument-values)
		 (error "I could find no procedure description for ~a."
			procedure)))))
      (case (first procedure)
	(m-lambda (micro-eval (third procedure)
			      (bind-variables (second procedure)
					      argument-values
					      #+dynamic environment)))
	(m-closure (micro-eval (third procedure)
			       (bind-variables (second procedure)
					       argument-values
					       (fourth procedure)))))))

(defun bind-variables (variables values &optional a-list)
  (append (mapcar #'list variables values) a-list))

