;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; VARIABLES

(defvar *clause-counter*)
(defvar *name-counter*)

;;;; RESOLUTION

(defun prove (&rest clauses &aux (level 0))
  "
  Purpose:	Prove theorems.
  Arguments:	A list of quoted clauses, including the negated theorem.
  Returns:	T if theorem is provable.
  Caveats:	Loops forever if theorem is not provable.
  Remarks:	Presumes everything has already been converted to clause form.
  "
  ;;Initialize the variable-nameing number:
  (setf *name-counter* 0)
  ;;Initialize the clause-numbering variable:
  (setf *clause-counter* (apply #'max (mapcar #'first clauses)))
  ;;Make all initial-clause variable unique:
  (setf clauses (mapcar #'make-variables-unique clauses))
  (format t "~%The initial clauses are:")
  (mapcar #'print-clause clauses)
  (catch 'exit
    (loop
      (format t "~%There are ~a clauses on level ~a."
	      (length clauses) (incf level))
      ;;Try to resolve all clause pairs, thus implementing the breadth-first
      ;;resolution strategy:
      (do* ((set1 clauses (rest set1))
	    (c1 (first set1) (first set1)))
	  ((endp set1))
	(do* ((set2 (rest set1) (rest set2))
	      (c2 (first set2) (first set2)))
	     ((endp set2))
	  #+comment
	  (format t "~%I am working on clause ~a and clause ~a."
		  (first c1) (first c2))
	  (let ((resolvents (resolve-clauses (remove-decorations c1)
					     (remove-decorations c2))))
	    (if (member nil resolvents)
		;;If a NIL resolvent is produced, the theorem is proved:
		(progn
		  (format t "~%I resolved ~a with ~a producing nil!"
			  (first c1) (first c2))
		  (format t "~%I proved the theorem!")
		  (format t "~%There were ~a clauses on ~a ~
			       levels at termination."
			  (length clauses)
			  level)
		  (throw 'exit t))
	      ;;Otherwise, add any new resolvents to the list of clauses:
	      (dolist (resolvent resolvents)
		(let ((result (insert-decorations resolvent)))
		  (format t "~%I resolved ~a with ~a producing"
			  (first c1) (first c2))
		  (print-clause result)
		  (push result clauses))))))))))

(defun resolve-literals (l1 l2)
  "
  Purpose:	Resolves two literals, if possible, using unification.
  Caveats:	Remember that UNIFY returns FAIL if the patterns do
		no unify, rather than NIL.  The NIL result means they
		unify, with an empty variable-binding list.
  "
  (let ((result (unify (list 'not l1) l2)))
    (if (eq 'fail result)
	(unify (list 'not l2) l1)
      result)))

(defun resolve-clauses (c1 c2 &aux resolvents)
  "
  Purpose:	Resolves two clauses, if possible.
  Remarks:	There may be zero, one, or more ways to resolve the clauses.
  "
  (dolist (l1 c1)
    (dolist (l2 (remove l1 c2))
      (let ((bindings (resolve-literals l1 l2)))
	(unless (eq 'fail bindings)
	  (push (make-variables-unique
		  (instantiate-variables 
		    ;;Delete resolving literals:
		    (append (remove l1 c1) (remove l2 c2))
		    ;;Instantiate with bindings produced by unification:
		    bindings))
		resolvents)))))
  resolvents)

;;;; AUXILIARY PROCEDURES

(defun remove-decorations (clause)
  "
  Purpose:	Get rid of syntactic sugar.
  Remarks:	Removes clause number and OR symbols (v).
  "
  (when (numberp (first clause))
    (setf clause (rest clause)))
  (remove 'v clause))

(defun insert-decorations (clause &optional &aux (result nil))
  "
  Purpose:	Insert syntactic sugar.
  Remarks:	Inserts clause number and OR symbols (v).
  "
  (setf clause (remove-decorations clause))
  (when clause
    (push (first clause) result)
    (dolist (literal (rest (remove 'v clause)))
      (push 'v result)
      (push literal result)))
  (cons (incf *clause-counter*) (reverse result)))

(defun print-clause (clause)
  "
  Purpose:	Prints a clause, more or less readably.
  Remarks:	Assumes clause has been syntactically sugared
		with a clause number and OR symbols (v).
  "
  #+comment
  (setf clause (make-variables-readable clause))
  (format t "~%(~a ~a" (first clause) (second clause))
  (do ((clause (nthcdr 2 clause) (nthcdr 2 clause)))
      ((endp clause))
    (format t "~%   ~a ~a" (first clause) (second clause))))

(defun make-variables-readable (clause)
  "
  Purpose:	Remove unique variable names, replacing them
		with leading characters in names---these should
		be the same as the ones that appear in the
		original clauses.
  "
  (setf clause (make-variables-unique clause))
  (let* ((variables (list-variables clause))
	 (letters
	   ;;Make substitution list:
	   (mapcar #'(lambda (variable)
		       (format nil "~c"
			       ;;Extract first character of printed form:
			       (char (format nil "~a" variable) 0)))
		   variables)))
    (instantiate-variables
	  clause
	  ;;Make binding list:
	  (mapcar #'(lambda (v l) (list v (list '? l)))
		  variables
		  letters))))

;;;; AUXILIARY PROCEDURES BORROWED FROM BACKWARD.lisp

(defun instantiate-variables (clause a-list)
  "
  Purpose:	Create unique variable names, basing them
		on characters that appear in the original
		clause.
  "
  (cond ((atom clause) clause)
        ((eq '? (first clause))
         (let ((binding (find-binding clause a-list)))
           (if binding
               (instantiate-variables (extract-value binding) a-list)
               clause)))
        (t (cons (instantiate-variables (first clause) a-list)
                 (instantiate-variables (rest clause) a-list)))))

(defun make-variables-unique (clause)
  "
  Purpose:	Avoids conflict resulting from having the same variable
		names appear in more than one rule.
  "
  (let* ((variables (list-variables clause))
	 (unique-variables
	   ;;Make substitution list:
	   (mapcar #'(lambda (variable)
		       ;;Generate a new name, based on first character of
		       ;;the given variable, plus a unique number:
		       (with-input-from-string
			 (input (format nil "~c-~a" 
					(aref (format nil "~a" variable) 0)
					(incf *name-counter*)))
			 (read input)))
		   variables)))
    (instantiate-variables
	  clause
	  ;;Make binding list:
	  (mapcar #'(lambda (v l) (list v (list '? l)))
		  variables
		  unique-variables))))

(defun list-variables (clause &optional names)
  "
  Purpose:	Creates a list of variables appearing in a clause.
  "
  (cond ((atom clause) names)
        ((eq '? (first clause))
         ;;Ignore anonymous variable:
         (if (or (eq '\_ (second clause))
                 (member (second clause) names))
             names
             (append names (rest clause))))
        (t (list-variables (rest clause)
                           (list-variables (first clause)
                                           names)))))






