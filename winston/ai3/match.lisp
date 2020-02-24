;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; MATCHER

(defun match (p d &optional bindings)
  "
  Purpose:	Matching
  Arguments:	Pattern, datum, optional bindings.
  Returns:	A list of bindings, which may be NIL, or FAIL.
  Remarks:	Pattern variables are indicated by (? <variable name>)
		Prolog's nameless variable is indicated by (? _)
  "
  (cond ((and (atom p) (atom d))
         (match-atoms p d bindings))
        ((and (listp p) (eq '? (first p)))
         (match-variable p d bindings))
	;;Added p & d to ensure that there is something left, 10/29/91:
        ((and p d (listp p) (listp d))
         (match-pieces p d bindings))
        (t 'fail)))

(defun match-atoms (p d bindings)
  ;;Are the pattern and datum the same:
  (if (eql p d)
      ;;If so, return the value of BINDINGS:
      bindings
      ;;Otherwise, return FAIL.
      'fail))

(defun match-variable (p d bindings)
  (let ((binding (find-binding p bindings)))
    ;;Is the pattern variable on the list of bindings:
    (if binding 
        ;;If it is, substitute its value an try again:
        (match (extract-value binding) d bindings)      
        ;;Otherwise, add new binding:
        (add-binding p d bindings))))

(defun match-pieces (p d bindings)
  (let ((result (match (first p) (first d) bindings)))
    ;;See if the FIRST parts match producing new bindings:
    (if (eq 'fail result)
	;;If they do not match, fail.
	'fail
	;;If they do match, try the REST parts using the resulting bindings:
	(match (rest p) (rest d) result))))

;;;; AUXILIARY PROCEDURES

(defun add-binding (pattern-variable-expression datum bindings)
 (if (eq '_ (second pattern-variable-expression))
     bindings
     (cons (list (second pattern-variable-expression) datum) bindings)))

(defun find-binding (pattern-variable-expression binding)
  (unless (eq '_ (second pattern-variable-expression))
    (assoc (second pattern-variable-expression) binding)))

(defun extract-key (binding)
  (first binding))

(defun extract-value (binding)
  (second binding))

;;;; UNIFYER (not used by forward or backard chainers)

(defun unify (p1 p2 &optional bindings)
  (cond ((and (atom p1) (atom p2))              ;Are both arguments atoms?
         (unify-atoms p1 p2 bindings))          ;If yes, ok; if no, fail.
        ((and (listp p1) (eq '? (first p1)))    ;Is p1 a variable?
         (unify-variable p1 p2 bindings))       ;Unify variable using bindings.
        ((and (listp p2) (eq '? (first p2)))    ;Is p2 a variable?
         (unify-variable p2 p1 bindings))       ;Unify variable using bindings.
        ((and (listp p1) (listp p2))            ;Are both patterns lists?
         (unify-pieces p1 p2 bindings))         ;Unify pieces.
        (t 'fail)))

(defun unify-atoms (p1 p2 bindings)     ;Identical to MATCH-ATOMS.
  (if (eql p1 p2) bindings 'fail))

(defun unify-pieces (p1 p2 bindings)    ;Identical to MATCH-PIECES.
  (let ((result (unify (first p1) (first p2) bindings)))
    (if (eq 'fail result)
        'fail
        (unify (rest p1) (rest p2) result))))

(defun unify-variable (p1 p2 bindings)
  (let ((binding (find-binding p1 bindings)))           ;Find binding, if any.
    (if binding                                         ;Is there a binding?
        (unify (extract-value binding) p2 bindings)     ;If yes, use value.
        (if (insidep p1 p2 bindings)                    ;Is p1 inside p2?
            'fail                                       ;If yes, fail.
            (add-binding p1 p2 bindings)))))            ;If no, add binding.

(defun insidep (variable expression bindings)
  (if (equal variable expression)
      nil
      (inside-or-equal-p variable expression bindings)))

(defun inside-or-equal-p (variable expression bindings)
  (cond ((equal variable expression) t)
        ((atom expression) nil)
        ((eq '? (first expression))
         (let ((binding (find-binding expression bindings)))
           (when binding
             (inside-or-equal-p variable (extract-value binding) bindings))))
        (t (or (inside-or-equal-p variable (first expression) bindings)
               (inside-or-equal-p variable (rest expression) bindings)))))


