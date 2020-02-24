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

;;;; PROCEDURES

(defun add-binding (pattern-variable-expression datum bindings)
 (if (eq '\_ (extract-variable pattern-variable-expression))
     bindings
     (cons (make-binding
             (extract-variable pattern-variable-expression)
             datum)
           bindings)))

(defun extract-variable (pattern-variable-expression)
  (second pattern-variable-expression))

(defun make-binding (variable datum)
  (list variable datum))

(defun find-binding (pattern-variable-expression binding)
  (unless (eq '\_ (extract-variable pattern-variable-expression))
    (assoc (extract-variable pattern-variable-expression) binding)))

(defun extract-key (binding)
  (first binding))

(defun extract-value (binding)
  (second binding))

(defun match-atoms (p d bindings)
  ;;See if \sy{P} and \sy{D} are the same:
  (if (eql p d)
      ;;If so, return the value of \sy{BINDINGS}:
      bindings
      ;;Otherwise, return \sy{FAIL}.
      'fail))

(defun match-variable (p d bindings)
  (let ((binding (find-binding p bindings)))
    ;;See if the pattern variable is known:
    (if binding 
        ;;If it is, substitute its value and try again:
        (match (extract-value binding) d bindings)      
        ;;Otherwise, add new binding:
        (add-binding p d bindings))))

(defun match-pieces (p d bindings)
  (let ((result (match (first p) (first d) bindings)))
    ;;See if the first parts match producing new bindings:
    (if (eq 'fail result)
        ;;If they do not match, fail.
        'fail
        ;;If they do match, try the rest parts using the resulting bindings:
        (match (rest p) (rest d) result))))

(defun elements-p (p d) 
  (and (atom p) (atom d)))

(defun variable-p (p)
  (and (listp p) (eq '? (first p))))

(defun recursive-p (p d)
  (and (listp p) (listp d)))

(defun match (p d &optional bindings)
  (cond ((elements-p p d)  (match-atoms p d bindings))
        ((variable-p p)    (match-variable p d bindings))
        ((recursive-p p d) (match-pieces p d bindings))
        (t 'fail)))

(defun unify (p1 p2 &optional bindings)
  (cond ((elements-p p1 p2)                  ;Are both atoms?
         (unify-atoms p1 p2 bindings))       ;If yes, ok; if no, fail.
        ((variable-p p1)                     ;Is \sy{P1} a variable?
         (unify-variable p1 p2 bindings))    ;Unify using bindings.
        ((variable-p p2)                     ;Is \sy{P2} a variable?
         (unify-variable p2 p1 bindings))    ;Unify using bindings.
        ((recursive-p p1 p2)                 ;Are both lists?
         (unify-pieces p1 p2 bindings))      ;Unify pieces.
        (t 'fail)))

(defun unify-atoms (p1 p2 bindings)  ;Identical to \sy{MATCH-ATOMS}.
  (if (eql p1 p2) bindings 'fail))

(defun unify-pieces (p1 p2 bindings) ;Identical to \sy{MATCH-PIECES}.
  (let ((result (unify (first p1) (first p2) bindings)))
    (if (eq 'fail result)
        'fail
        (unify (rest p1) (rest p2) result))))

(defun unify-variable (p1 p2 bindings)
  (let ((binding (find-binding p1 bindings)))   ;Find binding, if any.
    (if binding                                 ;Is there a binding?
        (unify (extract-value binding) p2 bindings) ;If yes, use value.
        (if (insidep p1 p2 bindings)                ;Is \sy{P1} inside \sy{P2}?
            'fail                                   ;If yes, fail.
            (add-binding p1 p2 bindings)))))        ;If no, add binding.

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
             (inside-or-equal-p variable
                                (extract-value binding)
                                bindings))))
        (t (or (inside-or-equal-p variable
                                  (first expression)
                                  bindings)
               (inside-or-equal-p variable
                                  (rest expression)
                                  bindings)))))

