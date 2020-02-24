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

This version is the same as the one in the book except that a bug was fixed
in PROPAGATE-VIA-ASSERTION (one of the FORMAT form's arguments were in
another FORMAT form).

|#

;;;; PROCEDURES

(defclass assertion ()
  ((name        :accessor assertion-name :initarg :name)
   (lower-bound :accessor assertion-lower-bound :initform 0)
   (upper-bound :accessor assertion-upper-bound :initform 1)
   (constraints :accessor assertion-constraints :initform nil)))

(defclass constraint ()
  ((name   :accessor constraint-name :initarg :name)
   (output :accessor constraint-output)))
(defclass binary-constraint (constraint)
  ((input :accessor constraint-input)))
(defclass ternary-constraint (constraint)
  ((input-a :accessor constraint-input-a)
   (input-b :accessor constraint-input-b)))

(defclass not-box (binary-constraint) ())

(defclass or-box (ternary-constraint) ())

(defclass and-box (ternary-constraint) ())

(defmethod connect2 ((c constraint)
                     (i assertion)
                     (o assertion))
  (setf (constraint-input  c) i)
  (setf (constraint-output c) o)
  (push c (assertion-constraints i))
  (push c (assertion-constraints o)))

(defmethod connect3 ((c constraint)
                     (a assertion)
                     (b assertion)
                     (o assertion))
  (setf (constraint-input-a c) a)
  (setf (constraint-input-b c) b)
  (setf (constraint-output  c) o)
  (push c (assertion-constraints a))
  (push c (assertion-constraints b))
  (push c (assertion-constraints o)))

(defmethod propagate-via-box ((constraint or-box))
  (let* ((a (constraint-input-a constraint))
         (b (constraint-input-b constraint))
         (o (constraint-output constraint))
         (la (assertion-lower-bound a))
         (ua (assertion-upper-bound a))
         (lb (assertion-lower-bound b))
         (ub (assertion-upper-bound b))
         (lo (assertion-lower-bound o))
         (uo (assertion-upper-bound o)))
    (propagate-via-assertion o constraint (max la lb) (+ ua ub))
    (propagate-via-assertion a constraint (- lo ub) uo)
    (propagate-via-assertion b constraint (- lo ua) uo)))

(defmethod propagate-via-box ((constraint and-box))
  (let* ((a (constraint-input-a constraint))
         (b (constraint-input-b constraint))
         (o (constraint-output constraint))
         (la (assertion-lower-bound a))
         (ua (assertion-upper-bound a))
         (lb (assertion-lower-bound b))
         (ub (assertion-upper-bound b))
         (lo (assertion-lower-bound o))
         (uo (assertion-upper-bound o)))
    (propagate-via-assertion o constraint (+ la lb -1) (min ua ub))
    (propagate-via-assertion a constraint 0 (+ 1 (- uo lb)))
    (propagate-via-assertion b constraint 0 (+ 1 (- uo la)))))

(defmethod propagate-via-assertion ((assertion assertion) 
                                    (source constraint)
                                    lower
                                    upper)
  (let* ((old-upper (assertion-upper-bound assertion))
         (old-lower (assertion-lower-bound assertion))
         (new-upper (max 0 (min old-upper upper)))
         (new-lower (min 1 (max old-lower lower))))
    (unless (= old-upper new-upper)
      (setf (assertion-upper-bound assertion) new-upper))
    (unless (= old-lower new-lower)
      (setf (assertion-lower-bound assertion) new-lower))
    (when (or (/= old-lower new-lower) (/= old-upper new-upper))
      (format t "~%Constraint ~a has modified ~a's values:"
              (constraint-name source)
              (assertion-name assertion))
      (format t
	      #+gclisp
	      "~%[~a, ~a] --> [~a, ~a]"
	      #-gclisp
	      "~%[~4,2f, ~4,2f] --> [~4,2f, ~4,2f]" 
              old-lower old-upper
              new-lower new-upper)
      (dolist (constraint (assertion-constraints assertion))
        (propagate-via-box constraint)))))

(defmethod initiate-propagation ((assertion assertion) 
                                 lower
                                 upper)
    (setf (assertion-upper-bound assertion) upper)
    (setf (assertion-lower-bound assertion) lower)
    (format t "~%You have started propagation from ~a with values:"
            (assertion-name assertion))
    (Format t
	    #+gclisp
	    "~%[~a, ~a]"
	    #-gclisp
	    "~%[~4,2f, ~4,2f]"
            lower upper)
      (dolist (constraint (assertion-constraints assertion))
        (propagate-via-box constraint)))


