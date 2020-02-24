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

(defun compile-branches (forms)
  (unless (endp forms)
    `(multiple-value-bind (result binding words-left-over)
         ,(compile-elements (first forms))
         (if result
             (values result binding words-left-over)
             ,(compile-branches (rest forms))))))

(defun compile-elements (tree)
  (cond ((endp tree) '(values t nil word-list))
        ((eq '> (first tree))
         `(multiple-value-bind (result ,(second tree) word-list)
              (,(second tree) word-list)
            (when result
                  ,(compile-elements (rest (rest tree))))))
        ((eq 'brnchs (first tree))
         (compile-branches (rest tree)))
        ((eq 'rtn (first tree))
         `(values t (progn ,@(rest tree)) word-list))
        ((eq 'if-end-rtn (first tree))
         `(when (null word-list)
            (values t (progn ,@(rest tree)) nil)))
        (t `(let ((current-word (first word-list))
                  (word-list (rest word-list)))
              (when (eq current-word ',(first tree))
                  ,(compile-elements (rest tree)))))))

(defmacro compile-tree (name tree)
  `(defun ,name (word-list)
     ,(compile-elements tree)))

