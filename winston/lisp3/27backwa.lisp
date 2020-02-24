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

This version is the same as the one in the book except that
(MAKE-EMPTY-STREAM) replaces 'EMPTY-STREAM.

Also fixed following bug:

(gentemp variable)
-->
(gensym (format nil "~a" variable))

|#

(defun remember-assertion (assertion)
  (stream-remember assertion *assertions*))

(defun remember-rule (rule)
  (stream-remember rule *rules*))

(defun rule-name (rule) (first rule))

(defun rule-ifs (rule) (butlast (rest rule)))

(defun rule-then (rule) (first (last rule)))

(defun apply-filters (patterns initial-input-stream)
  (if (endp patterns)
      initial-input-stream
      (apply-filters (rest patterns)
                     (filter-binding-stream (first patterns)
                                            initial-input-stream))))

(defun filter-binding-stream (pattern stream)
  (stream-concatenate
    (stream-transform
      #'(lambda (bindings)
          (stream-concatenate
            (stream-cons
              (match-pattern-to-assertions pattern bindings)
              (stream-cons
                (match-pattern-to-rules pattern bindings)
                (make-empty-stream)))))
      stream)))

(defun match-pattern-to-assertions (pattern bindings)
  (stream-concatenate
    (stream-transform
      #'(lambda (assertion) (try-assertion pattern
                                           assertion
                                           bindings))
      *assertions*)))

(defun try-assertion (pattern assertion bindings)
  (let ((result (match pattern assertion bindings)))
    (if (eq 'fail result)
        (make-empty-stream)
        (stream-cons result (make-empty-stream)))))

(defun match-pattern-to-rules (pattern bindings)
  (stream-concatenate
    (stream-transform
      #'(lambda (rule) (try-rule pattern rule bindings))
      *rules*)))

(defun try-rule (pattern rule bindings)
  (let* ((rule (make-variables-unique rule))
         (result (unify pattern (rule-then rule) bindings)))
    (if (eq 'fail result)
        (make-empty-stream)
        (apply-filters (rule-ifs rule)
                       (stream-cons result
                                    (make-empty-stream))))))

(defun backward-chain (&rest patterns)
  (let ((binding-stream
          (apply-filters patterns
                         (stream-cons nil (make-empty-stream))))
        (variables (list-variables patterns))
        (displayed-answers nil))
    (if (endp variables)
        (if (stream-endp binding-stream)
            'no
            'yes)
        (do ((binding-stream binding-stream
                             (stream-rest binding-stream)))
            ((stream-endp binding-stream) 'no-more)
          (let ((answer
                  (make-answer variables
                               (stream-first binding-stream))))
            (unless (member answer displayed-answers
                            :test #'equal)
              (display-answer answer)
              (setf displayed-answers
                    (cons answer displayed-answers))))))))

(defun list-variables (tree &optional names)
  (cond ((atom tree) names)
        ((eq '? (first tree))
         (if (member (second tree) names)
             names
             (append names (rest tree))))
        (t (list-variables (rest tree)
                           (list-variables (first tree)
                                           names)))))

(defun make-answer (variables bindings)
  (instantiate-variables 
    (mapcar #'(lambda (variable)
                 (list variable (list '? variable)))
            variables)
    bindings))

(defun instantiate-variables (pattern a-list)
  (cond ((atom pattern) pattern)
        ((eq '? (first pattern))
         (let ((binding (find-binding pattern a-list)))
           (if binding
               (instantiate-variables (extract-value binding)
                                      a-list)
               pattern)))
        (t (cons (instantiate-variables (first pattern)
                                        a-list)
                 (instantiate-variables (rest pattern)
                                        a-list)))))

(defun display-answer (answers)
  (format t "~&-->")
  (dolist (answer answers)
    (format t " ~a = ~a" (first answer) (second answer))))

(defun make-variables-unique (rule)
  (let ((variables (list-variables rule)))
    (dolist (variable variables rule)
      (setf rule
            (instantiate-variables
              rule
              (list (list variable
                          (list '? (gensym (format nil "~a" variable))))))))))


