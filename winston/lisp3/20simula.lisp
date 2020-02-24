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

(defun connect (task output inputs)
  (setf (task-input-events task) inputs)        ;Inputs
  (dolist (event inputs)
    (push task (event-output-tasks event)))
  (setf (task-output-event task) output)        ;Output
  (setf (event-input-task output) task))

(defun simulate-event (time event)
  (setf (event-time event) time)
  (dolist (output-task (event-output-tasks event))
    (simulate-task output-task)))

#-gclisp

(defun announce-start-time (time name)
  (format t "~%Time ~a:~9tstarting~19t~a." time name))

#-gclisp

(defun announce-finish-time (time name)
  (format t "~%Time ~a:~9tfinishing~19t~a." time name))

#+gclisp

(defun announce-start-time (time name)
  (format t "~%Time ~a: starting ~a." time name))

#+gclisp

(defun announce-finish-time (time name)
  (format t "~%Time ~a: finishing ~a." time name))

(defstruct (event (:print-function print-event))
  (time 'unknown)
  (input-task nil)
  (output-tasks nil))

(defun print-event (structure &rest ignore)
  (format t "<event structure>"))

(defstruct (task (:print-function print-task))
  (name 'unknown)
  (input-events nil)
  (output-event nil)
  (duration 'unknown))

(defun print-task (structure &rest ignore)
  (format t "<task structure for ~a>" (task-name structure)))

(defun add-to-event-sequence (form)
  (setf *event-sequence*
        (sort (cons form *event-sequence*)
              #'earlier-first-p)))

(defun earlier-first-p (x y)
  (cond ((< (second x) (second y)) t)
        ((= (second x) (second y))
         (cond ((eq 'announce-finish-time (first x)) t)
               ((eq 'announce-finish-time (first y)) nil)
               ((eq 'simulate-event (first x)) t)
               ((eq 'simulate-event (first y)) nil)))))

(defun event-times-known-p (list-of-events)
  (not (find-if-not
         #'(lambda (event) (numberp (event-time event)))
         list-of-events)))

(defun latest-time (events)
  (apply #'max (mapcar #'event-time events)))

(defun simulate-task (task)
  (when (event-times-known-p (task-input-events task))
    (let* ((start-time (latest-time (task-input-events task)))
           (finish-time (+ (task-duration task) start-time)))
      (add-to-event-sequence
        `(announce-start-time ,start-time ',(task-name task)))
      (add-to-event-sequence
        `(announce-finish-time ,finish-time ',(task-name task)))
      (add-to-event-sequence 
        `(simulate-event ,finish-time ,(task-output-event task))))))

(defun simulate (starting-event time)
  (setf *event-sequence* nil)
  (simulate-event time starting-event)
  (loop
    (if (endp *event-sequence*)
        (return 'done)
      (eval (pop *event-sequence*)))))

