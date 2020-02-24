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

(defun extend (path)
  (print (reverse path))                             ;Print path.
  (mapcar #'(lambda (new-node) (cons new-node path)) ;Form new paths.
          (remove-if #'(lambda (neighbor) (member neighbor path))
                     (get (first path) 'neighbors))))

(defun depth-first (start finish &optional
                                 (queue (list (list start))))
  (cond ((endp queue) nil)                      ;Queue empty?
        ((eq finish (first (first queue)))      ;Finish found?
         (reverse (first queue)))               ;Return path.
        (t (depth-first                         ;Try again.
             start
             finish
             (append (extend (first queue))     ;New paths in front.
                     (rest queue))))))          ;Skip extended path.

(defun breadth-first (start finish &optional
                                   (queue (list (list start))))
  (cond ((endp queue) nil)                      ;Queue empty?
        ((eq finish (first (first queue)))      ;Finish found?
         (reverse (first queue)))               ;Return path.
        (t (breadth-first                       ;Try again.
             start
             finish
             (append (rest queue)                ;Skip extended path.
                     (extend (first queue))))))) ;New paths in back.

(defun best-first (start finish &optional
                                (queue (list (list start))))
  (cond ((endp queue) nil)                      ;Queue empty?
        ((eq finish (first (first queue)))      ;Finish found?
         (reverse (first queue)))               ;Return path.
        (t (best-first                          ;Try again.
             start
             finish
             (sort (append (extend (first queue))
                           (rest queue))
                   #'(lambda (p1 p2) (closerp p1 p2 finish)))))))

(defun straight-line-distance (node-1 node-2)
  (let ((coordinates-1 (get node-1 'coordinates))
        (coordinates-2 (get node-2 'coordinates)))
    (sqrt (+ (expt (- (first coordinates-1)
                      (first coordinates-2))
                   2)
             (expt (- (second coordinates-1)
                      (second coordinates-2))
                   2)))))

(defun closerp (path-1 path-2 target-node)
  (< (straight-line-distance (first path-1) target-node)
     (straight-line-distance (first path-2) target-node)))

;;;; DATA

(setf (get 's 'neighbors) '(a d)
      (get 'a 'neighbors) '(s b d)
      (get 'b 'neighbors) '(a c e)
      (get 'c 'neighbors) '(b)
      (get 'd 'neighbors) '(s a e)
      (get 'e 'neighbors) '(b d f)
      (get 'f 'neighbors) '(e))

(setf (get 's 'coordinates) '(0 3)
      (get 'a 'coordinates) '(4 6)
      (get 'b 'coordinates) '(7 6)
      (get 'c 'coordinates) '(11 6)
      (get 'd 'coordinates) '(3 0)
      (get 'e 'coordinates) '(6 0)
      (get 'f 'coordinates) '(11 3))

