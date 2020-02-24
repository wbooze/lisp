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

This version uses delayed evaluation.  There are small, but important
improvements relative to the version provided in the book.  The
ENCAPSULATE procedure, for example, ensures that each stream element
is computed only once.  Also, MAKE-EMPTY-STREAM is introduced;
(MAKE-EMPTY-STREAM) replaces 'EMPTY-STREAM.

|#

;;;; BASIC ACCESS FUNCTIONS WITH DELAYED EVALUATION

(defun make-empty-stream () 'empty-stream)

(defun stream-endp (stream) (eq stream 'empty-stream))

(defun stream-first (stream) (first stream))

(defun stream-rest (stream)		
  (expose (second stream)))

(defmacro stream-cons (object stream)	
  `(list ,object (encapsulate ,stream)))

(defun stream-append (stream1 stream2)
  (if (stream-endp stream1)
      stream2
      (stream-cons (stream-first stream1)
                   (stream-append (stream-rest stream1)
                                  stream2))))

(defun stream-concatenate (streams)
  (if (stream-endp streams)
      'empty-stream
    (if (stream-endp (stream-first streams))
	(stream-concatenate (stream-rest streams))
      (stream-cons (stream-first (stream-first streams))
		   (stream-concatenate
		     (stream-cons (stream-rest (stream-first streams))
				  (stream-rest streams)))))))

(defun stream-transform (procedure stream)
  (if (stream-endp stream)
      'empty-stream
      (stream-cons (funcall procedure (stream-first stream))
                   (stream-transform procedure
                                     (stream-rest stream)))))

(defun stream-member (object stream)
  (cond ((stream-endp stream) nil)
        ((equal object (stream-first stream)) t)
        (t (stream-member object (stream-rest stream)))))

(defmacro stream-remember (object variable)
  `(unless (stream-member ,object ,variable)
       (setf ,variable
	     (stream-append ,variable
			    (stream-cons ,object
					 'empty-stream)))
       ,object))

;;;; AUXILIARY PROCEDURES FOR LEXICAL ENCAPSULATION

(defmacro encapsulate (form)	;From a problem solution.
  `(let ((switch nil) (result nil))
       #'(lambda ()
	   (cond (switch #+comment
			 (format t "~%Remembering ...")
			 result)
		 (t #+comment
		    (format t "~%Computing ...")
		    (setf switch t result ,form))))))

(defmacro expose (procedure)
    `(funcall ,procedure))


