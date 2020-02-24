;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; REMARKS

#|

This version does not use delayed evaluation.

|#

;;;; BASIC ACCESS FUNCTIONS

(defun make-empty-stream () nil)

(defun stream-endp (stream) (endp stream))

(defun stream-first (stream) (first stream))

(defun stream-rest (stream) (rest stream))

(defun stream-cons (object stream) (cons object stream))

(defun stream-append (&rest streams)
  (apply #'append streams))

(defun stream-concatenate (streams) (apply #'append streams))

(defun stream-map (procedure stream)
  (mapcar procedure stream))

(defun stream-member (object stream)
  (member object stream :test #'equal))

(defmacro stream-remember (object variable)
  `(unless (member ,object ,variable :test #'equal)
     (setf ,variable
	   (append ,variable
		   (cons ,object (make-empty-stream))))
     ,object))



