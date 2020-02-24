;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; REMARKS

#|

This version uses delayed evaluation.  Note that
encapsulate is designed so that each stream
element is computed only once.

|#

;;;; AUXILIARY PROCEDURES FOR LEXICAL ENCAPSULATION

(defmacro encapsulate (form)	;From a problem solution.
  `(let ((switch nil) (result nil))
       #'(lambda ()
	   (if switch
	       result
	     (setf switch t result ,form)))))

(defmacro expose-stream-element (procedure)
    `(funcall ,procedure))

;;;; BASIC ACCESS FUNCTIONS WITH DELAYED EVALUATION

(defun make-empty-stream () 'empty-stream)

(defun stream-endp (stream) (eq stream 'empty-stream))

(defun stream-first (stream) (first stream))

(defun stream-rest (stream)		
  (expose-stream-element (second stream)))

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

(defun stream-map (procedure stream)
  (if (stream-endp stream)
      'empty-stream
      (stream-cons (funcall procedure (stream-first stream))
                   (stream-map procedure
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



