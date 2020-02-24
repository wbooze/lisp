;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;; Define Variables

(defparameter *array-size* 30)

;;; Create Arrays and Array Printer

(defconstant barometer (make-array *array-size*) :initial-element 0)
(defconstant confidence (make-array *array-size*) :initial-element 0)

(defun print-array (array)
  (terpri)
  (dotimes (n *array-size*)
    (format t "~a " (tilde-f (aref array n) nil 1))))

;;; Implement Relaxation Formua

(defun relax (constant-array confidence-array old-array new-array)
  (let ((limit *array-size*))
    (dotimes (n limit)
      (setf (aref new-array n)
	    (+ (* (aref confidence-array n) (aref constant-array n))
	       (* (- 1 (aref confidence-array n))
		  (cond ((zerop n) (aref old-array (1+ n)))
			((= n (1- limit)) (aref old-array (1- n)))
			(t (/ (+ (aref old-array (1- n))
				 (aref old-array (1+ n)))
			      2)))))))))

;;; Implement Relaxation

(defun propagate (iterations)
  "
  Purpose:	Perform ITERATIONS steps of parallel iteration.
  Returns:	Nothing.  Results printed.
  Caveats:	Argument must be > or = 1.
  Remarks:	Inefficient because ends handled inside, rather than
		outside the loop, and because array is copied, rather
		than switched.  Good enough for demonstration only.
  "
  (let ((array1 (make-array *array-size* :initial-element 0))
	(array2 (make-array *array-size* :initial-element 0)))
    (do ((new (relax barometer confidence barometer array2)
	      (relax barometer confidence array1 array2))
	 (count (1- iterations) (1- count)))
	((zerop count) (print-array array2) (values))
      (dotimes (n *array-size*)
	(setf (aref array1 n) (aref array2 n))))))

;;; Test Data

(progn
  (setf (aref barometer 0) 0)	(setf (aref confidence 0) 1.0)
  (setf (aref barometer 3) 6)	(setf (aref confidence 3) .9)
  (setf (aref barometer 5) 8)	(setf (aref confidence 5) .9)
  (setf (aref barometer 7) 10)	(setf (aref confidence 7) .1)
  (setf (aref barometer 10) 3)	(setf (aref confidence 10) .7)
  
  (setf (aref barometer 16) 6)	(setf (aref confidence 16) .1)
  (setf (aref barometer 21) 0)	(setf (aref confidence 21) 1.0)
  (setf (aref barometer 22) 6)	(setf (aref confidence 22) .9)
  (setf (aref barometer 28) 1)	(setf (aref confidence 28) .7)
  (setf (aref barometer 29) 1)	(setf (aref confidence 29) 1.0)
  (values))

(print-array barometer)
0.0 0.0 0.0 6.0 0.0 8.0 0.0 10.0 0.0 0.0 3.0 0.0 0.0 0.0 0.0 0.0 6.0 0.0 0.0 0.0 0.0 0.0 6.0 0.0 0.0 0.0 0.0 0.0 1.0 1.0 

(print-array confidence)
1.0 0.0 0.0 0.8 0.0 0.8 0.0 0.0 0.0 0.0 0.6 0.0 0.0 0.0 0.0 0.0 0.1 0.0 0.0 0.0 0.0 1.0 0.8 0.0 0.0 0.0 0.0 0.0 0.6 1.0 

(propagate 1)
0.0 0.0 3.0 5.3 7.0 7.1 9.0 0.9 5.0 1.5 2.0 1.5 0.0 0.0 0.0 3.0 0.5 3.0 0.0 0.0 0.0 0.0 5.3 3.0 0.0 0.0 0.0 0.5 0.8 1.0 

(propagate 5)
0.0 1.7 3.8 5.9 6.9 7.8 7.4 4.6 5.4 2.6 2.9 2.0 0.9 1.5 0.4 2.2 1.0 2.1 0.2 0.9 0.0 0.0 5.5 3.8 2.1 1.4 0.6 0.8 0.9 1.0 

(propagate 10)
0.0 1.9 3.9 5.9 6.9 7.9 6.9 6.8 4.7 4.3 3.0 2.3 2.2 1.3 2.1 1.2 2.5 0.9 1.5 0.3 0.5 0.0 5.6 4.2 3.2 2.2 1.7 1.2 1.0 1.0 

(propagate 20)
0.0 1.9 3.9 5.9 6.9 7.9 7.3 6.8 5.5 4.3 3.1 2.7 2.8 2.1 2.6 2.0 2.7 1.6 1.5 0.6 0.5 0.0 5.6 4.7 3.9 3.0 2.4 1.7 1.1 1.0 

(propagate 40)
0.0 1.9 3.9 5.9 6.9 7.9 7.3 6.8 5.6 4.4 3.2 3.0 3.1 2.8 3.0 2.8 3.0 2.2 1.7 1.0 0.5 0.0 5.6 4.8 4.1 3.3 2.6 1.8 1.1 1.0 
