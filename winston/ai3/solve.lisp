;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; REMARKS

#|

Solves linear equations by row reduction.  
Doubtlessly has terrible numeric properties.

|#

;;;; USER-LEVEL PROCEDURE

(defun solve-equations (rows &aux solution)
  "
  Purpose:	Solve linear equations.
  Arguments:	Rows, in the following form:
		((c11 c12 c13 c1)
		 (c21 c22 c23 c2)
		 (c31 c32 c33 c3))
		where
		  c11 x + c12 y + c13 z = c1
		  c21 x + c22 y + c23 z = c2
		  c31 x + c32 y + c33 z = c3
  Returns:	(x y z)
  "
  (setf rows (mapcar #'(lambda (e) (append e nil)) rows))
  (dotimes (n (length rows))
    (setf (nth n rows)
	  (scale-row (/ (float (nth n (nth n rows)))) (nth n rows)))
    (dotimes (m (length rows))
      (unless (= m n)
	(setf (nth m rows)
	      (add-two-rows (nth m rows)
			    (scale-row (- (nth n (nth m rows)))
				       (nth n rows)))))))
  (setf solution (apply #'append (mapcar #'last rows)))
  solution)

;;;; AUXILIARIES

(defun add-two-rows (r1 r2) (mapcar #'+ r1 r2))

(defun scale-row (c r) (mapcar #'(lambda (e) (* c e)) r))

