;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; DATA STRUCTURE

(defstruct approximation-net centers weights sigmas)

;;;; USER-LEVEL PROCEDURES

(defmacro make-interpolation-net (sigma &rest samples)
  "
  Purpose:	Create an interpolation net.
  Arguments:	Sigma
		(o1 x11 x12 x13)	;First sample, with desired output. 
		(o2 x21 x22 x23)
	        ...
	        (on xn1 xn2 xn3)	;Nth sample, with desired output. 
  Returns:	Interpolation net structure.
  Remarks:	Real work is done by MAKE-NET.
  "
  `(make-net ',samples ,sigma))

(defun make-net (samples sigma &aux result)
  "
  Remarks:	See MAKE-INTERPOLATION-NET.
  "
  (let ((outputs (mapcar #'first samples))
	(inputs (mapcar #'rest samples))
	(sigmas (mapcar #'(lambda (e) sigma) samples)))
    (setf result (make-approximation-net :centers inputs :sigmas sigmas))
    (setf (approximation-net-weights result)
	  (let ((equations nil))
	    (do ((centers inputs (rest centers))
		 (outputs outputs (rest outputs)))
		((endp centers))
	      (let ((equation nil))
		(dolist (input inputs (push (first outputs) equation))
		  (push (hidden-output input (first centers) sigma) equation))
		(push (reverse equation) equations)))
	    (solve-equations (reverse equations)))))
    result)

(defmacro train-weights (net rate steps &rest samples)
  "
  Purpose:	Train an already-created interpolation net.
  Arguments:	The net, a rate constant, the number of training steps,
		the training samples.
  Returns:	Approximation net structure.
  Remarks:	Real work done by TRAIN-WEIGHTS-AUX.
  "
  `(train-weights-aux ,net ',samples ,rate ,steps 1000))

(defun train-weights-aux (net samples rate steps &optional (report 10))
  "
  Remarks:	See TRAIN-WEIGHTS.
  "
  (dotimes (step steps net)
    ;;Compute weight derivatives:
    (let ((derivatives
	    (vector-times-scaler rate
				 (compute-weight-derivatives net samples))))
      ;;Adjust:
      (setf (approximation-net-weights net)
	    (vector-sum derivatives (approximation-net-weights net))))))

(defmacro train-weights-and-centers (net rate steps &rest samples)
  "
  Purpose:	Train an already-created interpolation net.
  Arguments:	The net, a rate constant, the number of training steps,
		the training samples.
  Returns:	Approximation net structure.
  Remarks:	Real work done by TRAIN-WEIGHTS-AUX.
  "
  `(train-weights-and-centers-aux ,net ',samples ,rate ,steps 1000))

(defun train-weights-and-centers-aux
       (net samples rate steps &optional (report 10))
  (dotimes (step steps net)
    ;;Compute weight derivatives:
    (let ((weight-derivatives
	    (vector-times-scaler rate
				 (compute-weight-derivatives net samples))))
      ;;Adjust:
      (setf (approximation-net-weights net)
	    (vector-sum weight-derivatives (approximation-net-weights net))))
    ;;Compute center derivatives:
    (let ((center-derivatives
	    (mapcar #'(lambda (v) (vector-times-scaler rate v))
		    (compute-center-derivatives net samples))))
      ;;Adjust:
      (setf (approximation-net-centers net)
	    (mapcar #'(lambda (v1 v2) (vector-sum v1 v2))
		    center-derivatives
		    (approximation-net-centers net))))))

(defun compute-net-output (net vector)
  (reduce #'+
	  (mapcar #'(lambda (center weight sigma)
		      (* weight (hidden-output vector center sigma)))
		  (approximation-net-centers net)
		  (approximation-net-weights net)
		  (approximation-net-sigmas net))))

;;;; COMPUTE ERRORS AND DERIVATIVES

(defun compute-error (net samples)
  (let ((sum 0.0))
    (dolist (sample samples sum)
      (let ((desired-output (first sample))
	    (inputs (rest sample)))
	(incf sum
	      (expt (- desired-output (compute-net-output net inputs)) 2))))))

(defun compute-weight-derivatives (net samples
	    &aux
	    (sigma (first (approximation-net-sigmas net)))
	    (result nil))
  "
  Remarks:	Embodies result of differentiation.
  "
  (dolist (center (approximation-net-centers net) (reverse result))
    (push
      (let ((sum 0.0))
	(dolist (sample samples)
	  (let ((desired-output (first sample))
		(inputs (rest sample)))
	    (incf sum
		  (* (- desired-output (compute-net-output net inputs))
		     (hidden-output inputs center sigma)))))
	sum)
      result)))

(defun compute-center-derivatives (net samples
	    &aux
	    (weights (approximation-net-weights net))
	    (centers (approximation-net-centers net))
	    (sigma (first (approximation-net-sigmas net)))
	    (result nil))
  "
  Remarks:	Embodies result of differentiation.
  "
  (dotimes (m (length weights) (reverse result))
    (let ((center (nth m centers))
	  (weight (nth m weights))
	  (center-deltas nil))
      (dotimes (n (length center) (push (reverse center-deltas) result))
	(let ((sum 0.0))
	  (dolist (sample samples)
	    (let ((desired-output (first sample))
		  (inputs (rest sample)))
	      (incf sum
		    (* (/ weight sigma)
		       (- (nth n inputs) (nth n center))
		       (- desired-output (compute-net-output net inputs))
		       (hidden-output inputs center sigma)))))
	  (push sum center-deltas))))))
      
;;;; AUXILIARY PROCEDURES

(defun vector-times-scaler (s v)
  (mapcar #'(lambda (e) (* s e)) v))

(defun vector-sum (x y)
  (mapcar #'+ x y))

(defun vector-difference (x y)
  (mapcar #'- x y))

(defun vector-norm-squared (x)
  (reduce #'+ (mapcar #'(lambda (e) (* e e)) x)))

(defun hidden-output (vector center sigma)
  (gauss-of-x2 (vector-norm-squared (vector-difference vector center)) sigma))

(defun gauss-of-x2 (x sigma)
  (exp (* (/ -.5 sigma) x)))

(defun gauss (x sigma)
  (exp (* (/ -.5 sigma) (* x x))))

;;;; DISPLAY RESULTS

(defun print-weights (net)
  (format t "~%Weights: ")
  (dolist (weight (approximation-net-weights net))
    (format t "~a" (tilde-f weight 10 2))))

(defun print-weights-and-centers (net)
  (format t "~%Weights: ")
  (dolist (weight (approximation-net-weights net))
    (format t "~a" (tilde-f weight 10 2)))
  (format t "~%Centers: ")
  (dolist (center (approximation-net-centers net))
    (format t "~a" (tilde-f (first center) 10 2))))

(defun tilde-f (n &optional w d)
  "
  Remarks:	A hack; enables use with Common Lisp subsets with weak 
		implementations of FORMAT.
  "
  (let ((string ""))
    (if d
	(setf string
	  (format nil "~a~a.~a"
	    (if (minusp n)
		(progn (setf n (- n)) "-")
	      "")
	    (floor n)
	    (dotimes (d d string)
	      (setf string
		    (concatenate
		      'string
		      string
		      (format nil "~a"
			      (floor (setf n (* 10 (- n (floor n)))))))))))
      (setf string (format nil "~a" n)))
    (when w
      (setf string
	    (if (> (length string) w)
		(make-string w :initial-element #\*)
	      (concatenate 'string
			   (make-string (- w (length string))
					:initial-element #\space)
			   string))))
    string))
