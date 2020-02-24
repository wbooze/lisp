;;;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;; San Marco LISP Explorer (R) -*- mode:lisp -*-
;;;; Copyright (c) 1984, 1985, 1987, 1988, 1989 San Marco Associates

;;; NOTE: Everything needs testing.

(provide 'backprop)

#|
To run this thing, set up a network with MAKEINPUT, MAKEMIDDLE, MAKEOUTPUT,
and MAKELINK as in NETS.DTA.  Then supply data in the form of a list
of pairs of input-output lists, also as in NETS.DTA.  Then run TEST.
|#

;;; Special Variables:

(defvar input-nodes)
(defvar output-nodes)
(defvar all-nodes)
(defvar all-links)
(defvar *rms* 0 "Current benchmark root-mean-square error.")
(defvar *rms-list*)


(defvar *peak-rms-percent* nil "Peak root-mean-square error in percent.")
(defvar *old-rms* nil "Old root-mean-square error.")

(defvar *r* 1.0 "The learning rate.")
(defvar *display* 25 "Display result after this many back propagations.")
(defvar *limit* 1000 "Give up after this many back propagations.")

;;; Macros:

(defmacro rotate (l)
  `(setf ,l (append (rest ,l) (list (first ,l)))))

(defmacro pushend (element place)
  `(setf ,place (append ,place (list ,element))))

;;; Structures and Constructors

(defstruct (node (:print-function print-node))
  (name 'unknown)
  (delta nil)				;Backward propagation term.
  (output nil)				;Actual output.
  (desired nil)				;Desired output.
  (inputs nil)
  (outputs nil))

(defun print-node (node stream &rest ignore)
  (format stream "<~a>" (node-name node)))

(defstruct (link (:print-function print-link))
  (name 'unknown)
  (weight 1)
  (accumulator 0.0)
  (input 'unknown)
  (output 'unknown))
  
(defun print-link (link stream &rest ignore)
  (format stream "<~a>" (link-name link)))

(defmacro makenode (name)
  `(progn (setf ,name (make-node :name ',name))
	  (pushend ,name all-nodes)
	  ,name))

(defmacro makeinput (name)
  `(let ((node (makenode ,name)))
     #+comment
     (setf (node-output ,name) .1) 		  ;Temporary.
     (pushend node input-nodes)
     node))

(defmacro makemiddle (name)
  `(let ((node (makenode ,name)))
     #+comment
     (setf (node-delta ,name) .1) 		  ;Temporary.
     (makethreshold ,name)
     node))

(defmacro makeoutput (name)
  `(let ((node (makenode ,name)))
     #+comment
     (setf (node-delta ,name) .1) 		  ;Temporary.
     (makethreshold ,name)
     (pushend node output-nodes)
     node))

#|
Following has effect of a learnable threshold value because the node's
output is always on.
|#

(defun makethreshold (output
		       &optional (weight (tick))
		       &aux (input (eval (eval `(makenode ,(mygentemp))))))
  (setf (node-output input) -1.0)
  (let* ((name (intern (format nil "LINK-~a-~a" 
			       (node-name input) (node-name output))))
	 (link (make-link :input input
			  :output output
			  :name name
			  :weight weight)))
    (push link (node-inputs output))
    (push link (node-outputs input))
    (pushend link all-links)
    link))

(defmacro makelink (input output &optional (weight (tick)))
  (let ((name (intern (format nil "LINK-~a-~a" input output))))
    `(setf ,name
	   (let ((link (make-link :input ,input
				  :output ,output
				  :name ',name
				  :weight ,weight)))
	     (push link (node-inputs ,output))
	     (push link (node-outputs ,input))
	     (pushend link all-links)
	     link))))
  
;;; Forward Propagation

;(defun exp (n) n)			;Temporary.

(defun forward (actual-inputs)
  "Very similar to BACKWARD."
  (reset-node-outputs all-nodes)
  (inject-actual-inputs actual-inputs)
  (let ((queue all-nodes))
    (do ((queue queue))
	((endp queue))
      (let ((node (pop queue)))
	(when (output-ready-to-compute-p node)
	  (setf (node-output node)
		(threshold (input-sum (node-inputs node))))
	  #+comment
	  (format t "~%Output of ~a is ~a." node (node-output node))
	  (dolist (l (node-outputs node))
	    (pushnew (link-output l) queue)))))))

(defun input-sum (links &aux (sum 0))
  (dolist (l links sum)
    (incf sum (* (link-weight l) (node-output (link-input l))))))

(defun output-ready-to-compute-p (node)
  (when (node-inputs node)
    (let* ((input-links (node-inputs node))
	   (input-nodes (mapcar #'link-input input-links)))
      (dolist (i input-nodes t)
	(unless (node-output i)
	  (return nil))))))

;;; Backward Propagation

(defun increment-weight-accumulators ()
  (dolist (l all-links)
    (let ((i (link-input l))
	  (j (link-output l)))
      (when (and (node-output i) (node-delta j))
	(incf (link-accumulator l)
	      (* *r* (node-output i) (node-delta j)))))))

(defun change-weights ()
  (dolist (l all-links)
    (setf (link-weight l)
	  (+ (link-weight l) (link-accumulator l)))
    (setf (link-accumulator l) 0.0)))

(defun backward (desired-outputs)
  "Very similar to FORWARD."
  (reset-node-deltas all-nodes)
  (inject-desired-outputs desired-outputs)
  (let ((queue all-nodes))
    (do ((queue queue))
	((endp queue))
      (let ((node (pop queue)))
	(when (delta-ready-to-compute-p node)
	  (setf (node-delta node)
		(* (slope node) (delta-sum node)))
	  #+comment
	  (format t "~%Delta for ~a is ~a."
		  node (node-delta node))
	  (dolist (l (node-inputs node))
	    (pushnew (link-input l) queue)))))))

(defun slope (node)
  (let ((n (node-output node)))
    (* n (- 1 n))))

(defun delta-sum (node &aux (result 0))
  (let* ((output-links (node-outputs node))
	 (output-nodes (mapcar #'link-output output-links)))
    (do ((links output-links (rest links))
	 (nodes output-nodes (rest nodes)))
	((endp links) result)
      (incf result (* (link-weight (first links))
		      (node-delta (first nodes)))))))

(defun delta-ready-to-compute-p (node)
  (when (node-outputs node)
    (let* ((output-links (node-outputs node))
	   (output-nodes (mapcar #'link-output output-links)))
      (dolist (i output-nodes t)
	(unless (node-delta i)
	  (return nil))))))

;;; Perform Cycle

(defvar *data*)

(defun step-weights (&optional (pairs *data*) (tests nil)
		     &aux
		     (c 0)		(*rms* 0)
		     (*old-rms* 0)	(*peak-rms-percent* 0))
  (display-links)
  (format t "~%Rate = ~a" *r*)
  (do () ()
    (when (multiple-value-bind (ignore rem) (floor c *display*)
	    (zerop rem))
      (format t "~%Cycle: ~a" c)
      (calculate-rms pairs)
      (setf *peak-rms-percent* 0 *old-rms* 0)
      (when (or (test-weights pairs) (> c *limit*))
	(format t "~%Here are the training results:")
	(test-weights pairs t)
	(when tests
	  (format t "~%Here are the test results:")
	  (test-weights tests t))
	(return 'done)))
    (incf c)
    (dotimes (n (length pairs))
      (let ((actual-inputs (first (first pairs)))
	    (desired-outputs (second (first pairs))))
	(forward actual-inputs)
	(backward desired-outputs)
	(increment-weight-accumulators)
	(rotate pairs)))
    (change-weights)
    (check-peak-rms pairs))
  (display-links)
  (reverse *rms-list*))

(defun display-rms-list ()
  (dolist (l (nreverse *rms-list*) (terpri))
    (format t "~a	" (f2 l))))

(defun check-peak-rms (pairs &optional (error 0))
  (dolist (pair pairs)
    (let ((actual-inputs (first pair))
	  (desired-outputs (second pair)))
      (forward actual-inputs)
      (inject-desired-outputs desired-outputs)
      (dolist (n output-nodes)
	(incf error (expt (- (node-output n) (node-desired n)) 2)))))
  (let* ((new-rms (sqrt (/ error (length pairs) (length output-nodes))))
	 (change nil))
    (when (> *old-rms* 0)
      (setf change (/ (* 100 (- new-rms *old-rms*)) *old-rms*))
      (when (> (abs change)
	       (abs *peak-rms-percent*))
	(setf *peak-rms-percent* change)))
    (setf *old-rms* new-rms)))

(defun calculate-rms (pairs &optional (error 0))
  (dolist (pair pairs)
    (let ((actual-inputs (first pair))
	  (desired-outputs (second pair)))
      (forward actual-inputs)
      (inject-desired-outputs desired-outputs)
      (dolist (n output-nodes)
	(incf error (expt (- (node-output n) (node-desired n)) 2)))))
  (let* ((rms (sqrt (/ error (length pairs) (length output-nodes))))
	 (lpairs (length pairs))
	 (percent (if (zerop *rms*)
		      0
		      (/ (* 100 (- rms *rms*)) *rms*))))
    (push rms *rms-list*)
    (format t " RMS error ~a ~a ~a"
	    (if (not (zerop *rms*))
		(format nil (if (> rms *rms*)
				"INCREASED by ~a% to"
			      "DECREASED by ~a% to")
			(f2 (abs percent)))
	      "is")
	    (f2 rms)
	    (if (zerop percent)
		""
	      (format nil "; peak/average = ~a"
	      (f2 (/ (* *peak-rms-percent* *display*) lpairs percent)))))
      (setf *rms* rms)))

(defun test-weights (pairs &optional switch)
  (catch 'enclosure
    (dolist (pair pairs t)
      (let ((actual-inputs (first pair))
	    (desired-outputs (second pair)))
	(forward actual-inputs)
	(inject-desired-outputs desired-outputs)
	(if switch
	    (dolist (n output-nodes)
	      (format t "~%~aFor ~a, ~
		      node ~a's output is ~a: it's ~a, not ~a"
		      (if (<= (abs (- (node-output n) (node-desired n))) .4)
			  ""
			"!")
		      actual-inputs n
		      (if (<= (abs (- (node-output n) (node-desired n))) .4)
			  "ok"
			"WRONG")
		      (f2 (node-output n)) (node-desired n)))
	  (dolist (n output-nodes)
	      (unless (if (zerop (node-desired n))
			  (< (node-output n) .1)
			(> (node-output n) .9))
		(throw 'enclosure nil))))))))

(defun display-nodes (&optional (nodes all-nodes))
  (dolist (n nodes)
    (format t "~%Node ~a's output is ~a" n (node-output n))))

(defun display-links (&optional (links all-links))
  (dolist (n links)
    (format t "~%Link ~a's weight is ~a" n (link-weight n))))

(defun reset-node-outputs (nodes)
  (dolist (n nodes)
    (when (node-inputs n) (setf (node-output n) nil))))

(defun reset-node-deltas (nodes)
  (dolist (n nodes)
    (setf (node-delta n) nil)
    (setf (node-desired n) nil)))

(defun inject-actual-inputs (actual-inputs)
  (when (< (length actual-inputs) (length input-nodes))
    (error "Not enough inputs!"))
  (mapc #'(lambda (value node) 
	    (setf (node-output node) value))
	actual-inputs
	input-nodes))

(defun inject-desired-outputs (desired-outputs)
  (when (< (length desired-outputs) (length output-nodes))
    (error "Not enough outputs!"))
  (mapc #'(lambda (value node) 
	    (setf (node-desired node) value))
	desired-outputs
	output-nodes)
  (mapc #'(lambda (node)
	    (setf (node-delta node)
		  (* (slope node)
		     (-  (node-desired node) (node-output node)))))
	output-nodes))

;;; Test Function:

(defmacro experiment (output &rest commands)
  `(let ((output-file (string-downcase
			(namestring (make-pathname :name
						   ,output
						   :type "out"
						   :defaults
						   #+gclisp "/phw/ai3/"
						   #-gclisp "om:~phw/")))))
     (with-open-file (output output-file :direction :output)
       (let ((*standard-output*
	      (make-broadcast-stream *standard-output* output)))
       ,@commands))))

;;; GCL Hacks:

(defun reset-generators ()
  (setf *temp* 1)
  (setf *tick* 0.0)
  (setf *rms-list* nil))

(defun mygentemp (&optional (s "t"))
  (prog1 (intern (format nil "~a~a" s *temp*))
	 (incf *temp*)))

(defun tick () (incf *tick* .1))

;;; Squashing function:

(defun threshold (n)
  (/ 1.0 (1+ (exp (- (float n))))))

(defun squash (n) (threshold n))

(defun derivative (n)
  (let ((e (exp (- (float n)))))
    (/ e (expt (1+ e) 2))))

#+comment
(defun test (name)
  (let ((input-file (string-downcase
		      (namestring
			(make-pathname :name name :type "dta"
				       :defaults
				       #+gclisp "/phw/ai3/"
				       #-gclisp "om:~phw/")))))
    (format t"~%~a" name)
    (load input-file)
    (format t "Rate *r* = ~a~a"
	    *r*
	    (if input-file
		(format nil "; input file = ~a" input-file)
	      ""))
     (step-weights)))

