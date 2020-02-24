;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; DYNAMIC VARIABLES

(defparameter *p* 0.8)			;Selection probability.
(defparameter *population* 4)		;Maximum number of chromosomes.
(defparameter *satisfactory-score* 9.0)	;The best possible score.

(defparameter *initial-rule* '(0.0 0.0 0.0 0.0 0.0 0.0)) ;The starting rule.

;;;; TOP-LEVEL FUNCTION

(defun evolve (&optional generations)
  "
  Purpose:	Initiate and control evolution.
  "
  (let ((rules (list *initial-rule*)))
    (do ((generation 0 (1+ generation)))
	((or (when generations (= generation generations))
	     (when (finished-p (first rules))))
	 (display-rules generation rules)
	 (values generation rules))
      ;;Display rules in current generation:
      (display-rules generation rules)
      ;;Mutate all rules:
      (let ((new-rules (mapcar #'mutate-rule rules)))
	(setf rules (remove-duplicates (append new-rules rules)
				       :test #'equal)))
      ;;Breed all rule pairs:
      (when (> (length rules) 1)
	(let ((new-rules nil))
	  (dolist (parent-a rules)
	    (let* ((parent-b
		     (select-good-rule (remove parent-a rules)
				       (list parent-a)))
		   (offspring (crossover parent-a parent-b)))
	      (setf new-rules (append offspring new-rules))))
	  (setf rules (remove-duplicates (append new-rules rules)
					 :test #'equal))))
      ;;Create a new generation:
      (let* ((new-rules nil) (best-rule (first (sort-by-score rules))))
	  (push best-rule new-rules)
	  (setf rules (remove best-rule rules :test #'equal))
	  (dotimes (n (1- *population*))
	    (when (endp rules) (return))
	    (let ((survivor (select-good-rule rules new-rules)))
	      (push (copy-tree survivor) new-rules)
	       (setf rules (remove survivor rules))))
	  (setf rules (reverse new-rules))))))

;;;; OTHER KEY PROCEDURES

;;; Domain-Specific Fitness Measurement

(defun fitness (&rest args)
  "
  Purpose:	Domain-specific fitness measurement.
		Here, COOKIES is the name of a 2D array of
		numbers, persumed to reflect the quality
		of cookies, given the coordinate values,
		which represent sugar and flour.  
  "
  (apply #'aref cookies args)
  )

(defun finished-p (rule) (>= (fitness x) *satisfactory-score*))

;;; Domain-Specific Rule Mutation

(defun mutate-rule (old-rule)
  "
  Purpose:	Pick a gene to mutate.
  Remarks:	Generic.
  "
  (let* ((new-rule (copy-tree old-rule))
	 (target (random (length old-rule)))
	 (old-value (nth target old-rule))
	 (new-value (compute-new-value old-value)))
    (setf (nth target new-rule) new-value)
    (when (>= new-value 1.0)
      (error "mutate rule"))
    new-rule))

(defun compute-new-value (value)
  "
  Purpose:	Produce a new gene.
  Remarks:	Domain specific.
  "
  (let ((result (+ (- (random (* 2.0 *maximum-change*))
		      *maximum-change*)
		   value)))
    (setf result
	  (if (< result 0.0)
	      (- result)
	    (if (> result 1.0)
		(if (= result 1.0)
		    0.9999
		  (- 2.0 result))
	      result)))
    result))

;;; Rule Breeding

(defun crossover (rule1 rule2 &aux child1 child2)
  "
  Purpose:	Crosses a rule pair at a random place;
  Remarks:	Generic; produces two offspring.
  "
  (let* ((l1 (length rule1))
	 (l2 (length rule2))
	 (n  (1+ (random (1- l1)))))
    (when (zerop (random 2)) (psetq rule1 rule2 rule2 rule1))
    (unless (= l1 l2) (error "Cannot cross rules of different length!"))
    (setf child1 (append (butlast rule1 (- l1 n)) (nthcdr n rule2)))
    (setf child2 (append (butlast rule2 (- l1 n)) (nthcdr n rule1)))
    (list child1 child2)))
  
;;; Rule Selection

(defun select-good-rule (rules &optional references &aux result choices)
  (setf result (sort-by-diagonals rules references))
  (values result choices))

(defun select-by-position (rules)
  (dolist (r rules (first (last rules)))
    (when (< (random 1.0) *p*) (return r))))

;;;; AUXILIARY PROCEDURES

;;; Sorting and Scoring Procedures

(defun sort-by-diagonals (rules references &aux test winner)
  "
  Remarks:	This is where the rank space computation is done.
  "
  (let* (score-ordered-rules distance-ordered-rules)
    (setf distance-ordered-rules
	  (reverse (sort-by-distances rules references)))
    (setf score-ordered-rules (reverse (sort-by-score rules)))
    (setf test
	  (sort (mapcar #'(lambda (r)
			      (let ((d (position r distance-ordered-rules))
				    (s (position r score-ordered-rules)))
				(list (+ d s) d s r)))
			  distance-ordered-rules)
		#'(lambda (x y)
		    (or (> (first x) (first y))
			(and (= (first x) (first y))
			     (> (second x) (second y)))))))
    (setf winner (select-by-position (mapcar #'fourth test)))
    (display-rank score-ordered-rules distance-ordered-rules winner)
    (values winner (mapcar #'first test))))

(defun sort-by-score (rules)
  "
  Purpose:	Ranks by quality score.
  Remarks:	Highest score in front.
  "
  (mapcar #'second
	  (sort (mapcar #'(lambda (r) (list (fitness r) r)) rules)
		#'(lambda (x y) (> (first x) (first y))))))

(defun sort-by-distances (vectors references)
  "
  Purpose:	Ranks by quality score.
  Remarks:	Highest diversity score in front.
  "
  (let ((results
	  (sort
	    (mapcar
	      #'(lambda (v)
		  (list (sum-of-inverse-distance2 v references) v))
	      vectors)
	    #'(lambda (x y) (< (first x) (first y))))))
    (values (mapcar #'second results) (mapcar #'first results))))

;;; Distance Measurement

(defun sum-of-inverse-distance2 (vector references)
  "
  Purpose:	Computes sum of inverses of squared distances.
  Remarks:	Big is bad.
  "
  (apply #'+ (mapcar #'(lambda (r) (inverse-distance2 vector r)) references)))

(defun inverse-distance2 (r1 r2)
  "
  Purpose:	Computes inverse of squared distance.
  Remarks:	Big is bad.
  "
  (/ 1.0
     (apply #'+ (mapcar #'(lambda (x y) (let ((d (- x y))) (* d d))) r1 r2))))

;;; Random Selection

(defun randomth (l)
  "
  Purpose:	Select random element from a list.
  "
  (nth (random (length l)) l))

(defun weighted-randomth (weights)
  "
  Purpose:	Select weighted random element from a list.
  "
  (if (rest weights)
      (let ((total (apply #'+ weights)))
	(if (zerop total) 
	    (random (length weights))
	  (let ((threshold (random (float total))))
	    (do* ((weights weights (rest weights))
		  (sum (first weights) (+ sum (first weights)))
		  (position 0 (1+ position)))
		 ((> sum threshold) position)))))
    0))

;;;; DISPLAY

(defun display-hills ()
  (format t "~%Hills to be displayed here."))

(defun display-rank (score-rules diversity-rules &optional winner)
  (format t "~%Rank space to be displayed here."))

(defun display-rules (n rules)
  (format t "~%Rules to be displayed here."))


