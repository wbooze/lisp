;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;


;;; The F2 function:

(defun f1 (x)
  (print (* 2 (expt (/ (- x .1) .8) 2)))
  (* (expt (sin (* 5 pi x)) 6)
     (exp (* -2 (log (* 2 (expt (/ (- x .1) .8) 2)))))))

(defun f5 (x) (* (expt (sin (* 5 pi x)) 6) (exp (- x))))

(defun f10a (x) (* (expt (sin (* 10 pi x)) 6) (exp (- x))))

(defun f10b (x) (if (< x .5)
		  (* (expt (sin (* 10 pi x)) 6) (exp (- x)))
		(* (expt (sin (* 10 pi x)) 6) (exp (- x 1)))))
    
(defmacro deffitness (name)
  `(setf (symbol-function 'fitness) (symbol-function ,name)))

(defun display-fitness (&optional n &rest marks)
  (dotimes (n 79)
    (send *terminal-io*
	  :set-cursorpos
	  n
	  (- 24 (floor (* 26 (fitness (/ (float n) 80.0))))))
    (dolist (mark marks (format *terminal-io* "+"))
      (when (and (< (/ (- n .5)  80.0) mark)
		 (<= mark (/ (+ n .5) 80.0)))
	(format *terminal-io* "~c" #xdb)
	(return))))
  (make-frame (format nil "Generation: ~a" n))
  (values))

(defun make-frame (&rest strings)
  "This takes arguments a little differently;
   x and y are obvious; rest are strings, one per line; use multiple
   (format nil ...) commands to create multiple lines)"
  (setf strings (trim-strings strings))
  (let* ((height (length strings))
	 (width (reduce #'max (mapcar #'length strings)))
	 (x 50)
	 (y 3)
	 (border-window (make-window-stream :page *window-page*
				    :top y
				    :left x
				    :height (+ 2 height)
				    :width 20));(+ 5 width)
	 (text-window (make-window-stream :page *window-page*
					  :top (1+ y)
					  :left (+ 2 x)
					  :height height
					  :width (1+ width)))
	 (top (make-string (+ 2 width) :initial-element #\Ä))
	 (bottom (make-string (+ 2 width) :initial-element #\Í))
	 (spaces (make-string (+ 2 width) :initial-element #\space)))
    (format border-window "~&Ö~a¿" top)
    (dotimes (n height) (format border-window "~&º~a³" spaces))
    (format border-window "~&È~a¾" bottom)
    (dolist (l strings) (format text-window "~&~a" l))
    (values text-window border-window)))

#|

General description:

A rule is a ``genes.''  A gene is expressed as a pair of
array-reference numbers.

Main use is via EVOLVE, which takes an array that is to be
searched for the biggest number.

(evolve array)

See the EVOLVE.TST file for example use.

|#

(proclaim '(optimize (speed 3) (safety 0)))

(provide 'evolve)

;;;; SPECIAL VARIABLES

(defvar *haploid-rule* '(5 0 0))	 ;The initial haploid rule.
(defvar *perfect-score* 1.0) ;The sought-after, largest number in the array.

(defvar *p*) ;The probability that the most fit in a list will be selected.
(defvar *population*) ;The number of individuals.

(defvar *scoring-method*) ; standard, rank, rank-space
(defvar *cross-at-ends?*) ;If T, then crossover at end is allowed.
(defvar *communication*) ;If nil, turns a lot of output off.
(defvar *pause*) ;If an integer, pause after each *pause*th generation;
		 ;then clear screen.
(defvar *breed?*) ;If t, do crossover.
(defvar *remove-survivors?*) ;If t, remove survivors when forming generation.
(defvar *stop-on-crossover?*) ;If t, stops on favorable crossover for view.
(defvar *stop-when-perfect?*) ;If t, stops when optimum found.

(setf *p*				.66
      *population*			9
      *scoring-method*			'rank-space
      *breed?*				t
     
      *remove-survivors?*		t	;Permanent
      *cross-at-ends?*			nil	;Permanent

      *communication*			nil ;*standard-output*
      *stop-on-crossover?*		nil
      *stop-when-perfect?*		t
      *pause*				nil
)

;;;; USER-LEVEL PROCEDURES

(defun evolve (&optional generations)
  (send *terminal-io* :clear-screen)
  (let ((rules (list *haploid-rule*)))
    (format *communication* "~&Experimental parameters:~
		 ~%*p* = ~a~
		 ~%*population* = ~a~
		 ~%*scoring-method* = ~a~
		 ~%*breed?* = ~a~
		 "
	    *p* *population* *scoring-method* *breed?*)
    (do ((generation 0 (1+ generation)) (old-rules rules rules))
	((or (when generations (= generation generations))
	     (when *stop-when-perfect?*
	       (perfect-p (apply #'max (mapcar #'score-rule rules)))))
	 (apply #'display-fitness generation (mapcar #'rule-to-number rules))
	 (format *communication* "~%Completed in ~a generations:" generation)
	 (send *terminal-io* :set-cursorpos 0 0)
	 #+comment
	 (values generation rules)
	 (values))
      (apply #'display-fitness generation (mapcar #'rule-to-number rules))
      ;;Pause for a look:
      (when *pause*
	(multiple-value-bind (i r) (floor generation *pause*)
	  (when (zerop r)
	    (format *communication* "~%Generation ~a:" generation)
	    (format *communication* "~&~%Press any key to go on.")
	    (read-char *terminal-io*))))
      (print-rules rules)
      ;;Mutate all rules:
      (let ((new-rules (mapcar #'mutate-rule old-rules)))
	(setf new-rules
	      (set-difference
		(remove-duplicates new-rules :test #'equal)
		old-rules
		:test #'equal))
	(print-rules new-rules)
	(setf rules (union new-rules rules :test #'equal)))
      ;;Breed all rule pairs:
      (when (and *breed?* (> (length old-rules) 1))
	(let ((new-rules nil))
	  (dolist (parent-a old-rules)
	    (let* ((parent-b
		     (select-good-rule (remove parent-a old-rules)
				       (list parent-a)))
		   (offspring (crossover parent-a parent-b)))
	      (report-crossover parent-a parent-b offspring)
	      (setf new-rules (append offspring new-rules))))
	  (setf new-rules
		(set-difference (remove-duplicates new-rules :test #'equal)
				old-rules
				:test #'equal))
	  (print-rules new-rules)
	  (setf rules (union new-rules rules :test #'equal))))
      ;;Create new generation:
      (let* ((new-rules nil) (best-rule (first (sort-by-score rules))))
	(print-selected-rule best-rule)
	(push best-rule new-rules)
	(when *remove-survivors?* (setf rules (remove best-rule rules)))
	(dotimes (n (1- *population*))
	  (when (endp rules) (return))
	  (multiple-value-bind (survivor choices)
	      (select-good-rule rules new-rules)
	    (print-selected-rule survivor choices)
	    (push (copy-tree survivor) new-rules)
	    (when *remove-survivors?* (setf rules (remove survivor rules)))))
	(print-rules new-rules)
	(setf rules new-rules)))))

;;;; ACCESS PROCEDURES

(defun score-rule (rule) (fitness (rule-to-number rule)))

(defun rule-to-number (rule)
  (+ (* .1 (first rule)) (* .01 (second rule)) (* .001 (third rule))))

(defun number-to-rule (number)
  (setf number (zero-to-one number))
  (let* ((a (floor (* 10 number)))
	 (10a (* a 10))
	 (100a (* a 100))
	 (b (round (- (floor (* 100 number)) 10a)))
	 (10b (* b 10))
	 (c (round (- (floor (* 1000 number)) 10b 100a))))
    (list a b c)))

(defun zero-to-one (n)
  (if (> n 1)
      1.0
    (if (< n 0)
	0.0
      n)))

(defun perfect-p (score) (> score (* .99 *perfect-score*)))

(defun array-max (array)
  (let ((a (make-array (array-total-size array)
		       :element-type (array-element-type array)
		       :displaced-to array)))
    (reduce #'max a)))

(defmacro array-max-expander (dimensions)
  ;;Do not try to understand how this works; the author doesn't know.
  ;;It was written by a kind of blind trial-and-error, appropriately enough.
  `'(dotimes (n ,(first dimensions) (setf (first shrinking-args) 0))
       (setf (first shrinking-args) n)
       ,(if (rest dimensions)
	     `(let ((shrinking-args (rest shrinking-args)))
		,(eval `(array-max-expander ,(rest dimensions))))
	   `(when (> (funcall #'aref array ,@args) max)
	      (setf max (funcall #'aref array ,@args))))))

;;;; OTHER KEY PROCEDURES

;;; Rule Combination

(defun combine-rules (new old)
  (dolist (rule new old)
    (pushnew rule old :test #'equal)))

;;; Rule Mutation

(defun mutate-rule (old-rule)
  (let ((delta (- (random .2) .1)))
    (number-to-rule (zero-to-one (+ delta (rule-to-number old-rule))))))

;;; Rule Breeding

(defun crossover (rule1 rule2 &aux child1 child2)
  "Crosses a rule pair at a random place;
   Initial part of new rule can come from either of the old rules."
  (let* ((l1 (length rule1))
	 (l2 (length rule2))
	 (n (if *cross-at-ends?* (random (1+ l1)) (1+ (random (1- l1))))))
    (when (zerop (random 2)) (psetq rule1 rule2 rule2 rule1))
    (unless (= l1 l2) (error "Cannot cross rules of different length!"))
    (setf child1 (append (butlast rule1 (- l1 n)) (nthcdr n rule2)))
    (setf child2 (append (butlast rule2 (- l1 n)) (nthcdr n rule1)))
    (print-crossover rule1 rule2 n child1 child2)
    (list child1 child2)))
  
;;; Selection Procedures

(defun select-good-rule (rules &optional references &aux result choices)
  (case *scoring-method*
    (standard (setf choices (sort-by-score rules))
	      (setf result (nth (weighted-randomth (mapcar #'score-rule rules))
				rules)))
    (rank (setf choices (sort-by-score rules))
	   (setf result (select-by-position choices)))
    (rank-space (setf choices (sort-by-diagonals rules references))
		(setf result (select-by-position choices))))
  (values result choices))

#+comment
(defun select-by-position (rules)
  (labels ((make-position-list (list &aux result)
	     (dotimes (n (length list) result)
	       (push n result))))
    (let* ((scores (make-position-list rules))
	   (max-score (apply 'max scores))
	   (min-score (apply 'min scores))
	   (adjustment (/ (- (* *p* min-score) max-score) (1- *p*)))
	   (adjusted-scores
	     (mapcar #'(lambda (score) (- score adjustment)) scores)))
      (nth (weighted-randomth adjusted-scores) rules))))

#+comment
(defun select-by-position (rules)
  (let ((ratio+ (1+ *p*)))
    (dolist (r rules (first (last rules)))
      (when (< (random ratio+) *p*) (return r)))))

(defun select-by-position (rules)
  (dolist (r rules (first (last rules)))
    (when (< (random 1.0) *p*) (return r))))

;;;; AUXILIARY PROCEDURES

;;; Sorting Procedures

(defun sort-by-diagonals (rules references &aux test result)
  ;;This one sorts by backslash diagonals.
  (let* (score-ordered-rules distance-ordered-rules)
    (setf distance-ordered-rules (reverse (sort-by-ssd rules references)))
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
    #+comment
    (progn
      (print (offset distance-ordered-rules 1))
      (print (offset score-ordered-rules 1))
      (offset test 1))
    (values (mapcar #'fourth test) (mapcar #'first test))))

(defun sort-by-score (rules)
  (mapcar #'second
	  (sort (mapcar #'(lambda (r) (list (score-rule r) r)) rules)
		#'(lambda (x y) (> (first x) (first y))))))

(defun sort-by-ssd (vectors references)
  (let ((results
	  (sort
	    (mapcar
	      #'(lambda (v) (list (sum-of-squared-distances v references) v))
	      vectors)
	    #'(lambda (x y) (< (first x) (first y))))))
    (values (mapcar #'second results)
	    (mapcar #'first results))))

(defun sum-of-squared-distances (vector references)
  "Computes sum of inverses of squared distances."
  (apply #'+ (mapcar #'(lambda (r) (rule-distance2 vector r)) references)))

(defun rule-distance2 (r1 r2)
  "Computes inverse of squared distance."
  (/ 1.0 (let ((d (- (rule-to-number r1) (rule-to-number r2)))) (* d d))))

;;; Printing Procedures

(defun display-rules (rules) (mapcar #'rule-to-number rules))

(defun print-rules (rules)
  (when *communication*
    (let* ((acquired (reverse rules))
	   (acquired-scores (mapcar #'score-rule acquired))
	   (scored (sort-by-score rules))
	   (scored-scores (mapcar #'score-rule scored)))
      (when rules
	(format *communication* "~&Ranked	Score	Acqrd	Score")
	(mapcar #'(lambda (w x y z)
		    (format *communication* "~%")
		    (print-list-elements w)
		    #+:common-lisp
		    (format *communication* "	~,1f	" x)
		    #-:common-lisp
		    (format *communication* "	~a	" (tilde-f x nil 2))
		    (print-list-elements y)
		    (format *communication* "	")
		    #+:common-lisp
		    (format *communication* "~,1f	" z)
		    #-:common-lisp
		    (format *communication* "~a	" (tilde-f z nil 2)))
	      scored scored-scores acquired acquired-scores))
      (unless rules
	(format *communication* "~&No new rules in this group!")))))

(defun print-selected-rule (rule &optional choices)
  (format *communication* "~%Selected rule ")
  (print-list-elements rule)
  (when choices
    (format *communication* "from ")
    (dolist (c choices)
      (print-list-elements c)
      (format *communication* "- "))))

(defun print-list-elements (list)
  (dolist (m (offset list 0))
    (format *communication* "~a " m)))

(defun print-mutation (old-rule rule)
  (format *communication* "~&Mutation: ")
  (print-list-elements old-rule)
  (format *communication* "--> ")
  (print-list-elements rule))

(defun print-worst-rule (rule)
  (format *communication* "~&Replaced rule: ")
  (print-list-elements rule))

(defun print-crossover (r1 r2 at c1 c2)
  (format *communication* "~&Crossover: ")
  (print-list-elements r1)
  (format *communication* "x ")
  (print-list-elements r2)
  (format *communication* "--> ")
  (print-list-elements c1)
  (format *communication* ", ")
  (print-list-elements c2))

(defun report-crossover (a b new)
  (when (> (max (score-rule (first new))
		     (score-rule (second new)))
		(max (score-rule a) (score-rule b)))
    (format *communication* "~%Valuable crossover!")
    (when *stop-on-crossover?*
      (format t "~%Stopped for crossover!")
      (read-char))))

;;; Other Auxiliary Procedures

(defun randomth (l)
  "Selects random element from a list."
  (nth (random (length l)) l))

(defun weighted-randomth (weights)
  "Returns position of random weight from a list,
   with probability proportional to weight."
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


;;; Test Procedures

(defun rank-probabilities (r n &optional (remaining 1.0))
  (if (zerop (1- n))
      (list remaining)
    (cons (* r remaining)
	  (rank-probabilities r (1- n) (- remaining (* r remaining))))))

(defun offset (s n)
  (cond ((null s) nil)
	((atom s) (if (numberp s) (incf s n) s))
	(t (cons (offset (first s) n) (offset (rest s) n)))))

(defun show (a b &optional (offset-switch t))
  (when offset-switch (setf a (offset a -1) b (offset b -1)))
  (multiple-value-bind (x y) (sort-by-ssd a b)
    (format t "~%Distance Sums:	~a" y)
    (format t "~%Vectors:	~a" (offset x 1)))
  (multiple-value-bind (x y) (sort-by-diagonals a b)
    (format t "~%Rank sums:	~a" y)
    (format t "~%Vectors:	~a" (offset x 1)))
  (if (rest a)
      (show (rest (sort-by-diagonals a b))
	    (cons (first (sort-by-diagonals a b)) b)
	    nil)
    (offset (reverse (cons (first a) b)) 1)))