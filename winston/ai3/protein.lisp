;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

#|

General description:

The purpose of this program is to discover a regularity in a
batch of ``proteins'' using a genetic algorithm.  Both positive
and negative examples are used.

A ``gene'' is a pattern.  A gene is expressed as a list whose
elements are drawn from the twenty single-character atoms,
corresponding to the twenty amino acids, plus a *, which is the
don't-care symbol.  A ``protein'' is a list whose elements are
drawn from the twenty single-character atoms.

Use is via EVOLVE, a function of no arguments.  It works on
a set of positive examples, bound to *positives*, and negative
examples, bound to *negatives*.  It also makes use of an initial
gene, bound to *genes*, and a number limiting maximum population
size, bound to *population*.

See the PROTEIN.TST file for example use.

|#

(provide 'protein)

;;;; SPECIAL VARIABLES

(defvar *genes*)		;The initial pattern.
(defvar *positives*)	;The positive examples
(defvar *negatives*)    ;The negative examples
(defvar *insertion*)    ;The inserted sequence common to all positives.
			;This is the thing you want to match the ``gene.''

(defvar *p*)		;The probability that the most fit is selected next.
(defvar *population*)	;The number of individuals.
(defvar *scoring-method*) ;Either standard, rank, rank-space.

(defvar *communication*)  ;If nil, turn a lot of output off.
(defvar *pause*)	  ;If an integer, pause after each *pause*th
			  ;generation, then clear screen.
(defvar *stop-on-crossover?*) ;If t, stop on favorable crossover for view.
(defvar *stop-when-perfect?*) ;If t, stop when optimum found.

;;; New in Proteins

(defvar *select-random-mate*)	;No search for quality or diversity if T.
(defvar *extinction*)		;No limit if NIL.

;;; Set Parameters

(setf *p*				.66
      *population*			10
      *scoring-method*			'rank-space

      *communication*			nil	;*standard-output*
      *pause*				nil
      *stop-on-crossover?*		nil
      *stop-when-perfect?*		t

      *select-random-mate*		t
      *extinction*			nil
      )

;;;; HISTORY-KEEPING AUXILIARIES

(defstruct state
  extinction
  genesis
  rules)

(defun write-state ()
  (format t "~%Writing state...")
  (with-open-file (output "/phw/ai3/protein.sts" :direction :output)
    (print *evolution-state* output)))

(defun read-state ()
  (with-open-file (input "/phw/ai3/protein.sts")
    (setf *evolution-state* (read input)))
  *evolution-state*)

;;;; MAIN PROCEDURE

(defun evolve (&optional *evolution-state*)
  (declare (special *evolution-state*))
  (send *terminal-io* :clear-screen)
  (let ((rules) (genesis) (extinction))
    (unless (state-p *evolution-state*)
      (setf *evolution-state*
	    (make-state
	      :extinction *extinction*
	      :genesis 0
	      :rules (if (listp (first *genes*)) *genes* (list *genes*)))))
    (setf extinction (state-extinction *evolution-state*))
    (setf genesis (state-genesis *evolution-state*))
    (setf rules (state-rules *evolution-state*))
    (format *communication* "~&Experimental parameters:~
		 ~%*p* = ~a~
		 ~%*population* = ~a~
		 ~%*scoring-method* = ~a~
		 "
	    *p* *population* *scoring-method*)
    (do ((generation genesis (1+ generation)) (old-rules rules rules))
	((or (eql generation extinction)
	     (when *stop-when-perfect?* (perfect-p (first rules))))
	 (display-fitness generation rules)
	 (format t "~%Completed in ~a generations." generation)
	 #+comment
	 (send *terminal-io* :set-cursorpos 0 0)
	 #+comment
	 (values generation rules)
	 (values))
      (setf (state-genesis *evolution-state*) generation)
      (setf (state-rules *evolution-state*) rules)
      (setf rules (sort-by-score rules))
      (write-state)
      (display-fitness generation rules)
      ;;Pause for a look:
      (when *pause*
	(multiple-value-bind (ignore r) (floor generation *pause*)
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
	(setf rules (union rules new-rules :test #'equal)))
      (setf rules (sort-by-score rules))
      ;;Breed all rule pairs:
      (when (> (length old-rules) 1)
	(let ((new-rules nil))
	  (dolist (parent-a old-rules)
	    (let* ((parent-b (select-mate old-rules parent-a))
		   (offspring (crossover parent-a parent-b)))
	      (report-crossover parent-a parent-b offspring)
	      (setf new-rules (append offspring new-rules))))
	  (setf new-rules
		(set-difference (remove-duplicates new-rules :test #'equal)
				old-rules
				:test #'equal))
	  (print-rules new-rules)
	  (setf rules (union rules new-rules :test #'equal))))
      (setf rules (sort-by-score rules))
      ;;Create new generation:
      (let* ((new-rules nil) (best-rule nil))
	(setf best-rule (first rules))
	(print-selected-rule best-rule)
	(push best-rule new-rules)
	(setf rules (remove best-rule rules))
	(dotimes (n (1- *population*))
	  (when (endp rules) (return))
	  (multiple-value-bind (survivor choices)
	      (select-good-rule rules new-rules)
	    (print-selected-rule survivor choices)
	    (push (copy-tree survivor) new-rules)
	    (setf rules (remove survivor rules))))
	(print-rules (setf new-rules (reverse new-rules)))
	(setf rules new-rules)))))

;;;; APPLICATION-INDEPENDANT PROCEDURES

;;; Rule Breeding

(defun crossover (rule1 rule2 &aux child1 child2)
  "Crosses a rule pair at a random place;
   Initial part of new rule can come from either of the old rules."
  (let* ((l1 (length rule1))
	 (l2 (length rule2))
	 (n  (1+ (random (1- l1)))))
    (when (zerop (random 2)) (psetq rule1 rule2 rule2 rule1))
    (unless (= l1 l2) (error "Cannot cross rules of different length!"))
    (setf child1 (append (butlast rule1 (- l1 n)) (nthcdr n rule2)))
    (setf child2 (append (butlast rule2 (- l1 n)) (nthcdr n rule1)))
    (list child1 child2)))
  
;;; Mating Procedure (new for proteins)

(defun select-mate (old-rules parent-a)
  (if *select-random-mate*
      (randomth old-rules)
  (select-good-rule (remove parent-a old-rules) (list parent-a))))

;;; Selection Procedures

(defun select-good-rule (rules &optional references &aux result choices)
  (case *scoring-method*
    (standard (setf choices rules)
	      (setf result
		    (nth (weighted-randomth (mapcar #'score-pattern rules))
			 rules)))
    (rank (setf choices rules)
	  (setf result (select-by-position choices)))
    (rank-space (setf choices (sort-by-diagonals rules references))
		(setf result (select-by-position choices))))
  (values result choices))

(defun select-by-position (rules)
  (dolist (r rules (first (last rules)))
    (when (< (random 1.0) *p*) (return r))))

;;; Sorting Procedures

(defun sort-by-diagonals (rules references &aux test)
  ;;This one sorts by backslash diagonals.
  ;;Assumes rules are sorted by fitness
  (let* (score-ordered-rules distance-ordered-rules)
    (setf distance-ordered-rules (reverse (sort-by-ssd rules references)))
    (setf score-ordered-rules (reverse rules #+comment (sort-by-score rules)))
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
    (values (mapcar #'fourth test) (mapcar #'first test))))

(defun sort-by-score (rules)
  (format t ".")
  (mapcar #'second
	  (sort (mapcar #'(lambda (r) (list (score-pattern r) r)) rules)
		#'(lambda (x y) (> (first x) (first y))))))

(defun sort-by-ssd (vectors references)
  (format t ":")
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

;;;; APPLICATION SPECIFIC PROCEDURES

;;; The Fitness Function (application specific)

(defun score-pattern (pattern) (fitness pattern *positives* *negatives*))

(defun fitness (pattern positives negatives)
  (let ((p-switch t) (n-switch nil) (score nil)
	(max-score (compute-max-score pattern)))
  (values
    (- (if positives
	   (let ((sum 0))
	     (dolist (p positives) 
	       (setf score (match pattern p))
	       (unless (= score max-score) (setf p-switch nil))
	       (incf sum (match pattern p)))
	     (/ sum (length positives)))
	 0)
       (if negatives
	   (let ((sum 0))
	     (dolist (n negatives) 
	       (setf score (match pattern n))
	       (when (= score max-score) (setf n-switch t))
	       (incf sum score))
	     (/ sum (length negatives)))
	 0))
    (and p-switch (not n-switch)))))

(defun match (pattern data &aux result)
  (dotimes (n (1+ (- (length data) (length pattern))))
    (unless (zerop n) (pop data))
    (push (reduce #'+
		  (mapcar #'(lambda (p d) (if (eq '* p) 1 (if (eq p d) 2 0)))
			  pattern data))
	  result))
  (reduce #'max result))

(defun compute-max-score (pattern &aux (result 0))
  (dolist (p pattern result)
    (if (eq p '*) (incf result) (incf result 2))))

(defun perfect-p (pattern)
  (multiple-value-bind (ignore result)
      (fitness pattern *positives* *negatives*)
    result))

;;; Data Constructors (application specific)

(defun make-random-pattern (pattern-length &aux result)
  (dotimes (n pattern-length result)
    (push (randomth (cons '* *amino-acids*)) result)))

(defun make-negatives (count length &aux instance result)
  (dotimes (c count result)
    (setf instance nil)
    (dotimes (l length instance)
      (push (randomth *amino-acids*) instance))
    (push instance result)))

(defun make-insertion (length) (first (make-negatives 1 length)))

(defun make-positives (count length insertion)
  (mapcar #'(lambda (sequence) (insert insertion sequence))
	  (make-negatives count length)))

(defun insert (insertion protein &aux start)
  (setf protein (copy-list protein))
  (setf start (random (1+ (- (length protein) (length insertion)))))
  (dotimes (n (length insertion) protein)
    (setf (nth (+ n start) protein)
	  (nth n insertion))))

;;; Rule Mutation (application specific)

(defvar *amino-acids* '(a c d e f g h i k l m n p q r s t v w y))

(defun mutate-rule (old-rule &aux new-rule)
  (setf new-rule (copy-list old-rule))
  (setf (nth (random (length new-rule)) new-rule)
	(randomth (cons '* *amino-acids*)))
  new-rule)

;;; Rule Distances (application specific)

(defun rule-distance2 (r1 r2)
  "Computes inverse of squared distance."
  (/ 1.0 (let ((d (rule-distance r1 r2))) (* d d))))

(defun rule-distance (r1 r2)
  (reduce #'+ (mapcar #'(lambda (x y) (if (eq x y) 0 1)) r1 r2)))

;;;; PRINTING PROCEDURES (application specific, few actually used)

(defun display-fitness (generation rules &aux (string ""))
  (format t "~%Generation: ~a	Active Rules: ~a	Fitness: ~a	~%"
	  generation
	  (length rules)
	  (fitness (first rules) *positives* *negatives*))
  (setf string
	(format nil "~a"
	  (dolist (rule (cons *insertion* rules) string)
	    (setf string
		  (concatenate 'string
			       string
			       (make-rule-string rule *insertion*)
			       " ")))))
  (dotimes (n (min 75 (length string)) (format t "..."))
    (format t "~c" (aref string n))))

(defun make-rule-string (current target &aux (result ""))
  (dotimes (n (length current) result)
  (setf result
	(concatenate 'string result
		     (if (eq (nth n current) (nth n target))
			 (format nil "~a" (nth n current))
		       (string-downcase (format nil "~a" (nth n current))))))))

(defun print-rules (rules)
  (when *communication*
    (let* ((acquired (reverse rules))
	   (acquired-scores (mapcar #'score-pattern acquired))
	   (scored (sort-by-score rules))
	   (scored-scores (mapcar #'score-pattern scored)))
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
  (when *communication*
    (format *communication* "~%Selected rule ")
    (print-list-elements rule)
    (when choices
      (format *communication* "from ")
      (dolist (c choices)
	(print-list-elements c)
	(format *communication* "- ")))))

(defun print-list-elements (list)
  (when *communication*
    (dolist (m list)
      (format *communication* "~a " m))))

(defun report-crossover (a b new)
  (when *communication*
    (when (> (max (score-pattern (first new))
		  (score-pattern (second new)))
	     (max (score-pattern a) (score-pattern b)))
      (format *communication* "~%Valuable crossover!")
      (when *stop-on-crossover?*
	(format t "~%Stopped for crossover!")
	(read-char)))))

;;;; AUXILIARY PROCEDURES

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
