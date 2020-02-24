;;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; BACKGROUND STUFF

;;;; The material in this file is drawn from copyrighted programs
;;;; in LISP, Third Edition, by Patrick H. Winston and Berthold
;;;; K. P. Horn, and in the San Marco LISP Explorer (R).

#|
This portion of the file contains an interpreter for a
transition-tree natural language interface.
|#

;;;; Be sure Common Lisp is added to GCLisp

(cl)

;;;; Tree Definition and Interpretation

(defmacro define-tree args
  (let ((name-of-tree (first args))
	(tree-description (second args)))
    `(defun ,name-of-tree (word-list)
       (interpret-tree ',tree-description word-list))))

(defun interpret-tree (tree word-list &optional a-list)
  (cond ((endp tree) (values t nil word-list))
        ((eq (first tree) (first word-list))
         (interpret-tree (rest tree) (rest word-list) a-list))
        ((eq '> (first tree))
         (multiple-value-bind
             (result binding word-list)
             (funcall (second tree) word-list)
           (when result
             (interpret-tree (rest (rest tree))
                            word-list
                            (cons (list (second tree) binding)
                                  a-list)))))
        ((eq 'rtn (first tree))
         (values t
                 (evaluate-forms (rest tree) a-list)
                 word-list))
        ((and (eq 'if-end-rtn (first tree))
              (endp word-list))
         (values t (evaluate-forms (rest tree) a-list) nil))
        ((eq 'brnchs (first tree))
         (interpret-branches (rest tree) word-list a-list))
        (t (values nil nil word-list))))

(defun interpret-branches (branches word-list a-list)
  (if (endp branches)
      (values nil nil word-list)
      (multiple-value-bind (result binding words-left-over)
          (interpret-tree (first branches) word-list a-list)
        (if result
            (values result binding words-left-over)
            (interpret-branches (rest branches)
                                word-list
                                a-list)))))

;;;; Auxiliaries

(defun evaluate-forms (forms a-list)
  (eval `(let ,(make-let-variables a-list)
              ,@forms)))

(defun make-let-variables (a-list)
  (mapcar #'(lambda (pair)
              `(,(first pair) ',(second pair))) a-list))


;;;; Top-Level Interface

(defun run-interface ()
  (print '>)
  (do ((input (read-sentence) (read-sentence)))
      ((endp input)
       (format t "~&Ok---goodbye.")
       (values))
    (unless (question-or-command input)
      (format t "~&Sorry---I can't understand that.~
              ~&Press the return key if you want to stop."))
    (print '>)))

(defun read-sentence ()
  (with-input-from-string
    (input (string-trim ".?!" (read-line)))
    (do ((word (read input nil)
               (read input nil))
         (sentence nil))
        ((not word) (return (reverse sentence)))
      (push word sentence))))

#|
This portion of the file contains test data forms for the natural
language interpreter.
|#

;;;; Transition-Tree Grammar

(define-tree question-or-command
  (brnchs
    (count > objects if-end-rtn
       (db-call `(db-count ,objects)))
    (how many > objects are there if-end-rtn
       (db-call `(db-count ,objects)))
    (> enumerate > objects if-end-rtn
       (db-call `(db-show ,objects)))
    (> present the > attributes of > objects if-end-rtn
       (db-call
         `(db-show
            (db-project ,objects over ,@attributes))))))

(define-tree attributes
  (brnchs (> attribute > attributes rtn (cons attribute
                                              attributes))
          (and > attribute          rtn (list attribute))
          (> attribute              rtn (list attribute))))

(define-tree attribute
  (brnchs (class        rtn 'class)
          (classes      rtn 'class)	;Added.
          (color        rtn 'color)
          (colors       rtn 'color)	;Added.
          (size         rtn 'size)
          (sizes        rtn 'size)	;Added.
          (weight       rtn 'weight)
          (weights      rtn 'weight)	;Added.
          (location     rtn 'location)
          (locations    rtn 'location)));Added.

(define-tree attribute-values
  (brnchs (> attribute-value > attribute-values
             rtn (append attribute-value attribute-values))
          (and > attribute-value rtn attribute-value)
          (> attribute-value rtn attribute-value)))

(define-tree attribute-value
  (brnchs (large        rtn '(size eql large))
          (medium       rtn '(size eql medium))
          (small        rtn '(size eql small))
          (long         rtn '(size eql long))
          (short        rtn '(size eql short))
          (black        rtn '(color eql black))
          (blue         rtn '(color eql blue))
          (red          rtn '(color eql red))
          (yellow       rtn '(color eql yellow))
          (gray         rtn '(color eql gray))
          (heavy        rtn '(weight eql heavy))
          (light        rtn '(weight eql light))))

(define-tree objects
  (brnchs (> determiner > objects rtn objects)
          (> attribute-values > object
             rtn `(db-select ,object with ,@attribute-values))
          (> object rtn object)))

(define-tree determiner
  (brnchs (a) (the)))

(define-tree object
  (brnchs (saw          rtn 'saws)
          (saws         rtn 'saws)
          (hammer       rtn 'hammers)
          (hammers      rtn 'hammers)
          (wrench       rtn 'wrenches)
          (wrenches     rtn 'wrenches)
          (screwdriver  rtn 'screwdrivers)
          (screwdrivers rtn 'screwdrivers)))

(define-tree object			;Redone.
  (brnchs (tool		rtn 'tools)
	  (tools	rtn 'tools)
	  (saw          rtn '(db-select tools with class eql saw))
          (saws         rtn '(db-select tools with class eql saw))
          (hammer       rtn '(db-select tools with class eql hammer))
          (hammers      rtn '(db-select tools with class eql hammer))
          (wrench       rtn '(db-select tools with class eql wrench))
          (wrenches     rtn '(db-select tools with class eql wrench))
          (screwdriver  rtn '(db-select tools with class eql screwdriver))
          (screwdrivers rtn '(db-select tools with class eql screwdriver))))

(define-tree enumerate
  (brnchs (identify)
          (describe)))

(define-tree present
  (brnchs (show me)
          (what is)
          (what are)
          (give)
          (display)
          (print)
          (present)))

(define-tree object1 (> objects rtn objects))

(define-tree object2 (> objects rtn objects))

(defun report-distance (a b)
  (format t "~&~a"
          (sqrt (+ (square (- (first a) (first b)))
                   (square (- (second a) (second b)))))))

(defun square (n) (* n n))


#|
This portion of the file contains a set of access functions for
an instructional relational database system.  Note that all the
records for each database are kept in random access memory.  In
commercial-grade relational database systems, records are
normally stored on disk memory and are brought into random access
memory one at a time or in small batches.

Some of the procedures listed are drawn from the problem
solutions.  Several are modified so that the symbol AND is
ignored in argument lists.
|#

;;;; Database Interface

(defun db-call (arg) (eval arg))

;;;; Relation Access Procedures

(defmacro db-make-relation args
  (let ((relation (first args))
	(fields-and-records (rest args)))
    `(setf ,relation
	   ',fields-and-records)))

(defmacro db-extract-value args 
  (let ((relation (first args))
	(projections (rest (rest args))))
    `(db-extract-value-aux ,relation ',(remove-and projections))))

(defun db-extract-value-aux (relation projections)
  (let ((result (db-project-aux (first relation)
                                (rest relation)
                                projections)))
    (when (rest result)
      (format t "~&I'm taking the first of more than one record."))
    (when (rest (first result))
      (format t "~&I'm taking the first of more than one field."))
    (first (first result))))

(defun db-count (relation)
  (format t "~&~a" (length (rest relation)))
  (values))

;;;; Relation Manipulators

(defun db-union (&rest records)
  (cons (first (first records))
        (remove-duplicates (apply #'append
                                  (mapcar #'rest records))
                           :test #'equal)))

(defmacro db-project args
  (let ((relation (first args))
	(projections (rest (rest args))))
    `(cons ',(remove-and projections)
	   (db-project-aux (first ,relation) (rest ,relation)
			   ',(remove-and projections)))))

(defun db-project-aux (fields records projections)
  (remove-duplicates
    (mapcar
      #'(lambda (record)
          (mapcar
            #'(lambda (projection)
		(nth (position projection fields) record))
            projections))
      records)
    :test #'equal))

(defmacro db-select args
  (let ((relation (first args))
	(triples (rest (rest args))))
    `(cons (first ,relation)
	   (db-select-aux (first ,relation)
			  (rest ,relation)
			  ',(remove-and triples)))))

(defun db-select-aux (fields records triples)
  (if (endp triples)
      records
      (remove-if-not
        #'(lambda (record)
            (funcall (second triples)
                     (nth (position (first triples) fields)
                          record)
                     (third triples)))
        (db-select-aux fields records (nthcdr 3 triples)))))

(defmacro db-join args
  (let ((db1 (first args))
	(db2 (second args))
	(triples (rest (rest (rest args)))))
    `(cons (append (first ,db1) (first ,db2))
	   (db-join-aux (first ,db1)
			(first ,db2)
			(rest ,db1)
			(rest ,db2)
			',(remove-and triples)))))

(defun db-join-aux (fields1 fields2 db1 db2 triples &aux result)
  (dolist (line1 db1 (reverse result))
    (dolist (line2 db2)
      (do* ((triples triples (nthcdr 3 triples)))
           ((endp triples)
            (setf result (cons (append line1 line2) result)))
        (unless (funcall (second triples)
                         (nth (position (first triples) fields1)
                              line1)
                         (nth (position (third triples) fields2)
                              line2))
          (return))))))

;;;; Auxiliary

(defun remove-and (list)
  (cond ((endp list) nil)
	((eql 'and (first list)) (rest list))
	(t (cons (first list) (remove-and (rest list))))))

;;;; Relation Displayer

(defun db-show (relation)
  (let ((widths (db-find-field-widths relation)))
    (db-show-record widths (first relation))
    (db-show-record
        widths
        (mapcar #'(lambda (width)
                    (make-string width :initial-element #\-))
                widths))
    (dolist (record (rest relation) (values))
      (db-show-record widths record))))

(defun db-show-record (widths fields)
  (format t "~&|")
      (do ((widths widths (rest widths))
           (fields fields (rest fields)))
	  ((endp widths))
	;;Rewritten to work with Common LISP subsets:
	(format t " ~a~a |"
		(first fields)
		(make-string
		  (- (first widths)
		     (length (format nil "~a" (first fields))))
		  :initial-element #\ ))))

(defun db-find-field-widths (relation &aux result)
  (setf result
        (mapcar #'(lambda (field)
                    (length (format nil "~a" field)))
                (first relation)))
  (dolist (record (rest relation) result)
    (setf result 
          (mapcar #'(lambda (number symbol)
                      (max number
                           (length (format nil "~a" symbol))))
                  result
                  record))))



#|
This portion of the file contains test data for various relational
database procedures.
|#

;;;; Database

(db-make-relation tools
  (class	color	size	weight	location)
  (saw		black	medium	heavy	pegboard)
  (hammer	blue	large	heavy	workbench)
  (wrench	gray	small	light	pegboard)
  (wrench	gray	large	heavy	pegboard)
  (screwdriver	blue	long	light	workbench)
  (screwdriver	black	long	light	toolchest)
  (screwdriver	red	long	heavy	toolchest)
  (screwdriver	red	short	light	toolchest))

;;;; Database Extension

(defmacro db-list-values (projection)
  `(rest (mapcar #'first ,projection)))

;;;; New Definitions

(define-tree objects			;Redone.
  (brnchs (> determiner > objects rtn objects)
          (> attribute-values > object
             rtn `(db-select ,object with ,@attribute-values))
          (> object rtn object)
	  (> proper-name rtn proper-name)))	;Line added.

(define-tree object			;Redone.
  (brnchs (tool		rtn 'tools)
	  (tools	rtn 'tools)
	  (saw          rtn '(db-select tools with class eql saw))
          (saws         rtn '(db-select tools with class eql saw))
          (hammer       rtn '(db-select tools with class eql hammer))
          (hammers      rtn '(db-select tools with class eql hammer))
          (wrench       rtn '(db-select tools with class eql wrench))
          (wrenches     rtn '(db-select tools with class eql wrench))
          (screwdriver  rtn '(db-select tools with class eql screwdriver))
          (screwdrivers rtn '(db-select tools with class eql screwdriver))))

(define-tree attribute-value		;Redone
  (brnchs (> size-value rtn size-value)
	  (> color-value rtn color-value)
	  (> weight-value rtn weight-value)))

(define-tree size-value			;Introduced.
  (brnchs (large        rtn '(size eql large))
          (medium       rtn '(size eql medium))
          (small        rtn '(size eql small))
          (long         rtn '(size eql long))
          (short        rtn '(size eql short))))

(define-tree color-value			;Introduced.
  (brnchs (black        rtn '(color eql black))
          (blue         rtn '(color eql blue))
          (red          rtn '(color eql red))
          (yellow       rtn '(color eql yellow))
          (gray         rtn '(color eql gray))))

(define-tree weight-value			;Introduced.
  (brnchs (heavy        rtn '(weight eql heavy))
          (light        rtn '(weight eql light))))


;;;; Alternative Database

#+comment
(db-make-relation geography
  (NAME		TYPE		LOCATION	HEIGHT	   AREA)
  (Everest	Mountain	Asian		29028	   --)
  (Superior	Lake		North-American	--	   31820)
  (Angel	Waterfall       South-America   3212       --)
  (Sahara       Desert          African         --         3000500)   
  (Fujiyama     Volcano         Asian           12395      --)
  (Annapurna    Mountain        Asian           26503      --)
  (Vesuvius     Volcano         European        3900       --)
  (Victoria     Lake            African         --         26828)
  (Gobi         Desert          Asian           --         450000)
  (Victoria     Waterfall       African         420        --)
  (McKinley     Mountain        North-American  20300      --)
  (Bear		Lake            North-American  --         12800)
  (Victoria     Desert          Oceanianic      --         125000)
  (Niagara      Waterfall       North-American  167        --)
  (Kilimanjaro  Mountain        African         19321      --)
  (Whitney      Mountain        North-American  14495      --)
  (Chad         Lake            African         --         10000)
  (Olympus      Mountain        European        9730       --)
  (Kosciusko    Mountain        Oceanianic      7316       --)
  (Tasmania     Island          Oceanianic      --         26215)
  (Greenland    Island          North-American  --         840000))

;;;; Changes to solve location problem

#|

(define-tree objects
  (brnchs (> basic-objects > location
	     rtn `(db-select ,basic-objects with ,@location))
	  (> basic-objects rtn basic-objects)))

(define-tree basic-objects
  (brnchs (> determiner > objects rtn objects)
          (> attribute-values > object
             rtn `(db-select ,object with ,@attribute-values))
          (> object rtn object)))

(define-tree location
  (> location-word the > place rtn place))

(define-tree location-word
  (brnchs (in) (on)))

(define-tree place
  (brnchs (workbench rtn '(location eql workbench))
	  (pegboard rtn '(location eql pegboard))
	  (toolchest rtn '(location eql toolchest))))

;;;; Tracing for book:

(trace
  attributes attribute
  attribute-value attribute-values
  object objects
  determiner
  question-or-command)


|#

