;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; TOP-LEVEL INTERFACE AND DATABASE INTERFACE

(defun tools () (run-interface))

(defun db-call (arg) (eval arg))

;;;; TRANSITION-TREE GRAMMAR

;;; Questions and commands

(compile-tree interface
  (brnchs
    (count > list-of-objects if-end-rtn
       (db-call `(db-show (db-count-items ,list-of-objects))))
    (how many > list-of-objects are there if-end-rtn
       (db-call `(db-show (db-count-items ,list-of-objects))))
    (what are > attributes > list-of-objects if-end-rtn
	  (db-call
	      `(db-show
		 (db-project ,list-of-objects over ,@attributes))))
    (what > attributes are > list-of-objects if-end-rtn
	  (db-call
	      `(db-show
		 (db-project ,list-of-objects over ,@attributes))))
    (> identify
       brnchs
       (> list-of-objects if-end-rtn (db-call `(db-show ,list-of-objects)))
       (the > attributes of > list-of-objects if-end-rtn
	    (db-call
	      `(db-show
		 (db-project ,list-of-objects over ,@attributes)))))
    (put > object1 on > object2 if-end-rtn
	 (format t "~&~a"
	   `(put-on ,(db-call `(db-extract-value ,object1 name))
		    ,(db-call `(db-extract-value ,object2 name)))))
    (how far is > object1 from > object2 if-end-rtn
	 (eval `(report-distance
	      ',(db-call `(db-extract-value ,object1 position))
	      ',(db-call `(db-extract-value ,object2 position)))))
    (> rank > list-of-objects by > attributes if-end-rtn
       (db-call
	 `(db-show
	    (db-sort ,list-of-objects by ,@attributes))))))

;;; Objects

(compile-tree object1 (> objects rtn objects))

(compile-tree object2 (> objects rtn objects))

(compile-tree list-of-objects
  (brnchs (> objects > list-of-objects rtn `(db-combine ,objects
						      ,list-of-objects))
          (and > objects	       rtn objects)
          (> objects		       rtn objects)))

(compile-tree objects			;Redone.
  (brnchs (> determiner > objects rtn objects)
          (> attribute-values > object
             rtn `(db-select ,object with ,@attribute-values))
          (> object rtn object)
	  (> proper-name rtn proper-name)))	;Line added.

(compile-tree object			;Redone.
  (brnchs (block	rtn 'blocks)
	  (blocks	rtn 'blocks)
	  (box          rtn '(db-select blocks with class eql box))
          (boxes        rtn '(db-select blocks with class eql box))
          (ball         rtn '(db-select blocks with class eql ball))
          (balls        rtn '(db-select blocks with class eql ball))
          (wedge        rtn '(db-select blocks with class eql wedge))
          (wedges       rtn '(db-select blocks with class eql wedge))
          (brick  	rtn '(db-select blocks with class eql brick))
          (bricks 	rtn '(db-select blocks with class eql brick))
	  (tool		rtn 'tools)
	  (tools	rtn 'tools)
	  (saw          rtn '(db-select tools with class eql saw))
          (saws         rtn '(db-select tools with class eql saw))
          (hammer       rtn '(db-select tools with class eql hammer))
          (hammers      rtn '(db-select tools with class eql hammer))
          (wrench       rtn '(db-select tools with class eql wrench))
          (wrenches     rtn '(db-select tools with class eql wrench))
          (screwdriver  rtn '(db-select tools with class eql screwdriver))
          (screwdrivers rtn '(db-select tools with class eql screwdriver))))

(compile-tree proper-name		;Introduced.
  (brnchs (box1		rtn '(db-select blocks with name eql box1))
	  (ball1	rtn '(db-select blocks with name eql ball1))
	  (wedge1	rtn '(db-select blocks with name eql wedge1))
	  (wedge2	rtn '(db-select blocks with name eql wedge2))
	  (brick1	rtn '(db-select blocks with name eql brick1))
	  (brick2	rtn '(db-select blocks with name eql brick2))
	  (brick3	rtn '(db-select blocks with name eql brick3))
	  (brick4	rtn '(db-select blocks with name eql brick4))
	  (saw1		rtn '(db-select tools with name eql saw1))
	  (hammer1	rtn '(db-select tools with name eql hammer1))
	  (wrench1	rtn '(db-select tools with name eql wrench1))
	  (wrench2	rtn '(db-select tools with name eql wrench2))
	  (screwdriver1	rtn '(db-select tools with name eql screwdriver1))
	  (screwdriver2	rtn '(db-select tools with name eql screwdriver2))
	  (screwdriver3	rtn '(db-select tools with name eql screwdriver3))
	  (screwdriver4	rtn '(db-select tools with name eql screwdriver4))))

;;; Object Attributes

(compile-tree attributes
  (brnchs (> attribute > attributes rtn (cons attribute
                                              attributes))
          (and > attribute          rtn (list attribute))
          (> attribute              rtn (list attribute))))

(compile-tree attribute
  (brnchs (class        rtn 'class)
          (classes      rtn 'class)	;Added.
          (color        rtn 'color)
          (colors       rtn 'color)	;Added.
          (name         rtn 'name)
          (names        rtn 'name)	;Added.
          (size         rtn 'size)
          (sizes        rtn 'size)	;Added.
          (weight       rtn 'weight)
          (weights      rtn 'weight)	;Added.
          (position     rtn 'position)
          (positions    rtn 'position)	;Added.
          (location     rtn 'position)
          (locations    rtn 'position)

          (cost         rtn 'cost)
          (costs        rtn 'cost)
	  (quality      rtn 'quality)
          (qualities    rtn 'quality)
	  (supplier     rtn 'supplier)
          (suppliers    rtn 'supplier)
	  ))

(compile-tree attribute-values
  (brnchs (> attribute-value > attribute-values
             rtn (append attribute-value attribute-values))
          (and > attribute-value rtn attribute-value)
          (> attribute-value rtn attribute-value)))

(compile-tree attribute-value		;Redone
  (brnchs (> size-value rtn size-value)
	  (> color-value rtn color-value)
	  (> weight-value rtn weight-value)
	  (> quality-value rtn quality-value)
	  (> supplier-value rtn supplier-value)))

(compile-tree size-value			;Introduced.
  (brnchs (large        rtn '(size eql large))
          (medium       rtn '(size eql medium))
          (small        rtn '(size eql small))
          (long         rtn '(size eql long))
          (short        rtn '(size eql short))))

(compile-tree color-value			;Introduced.
  (brnchs (black        rtn '(color eql black))
          (blue         rtn '(color eql blue))
          (red          rtn '(color eql red))
          (yellow       rtn '(color eql yellow))
          (gray         rtn '(color eql gray))))

(compile-tree weight-value			;Introduced.
  (brnchs (heavy        rtn '(weight eql heavy))
          (light        rtn '(weight eql light))))

(compile-tree quality-value			;Introduced.
  (brnchs (good        rtn '(quality eql good))
          (bad         rtn '(quality eql bad))))

(compile-tree supplier-value			;Introduced.
  (brnchs (sears	rtn '(supplier eql sears))
          (true-value	rtn '(supplier eql true-value))
	  (fly-by-night	rtn '(supplier eql fly-by-night))))

;;; Miscellaneous

(compile-tree rank
  (brnchs (rank rtn 'rank)
	  (list rtn 'rank)
	  (sort rtn 'rank)))

(compile-tree determiner
  (brnchs (a) (the)))

(compile-tree identify
  (brnchs (identify)
          (describe)
	  (show me all) 	;Hack that sort of illustrates lack of back up
	  (show me)		;Reversing order defeats
          (what is)
          (what are)
          (who is)
          (who are)
          (give)
          (display)
          (print)
          (list)
          (present)))

;;;; AUXILIARY FUNCTIONS

;;; Database Comparison and Ranking

(defmacro db-sort (&rest args)
  (let ((relation (first args))
	(keys (rest (rest args))))
    `(cons (first ,relation)
	   (db-sort-aux (first (copy-list ,relation))
			(rest (copy-list ,relation))
			',keys))))

(defun db-sort-aux (fields records keys)
  (if (endp keys)
      records
    (let ((n (position (first keys) fields)))
      (if (numberp n)
	  (stable-sort
	   (copy-list (db-sort-aux fields records (rest keys)))
	   #'(lambda (x y) (lessp (nth n x) (nth n y))))
	(progn (format t "~%Sorry---I cannot rank by ~a." (first keys))
	       (db-sort-aux fields records (rest keys)))))))

(defun lessp (x y)
  (cond ((and (numberp x) (numberp y)) (< x y))
	((string-lessp (format nil "~a" x) (format nil "~a" y)))))

;;; Distance Determination and Reporting

(defun report-distance (a b)
  (format t "~&They are ~a units apart."
          (sqrt (+ (square (- (first a) (first b)))
                   (square (- (second a) (second b)))))))

(defun square (n) (* n n))

