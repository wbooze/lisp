;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; REMARKS

#|

This relational database simulator is used by the natural
language interface program.  See the tools data file for sample
definitions.

The procedures are meant to constitute an abstraction layer.
Hence no effort hs been made to comment them properly.

|#

;;;; CONSTRUCTORS

(defmacro db-define (relation &rest fields-and-records)
  `(setf ,relation ',fields-and-records))

(defmacro db-construct-relation (&rest args)
  `(db-define ,@args))

;;;; ACCESSORS

(defmacro db-extract-value (relation &rest projections)
  `(db-extract-value-aux (db-project-function ,relation ',projections)))

(defun db-extract-value-aux (relation)
  (setf relation (rest relation))
  (when (rest relation)
    (format t "~&I'm taking the first of more than one record."))
  (when (rest (first relation))
    (format t "~&I'm taking the first of more than one field."))
  (first (first relation)))

(defun db-extract-field-values (relation)
  (mapcar #'rest
	  (mapcar
	    #'(lambda (field)
		(apply #'append (db-project-function relation (list field))))
	    (first relation))))

(defun db-count-items (relation) (1- (length relation)))

;;;; OPERATORS

(defmacro db-add (record relation)
  `(setf ,relation (db-add-function ',record ,relation)))

(defmacro db-delete (record relation)
  `(setf ,relation (db-delete-function ',record ,relation)))

(defun db-add-function (record relation)
  (if (member record (rest relation) :test #'equal)
      relation
    (cons (first relation) (adjoin record (rest relation)))))

(defun db-delete-function (record relation)
  (delete record relation :test #'equal)
  relation)

(defun db-combine (&rest relations)
  (cons (first (first relations))
	(remove-duplicates
	  (apply #'append (mapcar #'rest relations)) :test #'equal)))

(defun db-combine-distinct (&rest relations)
  (cons (first (first relations))
	(apply #'append (mapcar #'rest relations))))

(defmacro db-project (relation &rest field-list)
  `(db-project-function ,relation ',(rest field-list)))

(defmacro db-project-distinct (relation &rest field-list)
  `(db-project-distinct-function ,relation ',(rest field-list)))

(defun db-project-function (relation field-list)
  (setf field-list (remove 'over (remove 'and field-list)))
  (cons field-list
	(remove-duplicates
	  (db-project-aux relation field-list) :test #'equal)))

(defun db-project-distinct-function (relation field-list)
  (setf field-list (remove 'over (remove 'and field-list)))
  (cons field-list (db-project-aux relation field-list)))

(defun db-project-aux (relation projections)
  (let ((fields (first relation))
	(records (rest relation)))
    (mapcar
	#'(lambda (record)
	    (mapcar
	      #'(lambda (projection)
		  (nth (position projection fields) record))
	      projections))
	records)))

(defmacro db-select (relation &rest triple-list)
  `(db-select-function ,relation ',(rest triple-list)))

(defun db-select-function (relation triple-list)
  (setf triple-list (remove 'with (remove 'and triple-list)))
  (cons (first relation)
	(db-select-aux relation triple-list)))
  
(defun db-select-aux (relation triple-list)
  (let ((fields (first relation))
	(records (rest relation)))
    (cond ((endp triple-list) records)
	  ((eq '* (third triple-list))
	   (db-select-aux relation (nthcdr 3 triple-list)))
	  (t (remove-if-not
	       #'(lambda (record)
		   (funcall (second triple-list)
			    (nth (position (first triple-list) fields)
				 record)
			    (third triple-list)))
	       (db-select-aux relation (nthcdr 3 triple-list)))))))

(defmacro db-join (db1 db2 &rest tripple-list)
  `(db-join-function ,db1 ,db2 ',(rest tripple-list)))

(defun db-join-function (db1 db2 triple-list)
  (setf triple-list (remove 'with (remove 'and triple-list)))
  (db-join-aux db1 db2 triple-list))

(defun db-join-aux (db1 db2 triples &aux result)
  (let ((fields1 (first db1)) (fields2 (first db2)))
    (dolist (line1 (rest db1) (cons (append fields1 fields2) (reverse result)))
      (dolist (line2 (rest db2))
	(do* ((triples triples (nthcdr 3 triples)))
	     ((endp triples)
	      (setf result (cons (append line1 line2) result)))
	  (unless (funcall (second triples)
			   (nth (position (first triples) fields1) line1)
			   (nth (position (third triples) fields2) line2))
	    (return)))))))

;;;; RELATION DISPLAY

(defun db-show (arg)
  (if (numberp arg)
      (format t "~a" arg)
  (let ((widths (db-find-field-widths arg)))
    (db-show-record widths (first arg))
    (db-show-record
        widths
        (mapcar #'(lambda (width)
                    (make-string width :initial-element #\-))
                widths))
    (dolist (record (rest arg) (values))
      (db-show-record widths record)))))

(defun db-show-record (widths fields)
  "
  Remarks:	Written strangely to work with Common Lisp subsets.
  "
  (format t "~&|")
      (do ((widths widths (rest widths))
           (fields fields (rest fields)))
	  ((endp widths))
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



