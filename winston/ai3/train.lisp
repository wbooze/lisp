;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; REMARKS

#|

Logic boxes are described by ordinary Lisp AND, OR, and NOT
functions, plus numbers which denote inputs.  Hence the output of
the logic box described by

1

is the same as the percptron's first input.  The output of logic
box described by

(or 3 7)

is the OR of the percptron's third and seventh inputs.  The
output of logic box described by

(and (or 3 7) (not 5))

is a logical function of the perceptron's third, fifth, and
seventh inputs.

(train-perceptron
  '(<first logic function>
	   <second logic function>
	   ...
	   <last logic function>)
  '((<input 1> ... <input n> <desired output>)		;First sample.
    (<input 1> ... <input n> <desired output>)
    ...
    (<input 1> ... <input n> <desired output>)))	;Final sample.

|#

;;;; DYNAMIC VARIABLE

(defvar *inputs*)

;;;; DATA STRUCTURE

(defstruct perceptron logic-forms weights)

;;;; USER-LEVEL PROCEDURES

(defmacro defperceptron (symbol logic-box-descriptions &optional weights)
  "
  Purpose:	Perceptron constructor.
  Arguments:	First argument is the name of the perceptron.
	        Second argument is a list of logic box descriptions.
  Remarks:	Purpose is to set the given perceptron name to a
		perceptron-describing structure.
  "
  `(setf ,symbol
	 (make-perceptron
	    :logic-forms
	    ;;The remarks above describe the form of logic box descriptions:
	    (mapcar #'replace-numbers ',logic-box-descriptions)
	    :weights
	    ;;If no weights provided, make them all zero initially:
	    (if ,weights ,weights
	       (mapcar #'(lambda (e) 0) ',logic-box-descriptions)))))

(defun replace-numbers (e)
  "
  Purpose:	Replaces numbers in a logic-box description by
		a FETCH form that picks an input out of an input
		list and translates from 1/0 to T/NIL.
  "
  (cond ((numberp e) (list 'fetch e))
	((atom e) e)
	(t (cons (replace-numbers (first e))
		 (replace-numbers (rest e))))))

(defun train-perceptron (perceptron class samples)
  "
  Purpose:	Train a perceptron.
  Arguments:	First argument is the name of the perceptron.
		Second is the class to be recognized.
		Third is a list of samples.  Each sample is a list
		whose first element is the actual class and whose
		remaining elements are the corresponding inputs.
  "
  (format t "~%~%Training a perceptron to recognize ~a." class)
  (let ((weight-history nil)
	(step 0))
    (push (perceptron-weights perceptron) weight-history)
    (loop
      (let ((change-switch nil))
	;;Look at each sample:
	(dolist (s samples)
	  ;;Determine if the perceptron produces the correct answer:
	  (when (xor (equal class (first s))
		     (run-perceptron perceptron (rest s)))
	    ;;If not, change the weights:
	    (alter-perceptron perceptron (equal class (first s)) (rest s))
	    (push (perceptron-weights perceptron) weight-history)
	    (format t "~%Step ~a: ~a ~a ~a --> ~a"
		    (incf step)
		    (second weight-history)
		    (if (equal class (first s)) "+" "-")
		    (rest s)
		    (first weight-history))
	    (setf change-switch t)))
	;;Quit if no sample produces a change:
	(unless change-switch (return)))))
  perceptron)

(defun run-perceptron (perceptron *inputs*)
  ;;Compare sum with zero:
  (plusp
    ;;Add up the weighted logic-box outputs:
    (reduce
      #'+
      ;;Multiply by weights:
      (mapcar #'*
	      (perceptron-weights perceptron)
	      ;;Translate from T/NIL to 1/0:
	      (mapcar #'to-0-or-1
		      ;;Compute logic box outputs:
		      (mapcar #'eval
			      (perceptron-logic-forms perceptron)))))))

(defun alter-perceptron (perceptron desired-result *inputs*)
  ;;Install altered weights:
  (setf (perceptron-weights perceptron)
	;;Add or subtract inputs from weights:
	(mapcar (if desired-result #'+ #'-)
		(perceptron-weights perceptron)
		;;Translate from T/NIL to 1/0:
		(mapcar #'to-0-or-1
			(mapcar #'eval
				(perceptron-logic-forms perceptron))))))

;;;; AUXILIARY PROCEDURES

(defun xor (a b) (if a (if b nil t) b))

(defun fetch (number) (to-t-or-nil (nth number *inputs*)))

(defun to-t-or-nil (x) (if (and (numberp x) (zerop x)) nil t))

(defun to-0-or-1 (x) (if x 1 0))

(defun reset-weights (perceptron)
  "
  Purpose:	Reset a perceptron's weights to 0s.
  "
  (setf (perceptron-weights perceptron)
	(mapcar #'(lambda (ignore) 0.0) (perceptron-weights perceptron))))


