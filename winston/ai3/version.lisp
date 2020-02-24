;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; DYNAMIC VARIABLES

(defvar *generalizations*)
(defvar *specializations*)

;;;; USER-LEVEL PROCEDURES

(defun reset-versions ()
  (setf *generalizations* (list (mapcar #'(lambda (x) '?) *ranges*)))
  (setf *specializations* nil))

(defun test-example (example)
  "
  Purpose:	Test example.
  "
  (format t "~%~%")
  ;;Self explanatory:
  (cond ((every #'(lambda (x) (match x example)) *specializations*)
	 (format t "The example, ~a,~
		 ~%matches all specializations; ~
		 it must be a positive example." example)
	 t)
	((not (some #'(lambda (x) (match x example)) *generalizations*))
	 (format t "The example, ~a,~
		 ~%fails to match any generalizations; ~
		 it must be a negative example." example)
	 nil)
	(t (format t "The example, ~a,~%may be positive or negative." example)
	   '?)))

(defun assimilate-positive-example (example)
  "
  Purpose:	Learn from a positive example.
  Remarks:	Much like ASSIMILATE-NEGATIVE-EXAMPLE.
  "
  (if *specializations*
      ;;If there are specializations already ...
      (progn
	;;Use the example to generalize the specific models:
	(setf *specializations*
	      (mapcan #'(lambda (x) (generalize x example)) *specializations*))
	;;Remove any specific model that is not a specialization
	;;of a general model:
	(setf *specializations*
          (remove-if-not
	    #'(lambda (x) (more-specific-than-one-of-p x *generalizations*))
		*specializations*))
	;;Remove any specific model that is more general
	;;than another specific model:
	(setf *specializations* (get-rid-of-generalizations *specializations*))
	;;Remove any generalization that does not match the example:
	(setf *generalizations*
	  (remove-if-not #'(lambda (x) (match x example)) *generalizations*)))
    ;;If there are no specializations yet ...
    (setf *specializations* (list example)))
  (print-result example "positive"))

(defun assimilate-negative-example (example)
  "
  Purpose:	Learn from a negative example.
  Remarks:	See ASSIMILATE-POSITIVE example for comments. 
  "
  (setf *generalizations*
	(mapcan #'(lambda (x) (specialize x example))
		*generalizations*))
  (setf *generalizations*
	(remove-if-not
	  #'(lambda (x) (more-general-than-one-of-p x *specializations*))
	  *generalizations*))
  (setf *generalizations* (get-rid-of-specializations *generalizations*))
  (setf *specializations*
	(remove-if #'(lambda (x) (match x example)) *specializations*))
  (print-result example "negative"))
  
;;;; AUXILIARY PROCEDURES

(defun test-versions ()
  "
  Purpose:	Determine when procedure has converged.
  "
  (cond ((and (= (length *generalizations*) 1)
	      (= (length *specializations*) 1)
	      (equal *generalizations* *specializations*))
	 (format t "~%Just converged on ~a!" (first *generalizations*)))
	((zerop (length *generalizations*))
	 (format t "~%Bad news; no generalizations left---cannot converge."))
	((zerop (length *specializations*))
	 (format t "~%Bad news; no specializations left---cannot converge."))))

(defun print-result (example type)
  (format t "~%~%Just assimilated a ~a example, ~a." type example)
  (format t "~%Surviving generalizations:")
  (dolist (g *generalizations*) (format t "~%~a" g))
  (format t "~%Surviving specializations:")
  (dolist (s *specializations*) (format t "~%~a" s))
  (test-versions)
  (values))
		  
(defun match (model sample)
  "
  Purpose:	Determine if the given sample is an instance of the model.
  "
  (cond ((endp model) t)
	((or (equal '? (first model))
	     (equal (first model) (first sample)))
	 (match (rest model) (rest sample)))
	(t nil)))

(defun generalize (model sample)
  "
  Purpose:	Generalize one model using the sample.
  Remarks:	Generalization only to ? symbol which matches everything;
		there is no class hierarchy.
  "
  (if (match model sample)
      (list model)
    (list (mapcar #'(lambda (x y) (if (equal x y) x '?)) model sample))))

(defun specialize (model sample &aux results)
  "
  Purpose:	Specialize one model using the sample.
  Remarks:	Specialization is to elements in a list of classes;
		there is no class hierarchy.
  "
  (if (match model sample)
      (dotimes (n (length model) results)
	(when (equal '? (nth n model))
	  (dolist (substitution (remove (nth n sample) (nth n *ranges*)))
	    (let ((new-model (copy-list model)))
	      (setf (nth n new-model) substitution)
	      (push new-model results)))))
    (list model)))

(defun more-general-than-one-of-p (generalization specializations)
  (some #'(lambda (x) (more-general-than-p generalization x))
	specializations))

(defun more-specific-than-one-of-p (specialization generalizations)
  (some #'(lambda (x) (more-general-than-p x specialization))
	generalizations))

(defun more-general-than-p (generalization specialization)
  "
  Remarks:	Much like MATCH.
  "
  (cond ((endp specialization) t)
	((or (equal '? (first generalization))
	     (equal (first generalization) (first specialization)))
	 (more-general-than-p (rest generalization) (rest specialization)))
	((equal '? (first specialization)) nil)
	(t nil)))

(defun get-rid-of-specializations (models &aux result)
  (dolist (model models result)
    (unless (more-specific-than-one-of-p model (remove model models))
      (push model result))))

(defun get-rid-of-generalizations (models &aux result)
  (dolist (model models result)
    (unless (more-general-than-one-of-p model (remove model models))
      (push model result))))

