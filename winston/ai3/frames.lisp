;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; REMARKS

#|

Instances can belong to only one class in CLOS.  To work around
this limitation, many singleton classes are created which have
just one instance under them.  For example, Blimpy belongs to the
Blimpys class, which can be a direct subclass of many classes.

|#

;;;; TEST DATA

;;; Experiment Group 1

(defclass dwarfs ()
  ((appetite :accessor appetite :initform 'small)
   (physique :accessor physique :initform 'fat)
   (personality :accessor personality :initform 'gregarious)))
(defclass eccentrics (dwarfs)
  ((personality :accessor personality :initform 'wierd)))
(defclass teachers (dwarfs) ())
(defclass programmers (dwarfs) ())
(defclass athletes (dwarfs) ())
(defclass endomorphs (dwarfs) ())
(defclass professors (eccentrics teachers) ())
(defclass hackers (eccentrics programmers)
  ((personality :accessor personality :initform 'shy)))
(defclass weightlifters (athletes endomorphs) ())
(defclass shotputters (athletes endomorphs) ())
(defclass crazies (professors hackers) ())
(defclass jacques (weightlifters shotputters athletes) ())
(setf crazy (make-instance 'crazies))
(setf jacque (make-instance 'jacques))

;;; Experiment Group 2

(defclass competitors (dwarfs) ((physique :accessor physique :initform 'thin)))
(defclass managers (competitors) ())
(defclass gourmands (dwarfs) ((appetite :accessor appetite :initform 'huge)))
(defclass diarists (dwarfs) ())
(defclass blimpies (managers gourmands diarists) ())
(setf blimpy (make-instance 'blimpies))

(defmethod (setf physique) :after ((x dwarfs) value)
  (when (eq value 'muscular)
    (setf (appetite x) 'large)))

;;; Experiment Group 3

(defmethod hobby ((x athletes)) 'exercise)

(defmethod hobby ((x dwarfs))
  (if (eq 'shy (personality x))
	  'reading
	'dancing))

;;; Experiment group 4

(defclass people () ())
(setf typical-person (make-instance 'people))
(setf typical-dwarf (make-instance 'dwarfs))
(defmethod size ((x dwarfs) (y people)) 'small)
(defmethod size ((x dwarfs) (y dwarfs)) 'average)

(defclass patricks (people) ())
(defclass contexts () ())
(defclass mountain-hiking (contexts) ())
(defclass airplane-travel (contexts) ())
(setf patrick (make-instance 'patricks))
(setf mh (make-instance 'mountain-hiking))
(setf at (make-instance 'airplane-travel))
(defmethod mood ((x patricks) (y mountain-hiking)) 'happy)
(defmethod mood ((x patricks) (y airplane-travel)) 'grumpy)

;;; Experiment Group 5

(defclass event ()
  ((instant :accessor instant
	    :initarg :instant
	    :initform
	    (progn
	      (format t 
		      "~&Enter the number with a colon in it~%> ")
	      (strange-read-line)))
   (day :accessor day
	:initarg :day
	:initform
	(progn
	  (format t 
		  "~&Enter the word like \"today\"~%> ")
	  (strange-read-line)))
   (place :accessor place
	  :initarg :place
	  :initform
	  (progn
	    (format t 
		    "~&Enter the place name~%> ")
	    (strange-read-line)))))

(defclass disaster (event) 
  ((damage :accessor damage
	   :initarg :damage
	   :initform
	   (progn
	     (format t 
		     "~&Enter the integer following a dollar sign~%> ")
	     (strange-read-line)))
   (fatalities :accessor fatalities
	       :initarg :fatalities
	       :initform
	       (progn
		 (format t 
			 "~&Enter the integer near \"kill\" or \"die\"~%> ")
		 (strange-read-line)))))

(defclass earthquake (disaster)
  ((magnitude :accessor magnitude
	      :initarg :magnitude
	      :initform
	      (progn
		(format t 
                  "~&Enter the floating point number between 1.0 and 10.0~%> ")
		(strange-read-line)))
   (fault :accessor fault
	  :initarg :fault
	  :initform
	  (progn
	    (format t 
		    "~&Enter the Proper name near the word \"fault\"~%> ")
	    (strange-read-line)))))

(defun strange-read-line ()
  "
  Remarks:	This is used instead of read-line in
		the earthquake definition above just in case
		your Lisp has trouble because the carriage returns
		required by UNIX are handed to read-line.
  "
  (let ((line (read-line)))
    (if (zerop (length line))
	(strange-read-line)
      line)))

