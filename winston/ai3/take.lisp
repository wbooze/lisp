;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; CLASS HIERARCHY

(defclass thing () ((name :accessor name :initarg :name)))
  (defclass person (thing) ())
  (defclass inanimate-thing (thing) ())
    (defclass place (inanimate-thing) ())
    (defclass time (inanimate-thing) ())
    (defclass object (inanimate-thing) ())
      (defclass medicine (object) ())
      (defclass toy (object) ())
      (defclass clothing-item (object) ())

(defclass gramatical-entity (thing) ())
  (defclass word (gramatical-entity) ())
    (defclass preposition (word) ())
      (defclass over (preposition) ((name :initform 'over)))
      (defclass for (preposition) ((name :initform 'for)))
      (defclass out (preposition) ((name :initform 'out)))
      (defclass off (preposition) ((name :initform 'off)))
      (defclass to (preposition) ((name :initform 'to)))
    (defclass verb (word)
      ((agent :accessor agent :initform 'unknown)
       (coagent :accessor coagent :initform 'unknown)
       (beneficiary :accessor beneficiary :initform 'unknown)
       (thematic-object :accessor thematic-object :initform 'thematic-unknown)
       (instrument :accessor instrument :initform 'unknown)
       (source :accessor source :initform 'unknown)
       (destination :accessor destination :initform 'unknown)
       (conveyance :accessor conveyance :initform 'unknown)
       (duration :accessor duration :initform 'unknown)))
      (defclass take (verb) ((name :initform 'take)))
    (defclass noun (word) ())
      (defclass home (noun place) ((name :initform 'home)))
      (defclass town (noun place) ((name :initform ' town)))
      (defclass day (noun time) ((name :initform ' day)))
      (defclass hour (noun time) ((name :initform ' hour)))
      (defclass aspirin (noun medicine) ((name :initform ' aspirin)))
      (defclass tea (noun medicine) ((name :initform ' tea)))
      (defclass box (noun toy) ((name :initform ' box)))
      (defclass ball (noun toy) ((name :initform ' ball)))
      (defclass bell (noun toy) ((name :initform ' bell)))
      (defclass jacket (noun clothing-item) ((name :initform ' jacket)))
      (defclass robbie (noun person) ((name :initform ' robbie)))
      (defclass suzie (noun person) ((name :initform ' suzie)))

;;;; INSTANCES

(setf home (make-instance 'home))
(setf town (make-instance 'town))
(setf day (make-instance 'day))
(setf hour (make-instance 'hour))
(setf aspirin (make-instance 'aspirin))
(setf tea (make-instance 'tea))
(setf box (make-instance 'box))
(setf ball (make-instance 'ball))
(setf bell (make-instance 'bell))
(setf jacket (make-instance 'jacket))
(setf robbie (make-instance 'robbie))
(setf suzie (make-instance 'suzie))

(setf over (make-instance 'over))
(setf for (make-instance 'for))
(setf out (make-instance 'out))
(setf off (make-instance 'off))
(setf to  (make-instance 'to))

(setf take (make-instance 'take))

;;;; INTERPRETATION METHODS

(defmacro interpret (&rest args)
  `(progn
     (catch 'catcher
       (case (length ',args)
	 (3 (interpret3 ,@args))
	 (4 (interpret4 ,@args))
	 (5 (interpret5 ,@args)))
       (format t "~%Sorry, I did not understand that."))
     (values)))

(defmethod interpret3 (a b c) nil)

(defmethod interpret4 (a b c d) nil)

(defmethod interpret5 (a b c d e) nil)

(defmethod interpret3 :before ((agent person)
			       (verb take)
			       (preposition over))
  (format t "~%~
Agent:		~a
Act:		ASSUME CONTROL
"
	  (name agent))
  (throw 'catcher t))

(defmethod interpret3 :before ((agent person)
			       (verb take)
			       (object medicine))
  (format t "~%~
Agent:		~a
Act:		SWALLOW
Object:		~a
Beneficiary:	~a
"
	  (name agent) (name object) (name agent))
  (throw 'catcher t))

(defmethod interpret3 :before ((agent person)
			       (verb take)
			       (object person))
  (format t "~%~
Agent:		~a
Act:		SWINDLE
Object:		~a
"
	  (name agent) (name object))
  (throw 'catcher t))

(defmethod interpret4 :before ((agent person)
			       (verb take)
			       (preposition out)
			       (object inanimate-thing))
  (format t "~%~
Agent:		~a
Act:		REMOVE FROM PREMISES
Object:		~a"
	  (name agent)
	  (name object))
  (throw 'catcher t))

(defmethod interpret4 :before ((agent person)
			       (verb take)
			       (preposition out)
			       (object person))
  (format t "~%~
Agent:		~a
Act:		DATE
Object:		~a"
	  (name agent)
	  (name object))
  (throw 'catcher t))

(defmethod interpret4 :before ((agent person)
			       (verb take)
			       (preposition off)
			       (object clothing-item))
  (format t "~%~
Agent:		~a
Act:		REMOVE FROM BODY
Object:		~a"
	  (name agent)
	  (name object))
  (throw 'catcher t))

(defmethod interpret5 :before ((agent person)
			       (verb take)
			       (object1 thing)
			       (preposition to)
			       (object2 place))
  (format t "~%~
Agent:		~a
Act:		TRANSPORT
Object:		~a
Destination:	~a
"
	  (name agent)
	  (name object1)
	  (name object2))
  (throw 'catcher t))

(defmethod interpret5 :before ((agent person)
			       (verb take)
			       (object1 thing)
			       (preposition for)
			       (object2 person))
  (format t "~%~
Agent:		~a
Act:		STEAL
Object:		~a
Beneficiary:	~a
"
	  (name agent)
	  (name object1)
	  (name object2))
  (throw 'catcher t))

