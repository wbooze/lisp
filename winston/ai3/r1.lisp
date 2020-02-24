;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; Created: 23 July 1991
;;;; Purpose: Implement representations based on semantic nets.


(require 'clos "/phw/modules/clos")

;;; TEST

(defparameter *test*
  nil
  "When T, evaluate arguments of TEST; when nil, ignore them.")

(defmacro test (&rest expressions)
  "
  Arguments:	Things to evaluate.
  Returns:	Nothing.
  "
  `(progn (if *test* (progn ,@expressions)
	    (format t "~%Test ignored."))
	  (values)))

;;; SEMANTIC NET

;; Node and link constructors

(defclass node ()
  ((name :initform (gentemp "node-") :initarg :name :accessor name)
   (links :initform nil :accessor links)))
  
(defmacro construct-node (&optional name)
  "
  Purpose:	Define a more convenient constructor for the node class.
  Remarks:	If name is supplied, the new node is bound to the name.
  Caveats:	Name is not evaluated.
  "
  (if name
      `(setf ,name (make-instance 'node :name ',name))
      `(make-node)))

(defclass link ()
  ((tail  'unknown :initarg :tail  :accessor tail)
   (label 'unknown :initarg :label :accessor label)
   (head  'unknown :initarg :head  :accessor head)))

(defun construct-link (tail label head)
  "
  Purpose:	Define a more convenient constructor for the link class.
  Remarks:	Does not make a new one if one exists.
  "
  (dolist (link (links tail) (construct-link-aux tail label head))
    (when (and (eql label (label link)) (eql head (head link)))
      (return link))))

(defun construct-link-aux (tail label head)
  "
  Purpose:	Auxiliary.
  Caveats:	Not to be called directly.
  "
  (let ((link (make-instance 'link :tail tail :label label :head head)))
    (push link (links tail))
    link))

;; Link readers

(defun read-links (node &optional label)
  "
  Purpose:	Returns all links given no label;
		returns all links with given label otherwise.
  "
  (if label
      (remove-if-not #'(lambda (link) (eq (label link) label))
		     (links node))
    (links node)))

(defun read-link (node link)
  "
  Purpose:	Returns link with given label.
  Caveats:	Complains if no link with given label or more than one.
  "
  (let ((links (read-links node link)))
    (case (length links)
      (0 (error "There is no ~a link from ~a." link (node-name node)))
      (1 (first links))
      (t (error "There is more than one ~a link from ~a."
		link (node-name node))))))

(defun read-heads (node &optional label)
  "
  Purpose:	Returns heads of links with given label.
  "
  (mapcar #'head (read-links node label)))

(defun read-head (node link)
  "
  Purpose:	Returns head of link with given label.
  Caveats:	Complains if no link with given label or more than one.
  "
  (head (read-link node link)))

;; Link deleter and replacer

(defun delete-link (tail label head)
  "
  Purpose:	Delete the link with given label and head.
  Returns:	T iff link exits.
  "
  (setf (links tail) (delete-if (and (eql label (label link))
				     (eql head (head link)))
				 (links tail))))

(defun replace-head (node label value)
  "
  Purpose:	Replace a link head.
  Caveat:	Requires zero or one link with given label.
  "
  (let ((all-links (links node))
	(old-links (read-links node label)))
    (case (length old-links)
      (0)
      (1 (setf (links node) (delete (first old-links) all-links)))
      (otherwise
       (error "Cannot use replace-head if more than one ~a link." label)))
    (construct-link node label value)))

;; Special writers and readers 

(defun write-class (node class) (construct-link node 'is-a class))

(defun read-classes (node) (read-heads node 'is-a))

(defun write-property (node property) (construct-link node 'is property))

(defun read-properties (node) (read-heads node 'is))

;;; GEOMETRIC ANALOGY NET

(defmacro construct-object (name)
  "
  Purpose:	Constructs a node and an IS-A link to OBJECT.
  "
  `(progn (construct-node ,name) (write-class ,name 'object)))

(defconstant *allowed-object-classes*
  '(dot circle triangle square rectangle)
  "The classes allowed.")

(defun write-object-class (object class)
  "
  Purpose:	Constructs an IS-A link to the given class.
  Remarks:	Complains if given class is not in the approved list.
  "
  (if (member class *allowed-object-classes*)
      (write-class object class)
    (error "An object cannot be a ~a." class)))

(defconstant *allowed-relation-labels*
  '(inside above left-of)
  "The classes allowed.")  

(defun construct-relation (o1 relation o2)
  "
  Purpose:	Constructs a relation between two nodes.
  Remarks:	Complains if given relation is not in the approved list.
  "
  (unless (member relation *allowed-relation-labels*)
    (error "~a is not a recognized relation." relation))
  (construct-link o1 relation o2))

(defconstant *allowed-transformation-labels*
  '(deletion addition expansion contraction rotation reflection)
  "The transformations allowed.")

(defun construct-transformation (o1 transformation o2)
  "
  Purpose:	Constructs a transformation between two nodes.
  Remarks:	Complains if given transformation is not in the approved list.
  "
  (unless (member transformation *allowed-transformation-labels*)
    (error "~a is not a recognized transformation." transformation))
  (construct-link o1 transformation o2))

(test
  (construct-object o1)
  (construct-object o2)
  (construct-object o1prime)
  (construct-object o2prime)
  (write-object-class o1 'rectangle)
  (write-object-class o1prime 'rectangle)
  (construct-relation o1 'above o2)
  (construct-transformation o1 'expansion o1prime)
  (print o1)
  (mapc #'print (links o1)))

;;; SEMANTIC TREE

(defun construct-branch (supernode subnode)
  "
  Purpose:	Constructs a branch between two nodes.
  "
  (construct-link supernode 'branch subnode))

(defun read-branches (node)
  "
  Purpose:	Access a node's branches.
  "
  (read-heads node 'branch))

;;; FEATURE SPACE

(defun write-feature (node feature feature-value)
  "
  Purpose:	Write's a given node's given feature value.
  Remarks:	It is a constructor if no such feature exists yet.	
  "
  (replace-head node feature feature-value))

(defun read-feature (node feature)
  "
  Purpose:	Read's a given node's given feature value.
  Caveats:	Complains if more than one feature link of given name. 
  "
  (read-head node feature))

(test
  (construct-node u)
  (write-feature u 'height 5)
  (write-feature u 'height 6)
  (describe u)
  (mapc #'describe (links u))
  (describe (read-head u 'height)))

;;; STATE SPACE

(defmacro construct-state (name)
  "
  Purpose:	Constructs a node and an IS-A link to STATE.
  "
  `(progn (construct-node ,name) (write-class ,name 'state)))

(defun construct-transition (s1 s2)
  "
  Purpose:	Constructs a TRANSITION link between two states.
  "
  (construct-link s1 'transition s2))

(defun read-transitions (node)
  "
  Purpose:	Access a node's transitions.
  "
  (read-heads node 'transitions))

(test (construct-state s1)
      (construct-state s2)
      (describe (construct-transition s1 s2))
      (describe (tail (construct-transition s1 s2))))

;;; GOAL TREE

(defmacro construct-goal (name)
  "
  Purpose:	Constructs a node and an IS-A link to GOAL.
  "
  `(progn (construct-node ,name) (write-class ,name 'goal)))

(defconstant *allowed-goal-classes*
  '(and-node or-node leaf-node)
  "The classes allowed.")

(defun write-goal-class (object class)
  "
  Purpose:	Constructs an IS-A link to the given class.
  Remarks:	Complains if given class is not in the approved list.
  "
  (if (member class *allowed-goal-classes*)
      (write-class object class)
    (error "A goal cannot be a ~a." class)))

(defun read-goal-class (goal)
  "
  Purpose:	Read a goal's class.
  Caveats:	Returns first found; does not check for more.
  "
  (first (intersection *allowed-goal-classes* (read-classes goal))))

(test
  (construct-goal s)
  (construct-goal 1st)
  (construct-goal 2nd)
  (construct-branch s 1st)
  (construct-branch s 2nd)
  (write-goal-class s 'and-node)
  (print (read-goal-class s))
  (mapc #'describe (read-branches s)))

;;; MAP

(defmacro construct-place (name)
  "
  Purpose:	Constructs a node and an IS-A link to PLACE.
  "
  `(progn (construct-node ,name) (write-class ,name 'place)))

(defun construct-neighbor (a b)
  "
  Purpose:	Construct NEIGHBOR links both ways between nodes.
  "
  (construct-link a 'neighbor b)
  (construct-link b 'neighbor a))

(defun read-neighbors (place)
  "
  Purpose:	Access a place's neighbors.
  "
  (read-heads place 'neighbor))

(test
  (construct-place x)
  (construct-place y)
  (construct-neighbor x y)
  (describe x)
  (describe y)
  (mapc #'describe (links x)))

;;; SEARCH TREE

(defmacro construct-path (name)
  "
  Purpose:	Constructs a node and an IS-A link to PATH.
  "
  `(progn (construct-node ,name) (write-class ,name 'path)))

(test
  (construct-path path)
  (construct-path 1st)
  (construct-path 2nd)
  (construct-branch path 1st)
  (construct-branch path 2nd)
  (construct-link path 'description '(s a))
  (construct-link 1st 'description '(s a b))
  (describe path)
  (mapc #'describe (read-branches path)))

;;; GAME TREE

(defmacro construct-board (name)
  "
  Purpose:	Constructs a node and an IS-A link to BOARD.
  "
  `(progn (construct-node ,name) (write-class ,name 'board)))

(defconstant *allowed-board-classes*
  '(max-node min-noded)
  "The classes allowed.")

(defun write-board-class (object class)
  "
  Purpose:	Constructs an IS-A link to the given class.
  Remarks:	Complains if given class is not in the approved list.
  "
  (if (member class *allowed-board-classes*)
      (write-class object class)
    (error "A board cannot be a ~a." class)))

(defun read-board-class (goal)
  "
  Purpose:	Read a board's class.
  Caveats:	Returns first found; does not check for more.
  "
  (first (intersection *allowed-board-classes* (read-classes goal))))

(test
  (construct-board superboard)
  (construct-board 1st)
  (construct-board 2nd)
  (construct-branch superboard 1st)
  (construct-branch superboard 2nd)
  (write-board-class superboard 'max-node)
  (construct-link superboard 'description 'check)
  (construct-link 1st 'description 'resign)
  (construct-link 2nd 'description 'adjourn)
  (print (read-board-class superboard))
  (describe superboard)
  (mapc #'describe (links superboard)))

;;; PREFERENCE NET

(defconstant *allowed-preference-classes*
  '(acceptible-state rejected-state best-state worst-state)
  "The preference classes allowed.")

(defun write-preference-class (state class)
  "
  Purpose:	Constructs an IS-A link to the given class.
  Remarks:	Complains if given class is not in the approved list.
  "
  (if (member class *allowed-preference-classes*)
      (write-class state class)
    (error "An state cannot be a ~a." class)))

(defun read-preference-classes (node)
  "
  Purpose:	Read a state's preference classes.
  "
  (intersection *allowed-preference-classes* (read-classes node)))

(defconstant *allowed-preference-links*
  '(better-than worse-than as-good-as)
  "The classes allowed.")  

(defun construct-preference-link (o1 link o2)
  "
  Purpose:	Constructs a preference link between two nodes.
  Remarks:	Complains if given preference link is not in the approved list.
  "
  (unless (member link *allowed-preference-links*)
    (error "~a is not a recognized preference link." link))
  (construct-link o1 link o2))

(defun read-preference-links (node)
  (remove-if-not
    #'(lambda (link) (member (label link) *allowed-preference-links*))
    (links node)))

(test
  (construct-state s1)
  (construct-state s2)
  (write-preference-class s1 'best-state)
  (write-preference-class s1 'rejected-state)
  (construct-link s1 'is-a 'mess)
  (print (read-classes s1))
  (print (read-preference-classes s1))
  (construct-preference-link s2 'better-than s1)
  (construct-preference-link s2 'as-good-as s1)
  (mapc #'describe (read-preference-links s2)))

;;; SIMILARITY NET

;;; VERSION SPACE

#|
Purpose: Provide example of a version space.
|#

(defun construct-specialization-link (v1 v2)
  (construct-link v1 'more-general-than v2))

(defun construct-generalization-link (v1 v2)
  (construct-link v1 'more-specific-than v2))

(defun read-specializations (version)
  (read-heads version 'more-general-than))

(defun read-generalizations (version)
  (read-heads version 'more-specific-than))

(test
  (construct-node top)
  (construct-node bottom)
  (construct-node middle)
  (construct-link top 'description "The top's descriptive pattern.")
  (construct-link middle 'description "The middle's descriptive pattern.")
  (construct-link bottom 'description "The bottom's descriptive pattern.")
  (construct-specialization-link top middle)
  (construct-generalization-link bottom middle)
  (describe top)
  (describe middle)
  (describe bottom))

;;; TEST TREE

;;; KD TREE

;;; IDENTIFICATION TREE

#|
Purpose: Provide example of identification tree.
Remarks: Need to reify links.
|#

(defclass link (node)
  "
  Purpose:	Redefine link class.
  Remarks:	Need to connect to node so that branches can be described.
  "
  ((tail  'unknown :initarg :tail  :accessor tail)
   (label 'unknown :initarg :label :accessor label)
   (head  'unknown :initarg :head  :accessor head)))

(defun construct-result (node1 node2 result)
  (let ((link (construct-link node1 'branch node2)))
    (construct-link link 'result result)))

(defun construct-test (node test) (construct-link node 'test test))

(defun read-test (node) (read-head node 'test))

(defun construct-samples (node samples)
  (construct-link node 'samples samples))

(defun read-samples (node) (read-head node 'samples))

(defun read-results (node)
  (read-branches node))

(test
  (construct-node supertest)
  (construct-node 1st)
  (construct-node 2nd)
  (write-class supertest 'test-node)
  (write-class 1st 'leaf-node)
  (write-class 2nd 'leaf-node)
  (construct-result supertest 1st 'female)
  (construct-result supertest 2nd 'male)
  (construct-test supertest 'sex)
  (construct-samples 1st '(sarah dana))
  (construct-samples 2nd '(john peter))
  (print (read-test supertest))
  (mapc #'print
	(mapcar #'(lambda (link) (read-head link 'result))
		(read-links supertest 'branch)))
  (mapc #'print (mapcar #'read-samples (read-results supertest))))

;;; TRANSITION TREE

;;; SEMANTIC TRANSITION TREE
