;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; GENERALIZED SOAR SEARCH PROCEDURE

(defun generalized-search (start goal preference-installer
			   &aux current-state states neighbors)
  "
  Purpose:	General-purpose, SOAR-based search procedure.
  Arguments:	Start, finish, and method-dependent preference function.
  Returns:	Path from start to finish.
  Remarks:	Implements search methods via tailored preference links.
  "  
  (setf current-state start)
  (push current-state states)
  (write-place-is current-state 'acceptable)
  (format t "~%Initial state is: ~a" current-state)
  ;;Loop until goal is found:
  (do () ((eq current-state goal) (print (path current-state)) (values))
    ;;Get current-state's neighbors:
    (setf neighbors (read-place-neighbors current-state))
    ;;But ignore neighbors already seen:
    (setf neighbors (remove-if #'(lambda (n) (member n states)) neighbors))
    (if (endp neighbors)
	;;If there are no surviving neighbors, reject the current state:
	(write-place-is current-state 'rejected)
      ;;Otherwise, install method-dependant preference links:
      (funcall preference-installer current-state neighbors states))
    ;;Parent links help establish path back from eventual winner:
    (dolist (n neighbors) (write-parent n current-state) (push n states))
    ;;Clear dominance relations from previous iteration:
    (dolist (s states) (clear-domination-relations s))
    ;;Use preference links to establish dominance; filter-out dominated nodes:
    (setf candidates (filter-candidate-states states))
    ;;Pick from remaining candidates; hope there is just one:
    (setf current-state (pick-next-state current-state candidates))
    (format t "~%Next state is: ~a" current-state)))

(defun filter-candidate-states (candidates)
  ;;Flush if not acceptable:
  (setf candidates
	(remove-if-not #'(lambda (state) (is-p state 'acceptable)) candidates))
  ;;Flush if rejected:
  (setf candidates
	(remove-if #'(lambda (state) (is-p state 'rejected)) candidates))
  ;;Establish dominance using better-than and worse-than links:
  (dolist (a candidates)
    (dolist (b candidates)
      (when (and (better-than-p a b) (not (better-than-p b a)))
	#+comment
	(format t "~% ~a > ~a" a b)
	(write-place-dominates a b))
      (when (and (worse-than-p a b) (not (worse-than-p b a)))
	#+comment
	(format t "~% ~a > ~a" a b)
	(write-place-dominates b a))))
  ;;Establish dominance using best and worse links:
  (dolist (p1 candidates)
    (when (and (is-p p1 'best)
	       (not (read-place-dominated-by p1)))
      (dolist (p2 (remove p1 candidates))
	(write-place-dominates p1 p2)))
    (when (and (is-p p1 'worst)
	       (not (read-place-dominates p1)))
      (dolist (p2 (remove p1 candidates))
	(write-place-dominates p2 p1))))
  ;;Flush if dominated:
  (remove-if #'read-place-dominated-by candidates))

(defun pick-next-state (current-state candidates)
  "
  Purpose:	Pick next state from surviving candidates.
  Caveats:	Differs from official SOAR version because
		each survivor is considered indifferent-to
		all other survivors; this is because there
		is no impasse-resolving mechanism.
  "
  (if (= 1 (length candidates))
      (setf current-state (first candidates))
    (if (endp candidates)
	(if (not (is-p current-state 'rejected)) 
	    (setf current-state current-state)
	  (progn
	    (format t "An impasse has developed---no candidate.")
	    (error "Preference mechanism cannont choose a next state.")))
      (if (member current-state candidates)
	  (setf current-state current-state)
	(progn (format t "An impasse has developed---I'll pick a candidate.")
	       (setf current-state (first candidates)))))))

;;;; SPECIFIC SEARCH PROCEDURES

;;; Depth-first Search

(defun depth-first-search (start goal)
  (generalized-search start goal #'install-depth-first-preferences))

(defun install-depth-first-preferences (current-state neighbors states)
  "
  Purpose:	Sets preference links for depth-first search.
  "
  (dolist (n neighbors) (write-place-better-than n current-state))
  (dolist (n neighbors) (write-place-is n 'acceptable))
  (do ((neighbors neighbors (rest neighbors)))
      ((endp (rest neighbors)))
    (write-place-better-than (first neighbors) (second neighbors))))

;;; Breadth-first Search

(defun breadth-first-search (start goal)
  (generalized-search start goal #'install-breadth-first-preferences))

(defun install-breadth-first-preferences (current-state neighbors states)
  "
  Purpose:	Sets preference links for breadth-first search.
  "
  (dolist (n neighbors) (dolist (s states) (write-place-worse-than n s)))
  (dolist (n neighbors) (write-place-is n 'acceptable))
  (do ((neighbors neighbors (rest neighbors)))
      ((endp (rest neighbors)))
    (write-place-better-than (first neighbors) (second neighbors))))

;;;; ACCESS PROCEDURES FOR PLACES (adapted from SEARCH.lisp)

(defmacro defplace (place neighbors coordinates)
  "
  Purpose:	Create places; initialize properties.
  Remarks:	Uses place's property list; perhaps a structures would
		be better.
  "
  `(progn
     ;;A place is an atom, which evaluates to itself:
     (setf ,place ',place)
     ;;It has a list of neighbors on its property list:
     (setf (get ',place 'neighbors) ',neighbors)
     ;;And it has a list of x-y coordinates on its property list:
     (setf (get ',place 'coordinates) ',coordinates)
     ;;And, moreover, it has no SOARlike annotations when created:
     (setf (get ',place 'better-than) nil)
     (setf (get ',place 'worse-than) nil)
     (setf (get ',place 'dominates) nil)
     (setf (get ',place 'dominated-by) nil)
     (setf (get ',place 'indifferent) nil)
     (setf (get ',place 'is) nil)
     (setf (get ',place 'parent) nil)
     ',place))

(defun read-place-neighbors (place)
  (get place 'neighbors))

(defun read-place-coordinates (place)
  (get place 'coordinates))

(defun read-place-better-than (place)
  (get place 'better-than))

(defun write-place-better-than (place1 place2)
  (pushnew place2 (get place1 'better-than)))

(defun read-place-worse-than (place)
  (get place 'worse-than))

(defun write-place-worse-than (place1 place2)
  (pushnew place2 (get place1 'worse-than)))

(defun better-than-p (place1 place2)
  (member place2 (read-place-better-than place1)))

(defun worse-than-p (place1 place2)
  (member place2 (read-place-worse-than place1)))

(defun write-place-dominates (place1 place2)
  "
  Remarks:	Installs both dominance relation and
		inverse dominace relation.
  "
  (pushnew place2 (get place1 'dominates))
  (pushnew place1 (get place2 'dominated-by)))

(defun read-place-dominates (place)
  (get place 'dominates))

(defun read-place-dominated-by (place)
  (get place 'dominated-by))

(defun clear-domination-relations (place)
  (setf (get place 'dominates) nil)
  (setf (get place 'dominated-by) nil))

(defun read-place-is (place)
  (get place 'is))

(defun write-place-is (place condition)
  (pushnew condition (get place 'is)))

(defun is-p (place condition)
  (member condition (get place 'is)))

(defun write-parent (child parent)
  (setf (get child 'parent) parent))

(defun read-parent (child)
  (get child 'parent))

;;;; AUXILIARY PROCEDURE

(defun path (node &optional path)
  "
  Purpose:	Finds path, once goal node is discovered,
		by following parent links.
  "
  (let ((parent (read-parent node)))
    (if parent
	(path parent (append (list node) path))
      (append (list node) path))))