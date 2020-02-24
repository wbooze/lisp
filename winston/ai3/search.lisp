;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; GENERALIZED SEARCH PROCEDURE

(defun generalized-search
       (start finish method &optional (queue (list (list start))))
  "
  Purpose:	General-purpose search procedure.
  Arguments:	Start, finish, and method-dependent queuing function.
  Returns:	Path from start to finish.
  Remarks:	Implements many search methods via various queuing methods.
  "  
  (cond
    ;;If the queue is empty, lose; no path can be found:
    ((endp queue) nil)
    ;;If the first element terminates at the finish, win:
    ((eq finish (first (first queue)))      ;Finish found; done.
     (format t "~%The successful path is ~a." (reverse (first queue)))
     (reverse (first queue)))
    ;;Otherwise, try again with a new, method-specific queue:
    (t (generalized-search start finish method
	 (funcall method (extend (first queue)) (rest queue) finish)))))

(defun extend (path)
  (format t "~%Extending the path ~a." (reverse path))
  ;;Produce a list of one-place partial-path extensions:
  (mapcar #'(lambda (new-node) (cons new-node path))
	  ;;Get rid of circular paths:
          (remove-if #'(lambda (neighbor) (member neighbor path))
		     ;;Get the neighbors of the terminal place:
		     (read-place-neighbors (first path)))))

;;;; ACCESS PROCEDURES FOR PLACES

(defmacro defplace (place neighbors coordinates)
  `(progn
     ;;A place is an atom, which evaluates to itself:
     (setf ,place ',place)
     ;;It has a list of neighbors on its property list:
     (setf (get ',place 'neighbors) ',neighbors)
     ;;And it has a list of x-y coordinates on its property list:
     (setf (get ',place 'coordinates) ',coordinates)
     ',place))

(defun read-place-neighbors (place)
  (get place 'neighbors))

(defun read-place-coordinates (place)
  (get place 'coordinates))

;;;; SPECIFIC SEARCH PROCEDURES

;;; Depth-first Search

(defun depth-first (start finish)
  (generalized-search start finish #'queue-depth-first))

(defun queue-depth-first (new old &rest ignore)
  ;;Add new paths to the front of the queue:
  (append new old))

;;; Breadth-first Search

(defun breadth-first (start finish)
  (generalized-search start finish #'queue-breadth-first))

(defun queue-breadth-first (new old &rest ignore)
  ;;Add new paths to the end of the queue:
  (append old new))

;;; Best-first Search

(defun best-first (start finish)
  (generalized-search start finish #'queue-best-first))

(defun queue-best-first (new old finish)
  ;;Sort entire queue by distance to goal:
  (sort (append new old) #'(lambda (p1 p2) (closerp p1 p2 finish))))

(defun closerp (path-1 path-2 finish)
  (< (straight-line-distance (first path-1) finish)
     (straight-line-distance (first path-2) finish)))

(defun straight-line-distance (node-1 node-2)
  (let ((coordinates-1 (read-place-coordinates node-1))
        (coordinates-2 (read-place-coordinates node-2)))
    (sqrt (+ (expt (- (first coordinates-1) (first coordinates-2)) 2)
             (expt (- (second coordinates-1) (second coordinates-2)) 2)))))

;;; Hill Climbing 

(defun hill-climb (start finish)
  (generalized-search start finish #'queue-hill-climb))

(defun queue-hill-climb (new old finish)
  ;;Sort new paths by distance to goal; add result to front of queue:
  (append (sort new #'(lambda (p1 p2) (closerp p1 p2 finish))) old))

;;; Beam Search

(defvar *beam-width* 2)

(defun beam (start finish &optional (queue (list (list start))))
  "
  Purpose:	Beam search.
  Returns:	Path from start to finish.
  Remarks:	Keeps *BEAM-WIDTH* best partial paths.
  "  
  (cond ((endp queue) nil)
        ((eq finish (first (first queue)))
	 (format t "~%The successful path is ~a." (reverse (first queue)))
         (reverse (first queue)))
	;;Differs from generalized search; all partial paths extended:
        (t (beam start finish (queue-beam (mapcan #'extend queue) finish)))))

(defun queue-beam (paths finish)
  ;;Sort all paths by distance to goal; keep only best:
  (setf paths
	(or (filter-out-losers paths finish)
	    (filter-out-dead-ends paths)))
  (let* ((n (length paths)))
    (butlast (sort paths
		   #'(lambda (p1 p2) (closerp p1 p2 finish)))
	     (max 0 (- n *beam-width*)))))

(defun filter-out-losers (paths finish)
  (remove-if-not #'(lambda (path) (eq finish (first path))) paths))

(defun filter-out-dead-ends (paths)
  (remove-if-not #'extendable-p paths))

(defun extendable-p (path)
  ;;Eliminate if circular:
  (remove-if #'(lambda (neighbor) (member neighbor path))
	     ;;Get the neighbors of the terminal place:
	     (read-place-neighbors (first path))))