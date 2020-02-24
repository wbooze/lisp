;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; REMARKS

#|
Works on a nest list of numbers, which simulates a game tree.
Thus, there is no specific game involved.  The virtue is that
you can experiment with the procedure on any tree you like.
|#

;;;; ALPHA-BETA PROCEDURE

(defun alpha-beta (top-level-function root-node)
  "
  Purpose:	Alpha-beta search.
  Arguments:	Minimizer or maximizer function, the game tree.
  Returns:	Nothing.
  Remarks:	MINIMIZER and MAXIMIZER do all the work.
  "
  (format t "~%~%Starting alpha-beta search ...")
  (multiple-value-bind (score path)
      (funcall top-level-function root-node -10000 +10000 0 nil)
    (format t "~%The winning score is ~a, ~
	         found at the end of ~a." score path)
    (values)))

(defun maximizer (node alpha beta depth path)
  "
  Remarks:	Much like MINIMIZER.
  "
  (let* ((children (children node)) (best-path nil))
    (if (endp children)
	;;If there are no children, report the static value:
	(values (score node) path)
      ;;Otherwise, check out the children; return alpha and the best path:
      (dotimes (n (length children) (values alpha best-path))
	(let ((child (nth n children)))
	  (multiple-value-bind (test-score test-path)
	      (minimizer child alpha beta (1+ depth) (append path (list n)))
	    ;;Determine if the child is the best so far:
	    (when (> test-score alpha)
	      ;;Reset alpha:
	      (setf alpha test-score)
	      ;;Reset the best path:
	      (setf best-path test-path)))
	  ;;Check for cutoff condition:
	  (when (and (>= alpha beta) (< n (1- (length children))))
	    (format t "~%Maximizer alpha-beta cutoff, depth ~a, path ~a."
		    depth path)
	    (return alpha)))))))

(defun minimizer (node alpha beta depth path)
  "
  Remarks:	Much like MAXIMIZER.  See MAXIMIZER for comments.
  "
  (let* ((children (children node)) (best-path nil))
    (if (endp children)
	(values (score node) path)
      (dotimes (n (length children) (values beta best-path))
	(let ((child (nth n children)))
	  (multiple-value-bind (test-score test-path)
	      (maximizer child alpha beta (1+ depth) (append path (list n)))
	    (when (< test-score beta)
	      (setf beta test-score)
	      (setf best-path test-path))
	    (when (and (>= alpha beta) (< n (1- (length children))))
	      (format t "~%Minimizer alpha-beta cutoff, depth ~a, path ~a."
		      depth path)
	      (return beta))))))))

;;;; ACCESS PROCEDURES

(defun children (node) (if (listp node) node nil))

(defun score (node) (if (listp node) (error "No score available") node))

