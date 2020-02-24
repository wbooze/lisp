;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; SPECIFIC SEARCH PROCEDURES

;;; Branch-and-Bound Search

(defun branch-and-bound (start finish)
  (generalized-search start finish #'queue-branch-and-bound))

(defun queue-branch-and-bound (new old finish)
  "
  Purpose:	Sorts entire queue by accumulated distance:
  Remarks:	Avoids recalculating accumulated distance while sorting.
  "
  (mapcar #'second
	  (sort (mapcar #'(lambda (path) (list (path-length path) path))
			(append new old))
		#'(lambda (x y) (< (first x) (first y))))))

(defun path-length (path)
  (if (endp (rest path))
      0
      (+ (straight-line-distance (first path) (second path))
         (path-length (rest path)))))

;;; A*

(defun A* (start finish) (generalized-search start finish #'queue-A*))

(defun queue-A* (new old finish)
  "
  Purpose:	Sorts entire queue by accumulated + bird-flies distance
  Remarks:	Avoids recalculating distances while sorting.
  "
  (mapcar #'second
	  (sort (mapcar #'(lambda (path)
			    (list (minimum-distance path finish) path))
			(filter-out-redundant-paths new old))
    #'(lambda (x y) (< (first x) (first y))))))

(defun minimum-distance (path finish)
  (+ (path-length path)
     (straight-line-distance (first path) finish)))

(defun filter-out-redundant-paths (candidates paths)
  (dolist (candidate candidates paths)
    (let ((control-switch nil))
      ;;Check each candidate path against those accepted so far:
      (dolist (path paths)
	(let ((redundant-path (member (first candidate) path)))
	  ;;If terminal place is embedded in another path, 
	  ;;check relative length, then set switch accordingly:
	  (when redundant-path
	    (if (< (path-length candidate)
		   (path-length redundant-path))
		(setf control-switch 'replace-old-path)
	      (setf control-switch 'reject-candidate))
	    (return))))
      ;;Ignore the candidate path, ammend an already accepted path,
      ;;or add the candidate path:
      (setf paths
	    (case control-switch
	      (reject-candidate paths)
	      (replace-old-path
		(mapcar #'(lambda (path) (substitute-path candidate path))
			paths))
	      (otherwise (cons candidate paths)))))))

(defun substitute-path (short long)
  ;;Splice in the sorter path to a common place:
  (cond ((endp short) long)
	((eq (first short) (first long)) short)
	(t (cons (first long) (substitute-path short (rest long))))))
    
	      
