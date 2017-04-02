(defun test-graph (win &optional p)
  (if p (window-clear win))
  (format-graph-from-roots '((a (b (d)) (c (d))))
    #'(lambda (x s) (princ (car x) s))
    #'cdr
    :stream win
    :orientation :vertical
    :merge-duplicates t
    :duplicate-key #'car)
  (force-output win))


(defun test-graph (tree &key (p nil) (win *standard-output*) (dup nil) (fun #'car) (ori :horizontal) (type nil))
  (if p
      (window-clear win))
	(terpri)
  (format-graph-from-roots 
	 (list tree)
   ;; note the tree given must be alreay in tree format else it won't work!
	 #'(lambda (x s) (princ (car x) s)) #'cdr :stream win :orientation ori
	 :graph-type type
	 :merge-duplicates dup :duplicate-key fun :type (if (eq dup t) :tree :dag))
	(terpri)
	(force-output win))
