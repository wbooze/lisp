(defun proper-list? (x)
  (or (null x)
    (and (consp x)
      (proper-list? (cdr x)))))

(setf pair (cons 'a 'b))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))
(defun bfs (end queue net)
  (if (null queue)
    nil
    (let ((path (car queue)))
      (let ((node (car path)))
	(if (eql node end)
	  (reverse path)
	  (bfs end
	    (append (cdr queue)
	      (new-paths path node net))
	    net))))))

(defun bfs (end queue net)
  (if (null queue)
    nil
    (let ((path (car queue)))
      (let ((node (car path)))
	(if (eql node end)
	  (reverse path)
	  (bfs end
	    (append (cdr queue)
	      (new-paths path node net))
	    net))))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
	      (cons n path))
    (cdr (assoc node net))))


(defun bin-search (obj vec)
  (let ((len (length :vec)))
    ;;if a real vector, send it to finder
    (and (not (zerop len)) ; returns nil if empty
         (finder :obj :vec 0 (- len 1)))))

(defun finder (obj vec start end)
  (format t "~A~%" (subseq :vec :start (+ :end 1)))
  (let ((range (- :end :start)))
    (if (zerop range)
        (if (eql :obj (svref :vec :start))
            :obj
            nil)
        (let ((mid (+ :start (round (/ range 2)))))
          (let ((obj2 (svref :vec mid)))
            (if (< :obj obj2)
                (finder :obj :vec :start (- mid 1))
                (if (> :obj obj2)
                    (finder :obj :vec (+ mid 1) :end)
                    :obj)))))))

(defun tokens (str test start)
  (let ((p1 (position-if test str :start start)))
    (if p1
        (let ((p2 (position-if #'(lambda (c)
                                   (not (funcall test c)))
                               str :start p1)))
          (cons (subseq str p1 p2)
                (if p2
                    (tokens str test p2)
                    nil)))
        nil)))

(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\   ))))

