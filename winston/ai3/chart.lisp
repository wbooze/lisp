;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

(provide 'chart)

(defvar *pause-switch* t)

(defmacro xls (extension &rest forms)
  `(let ((file (merge-pathnames ,(format nil ".~a" extension)
				,(gmacs::get-my-pathname))))
     (with-open-file (output file :direction :output)
       (let ((values (list ,@forms)))
	 (dolist (l values)
	   (print-number-list l output))
	 (terpri output)
	 (apply #'quick values)))))

(defun print-number-list (list &optional (stream t))
  (format stream "~&")
  (dolist (n list)
    (format stream "~a	" (if (stringp n) n (f2 n)))))
  
(defun quick (&rest number-lists &aux title)
  ;;Remove file name, if any:
  (when (numberp (first number-lists)) (pop number-lists))
  ;;Remove title, if any:
  (when (stringp (first number-lists)) (setf title (pop number-lists)))
  ;;Remove x-axis numbers, if any:
  (when (and (stringp (first (first number-lists)))
	     (zerop (length (first (first number-lists)))))
    (pop number-lists))
  ;;Remove labels, if any:
  #+comment
  (setf number-lists
	(mapcar #'(lambda (e) (if (stringp (first e)) (rest e) e))
		number-lists))
  (let* ((l (apply #'max (mapcar #'length number-lists)))
	 (mx (max 0 (apply #'max (mapcar #'(lambda (e) (funcall #'maxs e))
					 number-lists))))
	 (mn (min 0 (apply #'min (mapcar #'(lambda (e) (funcall #'mins e))
					 number-lists))))
	 (df (- mx mn))
	 (zr (floor (- 20 (* (/ (- mn) df) 20)))))
    (send *terminal-io* :clear-screen)
    (dotimes (n 79)
      (send *terminal-io* :set-cursorpos n zr)
      (write-char #\- *terminal-io*))
    (dotimes (n (length number-lists))
      (let (char (numbers (nth n number-lists)))
	(if (stringp (first numbers))
	    (setf char (pop numbers))
	  (setf char #\*))
	(dotimes (o (length numbers))
	  (let ((y (- 20 (floor (* (/ (- (nth o numbers) mn) df) 20)))))
	    (send *terminal-io* :set-cursorpos (floor (* o 79) l) y)
	    (unless (= y zr)
	      (write-char
		(coerce char 'character)
		*terminal-io*)))))))
  (when *pause-switch* (pause title))
  (values))

(defun maxs (l)
  (if (stringp (first l))
      (reduce #'max (rest l))
    (reduce #'max l)))

(defun mins (l)
  (if (stringp (first l))
      (apply #'min (rest l))
    (apply #'min l)))

(defun make-x-axis (from to step &optional text &aux result)
  (when text (push text result))
  (loop
    (push from result)
    (when (> (incf from step) to) (return (reverse result)))))

(defun make-data-points (ratings from to step &optional text &aux result)
  (when text (push text result))
  (setf ratings (mapcar #'reverse ratings))
  (loop
    (push (let ((r (assoc from ratings :test #'=))) (if r (second r) 0))
	  result)
    (when (> (incf from step) to) (return (reverse result)))))




