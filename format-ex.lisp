(defmacro without-notes (&rest body)
`(locally
    (handler-bind
     ((sb-ext:compiler-note #'muffle-warning)
     ,@body))))

(defmacro without-warnings (&rest body)
  `(locally
    (handler-bind
     ((style-warning #'muffle-warning)
     ,@body))))

(declaim (sb-ext:muffle-conditions style-warning))

;;some attempts!
(let ((list '(a c t g u)))
	(terpri)
	(with-drawing-options 
	 (t :text-size 20)
	 (with-text-family 
		(t :fix)
		(formatting-item-list 
		 (t)
		 (formatting-cell 
			(t)
			(surrounding-output-with-border 
			 (t :shape :rounded)
			 (surrounding-output-with-border 
				(t :shape :rectangle)
				(dotimes 
					(seq 2)
					(dotimes 
						(i 1)
						(dolist 
							(x list)
							(format t "~a " x))))))))))
	 
	 (terpri)
	 
	 (with-drawing-options 
		(t :text-size 20)
		(with-text-family 
		 (t :fix)
		 (formatting-item-list 
			(t)
			(formatting-cell 
			 (t)
			 (dotimes 
				 (seq 2)
				 (dotimes 
					 (i 1)
					 (dolist 
						 (x list)
						 (declare (ignore x))
						 (format t "| "))))))))
	 
	 (terpri)
	 
	 (with-drawing-options 
		(t :text-size 20)
		(with-text-family 
		 (t :fix)
		 (formatting-item-list 
			(t)
			(formatting-cell 
			 (t)
			 (surrounding-output-with-border 
				(t :shape :rectangle)
				(dotimes 
					(seq 2)
					(dotimes 
						(i 1)
						(dolist 
							(x (reverse list))
							(format t "~a " x))))))))))

;;base level
(defun sample-format1 ()
  (formatting-table (t)
    (formatting-row (t)
      (formatting-cell (t)
        (surrounding-output-with-border (t)
          (let ((list '(a c t g))
								(complement '(t g a c)))
            (with-drawing-options (t :text-size 20)
              (with-text-family (t :fix)
                (formatting-table (t)
                  (formatting-row (t)
                    (formatting-cell (t)
                      (dotimes (seq 2) (dotimes (i 1) (dolist (x list) (format t "~a " x)))))))))
            (terpri)
            (with-drawing-options (t :text-size 20)
              (with-text-family (t :fix)
                (formatting-table (t)
                  (formatting-row (t)
                    (formatting-cell (t)
                      (dotimes (seq 2)
                        (dotimes (i 1) (dolist (x list) (declare (ignore x)) (format t "| ")))))))))
            (terpri)
            (with-drawing-options (t :text-size 20)
              (with-text-family (t :fix)
                (formatting-table (t)
                  (formatting-row (t)
                    (formatting-cell (t)
                      (dotimes (seq 2)
                        (dotimes (i 1) (dolist (x complement) (format t "~a " x))))))))))))))
  (values))

;;some tries
(defun sample-format2 ()
  (formatting-table (t)
    (formatting-row (t)
      (formatting-cell (t)
        (surrounding-output-with-border (t)
          (let ((list '(a c t g))
                (complement '(t g a c)))
            (with-drawing-options (t :text-size 20)
              (with-text-family (t :fix)
                (formatting-table (t)
                  (formatting-row (t)
                    (formatting-cell (t)
                      (surrounding-output-with-border (t)
                        (dotimes (seq 2) (dotimes (i 1) (dolist (x list) (format t "~a " x)))))))))
              (terpri)
              (with-drawing-options (t :text-size 20)
                (with-text-family (t :fix)
                  (formatting-table (t)
                    (formatting-row (t)
                      (formatting-cell (t)
                        (dotimes (seq 2)
                          (dotimes (i 1) (dolist (x list) (declare (ignore x)) (format t "| ")))))))))
              (terpri)
              (with-drawing-options (t :text-size 20)
                (with-text-family (t :fix)
                  (formatting-table (t)
                    (formatting-row (t)
                      (formatting-cell (t)
                        (surrounding-output-with-border (t)
                          (dotimes (seq 2)
                            (dotimes (i 1) (dolist (x complement) (format t "~a " x))))))))))))))))
  (values))

;; still not good
(defun sample-format3 ()
  (formatting-table (t)
    (formatting-row (t)
      (formatting-cell (t)
        (surrounding-output-with-border (t)
          (let ((list '(a c t g))
                (complement '(t g a c)))
            (with-drawing-options (t :text-size 20)
              (with-text-family (t :fix)
                (formatting-table (t)
                  (formatting-row (t)
                    (formatting-cell (t)
                      (surrounding-output-with-border (t)
                        (dotimes (seq 2) (dotimes (i 1) (dolist (x list) (format t "~a " x)))))))))
              (terpri)
              (with-drawing-options (t :text-size 20)
                (with-text-family (t :fix)
                  (formatting-table (t)
                    (formatting-row (t)
                      (formatting-cell (t)
																			 (surrounding-output-with-border (t)
                        (dotimes (seq 2)
                          (dotimes (i 1) (dolist (x list) (declare (ignore x)) (format t "| "))))))))))
              (terpri)
              (with-drawing-options (t :text-size 20)
                (with-text-family (t :fix)
                  (formatting-table (t)
                    (formatting-row (t)
                      (formatting-cell (t)
                        (surrounding-output-with-border (t)
                          (dotimes (seq 2)
                            (dotimes (i 1) (dolist (x complement) (format t "~a " x))))))))))))))))
  (values))

;; base level done
;; but still additive composition of outputs with borders problematic
;; defaults not good ?
;; sample calculations in graph-formatting.lisp wrong ?
;; abstractions hosed ?
;; or are there still missing methods ?

(defun sample-format4 ()
  (formatting-table (t)
    (formatting-row (t)
      (formatting-cell (t)
        (surrounding-output-with-border (t)
          (let ((list '(a c t g)) (complement '(t g a c)))
            (with-drawing-options (t :text-size 20)
              (with-text-family (t :fix)
                (surrounding-output-with-border (t)
                  (formatting-table (t)
                    (formatting-row (t)
                      (formatting-item-list (t)
                        (formatting-cell (t)
                          (surrounding-output-with-border (t)
                            (dotimes (seq 2) (dotimes (i 1) (dolist (x list) (format t "~a " x))))))))
                    (terpri)
                    (with-drawing-options (t :text-size 20)
                      (with-text-family (t :fix)
                        (formatting-table (t)
                          (formatting-row (t)
                            (formatting-item-list (t)
                              (formatting-cell (t)
                                (surrounding-output-with-border (t)
                                  (dotimes (seq 2)
                                    (dotimes (i 1)
                                      (dolist (x list) (declare (ignore x)) (format t "| "))))))))))
                      (terpri)
                      (with-drawing-options (t :text-size 20)
                        (with-text-family (t :fix)
                          (formatting-table (t)
                            (formatting-row (t)
                              (formatting-item-list (t)
                                (formatting-cell (t)
                                  (surrounding-output-with-border (t)
                                    (dotimes (seq 2)
                                      (dotimes (i 1)
                                        (dolist (x complement) (format t "~a " x)))))))))))))))))))))
  (values))

(defun sample-format5 ()
  (formatting-table (t)
    (formatting-row (t)
      (formatting-cell (t)
        (surrounding-output-with-border (t)
          (let ((list '(a c t g)) (complement '(t g a c)))
            (with-drawing-options (t :text-size 20)
              (with-text-family (t :fix)
                (formatting-table (t)
                  (formatting-row (t)
                    (formatting-cell (t)
                      (surrounding-output-with-border (t)
                        (dotimes (seq 2) (dotimes (elem 1) (dolist (x list) (format t "~a " x))))))))))
            (terpri)
            (with-drawing-options (t :text-size 20)
              (with-text-family (t :fix)
                (formatting-table (t)
                  (formatting-row (t)
                    (formatting-cell (t)
                      (surrounding-output-with-border (t)
                        (dotimes (seq 2)
                          (dotimes (elem 1) (dolist (x list) (declare (ignore x)) (format t "| "))))))))))
            (terpri)
            (with-drawing-options (t :text-size 20)
              (with-text-family (t :fix)
                (formatting-table (t)
                  (formatting-row (t)
                    (formatting-cell (t)
                      (surrounding-output-with-border (t)
                        (dotimes (seq 2)
                          (dotimes (elem 1) (dolist (x complement) (format t "~a " x)))))))))))))))
  (values))


;; minimal setup to print bordered things side by side
(formatting-item-list (t)
  (dolist (x '(a b c d))
    (formatting-cell (t)
      (surrounding-output-with-border (t)
        (princ x)))))

;;equal to some of here

(formatting-table (t)
  (formatting-row (t)
    (dolist (x '(a b c d))
      (formatting-cell (t)
        (surrounding-output-with-border (t)
          (princ x))))))
;; observe where the dolist is in each of these!

(formatting-table (t)
  (formatting-item-list (t)
    (dolist (x '(a b c d))
      (formatting-cell (t)
        (surrounding-output-with-border (t)
          (princ x))))))

;; and contrast with

(surrounding-output-with-border (t)
  (terpri)
  (formatting-table (t)
    (formatting-item-list (t)
      (formatting-cell (t)
        (dolist (x '(a b c d))
          (surrounding-output-with-border (t)
            (terpri)
            (princ x)))))))

;; and this one!

(surrounding-output-with-border (t)
  (terpri)
  (formatting-table (t)
    (formatting-item-list (t)
      (formatting-cell (t)
        (dolist (x '(a b c d))
          (surrounding-output-with-border (t)
            (princ x)))))))

;; some coloring/filling demo!

(with-drawing-options (t :ink +white+)
  (surrounding-output-with-border (t :ink +black+ :filled t)
   (terpri)
   (terpri)
    (formatting-table (t)
      (formatting-item-list (t)
        (formatting-cell (t)
          (dolist (x '(a b c d))
            (surrounding-output-with-border (t :ink +darkolivegreen+ :filled t)
             (terpri)
              (princ x)))))))
                      (values))

(defun sample-format6 ()
  (let ((list '(a c t g)) (complement '(t g a c)))
    (surrounding-output-with-border (t)
    (with-drawing-options (t :text-size 20)
      (with-text-family (t :fix)
       (formatting-table (t)
                         
        (formatting-row (t)
         (dolist (x list)
           (formatting-cell (t)
             (surrounding-output-with-border (t :align-x :center) 
                                             (princ x)))))
          
        (formatting-row (t)
         (dolist (x list) (declare (ignore x)) 
          (formatting-cell (t :align-x :center)
                           (princ "|"))))

        (formatting-row (t)
         (dolist (x complement)
          (formatting-cell (t)
           (surrounding-output-with-border (t :align-x :center)
                                           (princ x))))))))))
  (terpri)
  (values))

(progn
  (terpri)
  (sample-format1)
  (terpri)
  (sample-format2)
  (terpri)
  (sample-format3)
  (terpri)
  (sample-format4)
  (terpri)
  (sample-format5)
  (terpri)
  (sample-format6))


(defun sample-format6 (&optional (n 1))
  (let ((list '(a c t g)) (complement '(t g a c)))
    (surrounding-output-with-border (t)
    (with-drawing-options (t :text-size 20)
      (with-text-family (t :fix)

       (formatting-table (t)
                         
        (formatting-row (t)
         (dolist (x list)
           (formatting-cell (t)
             (surrounding-output-with-border (t :align-x :center) 
                                             (dotimes (seq n) (princ x))))))
          
        (formatting-row (t)
         (dolist (x list) (declare (ignore x)) 
          (formatting-cell (t :align-x :center)
                           (dotimes (seq n) (princ "|")))))

        (formatting-row (t)
         (dolist (x complement)
          (formatting-cell (t)
           (surrounding-output-with-border (t :align-x :center)
                                           (dotimes (seq n) (princ x)))))))))))
  (terpri)
  (values))

(terpri)
(sample-format6 2)

(defun sample-format6 (&optional (n 1))
  (let ((list '(a c t g)) (complement '(t g a c)))
    (with-drawing-options (t :text-size 20)
      (with-text-family (t :fix)
    (surrounding-output-with-border (t)
       (formatting-table (t)
        (formatting-row (t)
         (dolist (x list) (dotimes (seq n)
           (formatting-cell (t)
             (surrounding-output-with-border (t :align-x :center) 
                                             (princ x))))))
          
        (formatting-row (t)
         (dolist (x list) (declare (ignore x)) (dotimes (seq n)
          (formatting-cell (t :align-x :center)
                            (princ "|")))))

        (formatting-row (t)
         (dolist (x complement) (dotimes (seq n)
          (formatting-cell (t)
           (surrounding-output-with-border (t :align-x :center)
                                           (princ x)))))))))))
  (terpri)
  (values))

(sample-format6 2)

(defun sample-format6 (&optional (n 1))
  (flet ((make-random-color ()
            (make-rgb-color 
             (/ (+ 50 (random (- 255 50))) 255)
             (/ (+ 50 (random (- 255 50))) 255)
             (/ (+ 50 (random (- 255 50))) 255))))
    (let ((list '(a c t g)) (complement '(t g a c)) (*random-state* (make-random-state t)) 
          (color-a (make-random-color)) (color-c (make-random-color)) (color-t (make-random-color)) (color-g (make-random-color)))

(with-room-for-graphics (t)
(with-drawing-options (t :text-size 20)
      (with-text-family (t :fix)
    (surrounding-output-with-border (t)
       (formatting-table (t)
        (formatting-row (t)
          (dolist (x list) (declare (ignore x)) (dotimes (seq n)
            (formatting-cell (t)
             (surrounding-output-with-border (t :align-x :center :ink (cond ((eq x 'a) color-a)
                                                   ((eq x 't) color-t)
                                                   ((eq x 'g) color-g)
                                                   ((eq x 'c) color-c)))
                                           (princ x))))))
        (formatting-row (t)
         (dolist (x list) (declare (ignore x)) (dotimes (seq n)
          (formatting-cell (t :align-x :center)
                           (princ "|")))))
        (formatting-row (t)
         (dolist (x complement) (declare (ignore x)) (dotimes (seq n)
          (formatting-cell (t)
           (surrounding-output-with-border (t :align-x :center :ink (cond ((eq x 'a) color-a)
                                                   ((eq x 't) color-t)
                                                   ((eq x 'g) color-g)
                                                   ((eq x 'c) color-c)))
                                        (princ x)))))))))))))
  (terpri)
  (values))

(sample-format6 2)

(defun sample-format6 (&optional (n 1))
  (flet ((make-random-color ()
            (make-rgb-color 
             (/ (+ 50 (random (- 255 50))) 255)
             (/ (+ 50 (random (- 255 50))) 255)
             (/ (+ 50 (random (- 255 50))) 255))))
    (let ((list '(a c t g)) (complement '(t g a c)) (*random-state* (make-random-state t)) 
          (color-a (make-random-color)) (color-c (make-random-color)) (color-t (make-random-color)) (color-g (make-random-color)))

(with-room-for-graphics (t)
    (with-drawing-options (t :text-size 20)
      (with-text-family (t :fix)
       (formatting-table (t :y-spacing 10)
        (formatting-row (t)
         (dolist (x list) (declare (ignore x)) (dotimes (seq n)
           (formatting-cell (t)
             (surrounding-output-with-border (t :align-x :center :shape :oval) 
               (with-drawing-options (t :ink (cond ((eq x 'a) color-a)
                                                   ((eq x 't) color-t)
                                                   ((eq x 'g) color-g)
                                                   ((eq x 'c) color-c)))
                                     (princ x)))))))
        (formatting-row (t)
         (dolist (x list) (declare (ignore x)) (dotimes (seq n)
          (formatting-cell (t :align-x :center)
                            (princ "|")))))
        (formatting-row (t)
         (dolist (x complement) (declare (ignore x)) (dotimes (seq n)
          (formatting-cell (t)
           (surrounding-output-with-border (t :align-x :center :shape :oval)
            (with-drawing-options (t :ink (cond ((eq x 'a) color-a)
                                             ((eq x 't) color-t)
                                             ((eq x 'g) color-g)
                                             ((eq x 'c) color-c))) 
                                           (princ x)))))))))))))
  (terpri)
  (values))

(sample-format6 2)

(defun sample-format6 (&optional (n 1) m)
  (flet ((make-random-color ()
            (make-rgb-color 
             (/ (+ 50 (random (- 255 50))) 255)
             (/ (+ 50 (random (- 255 50))) 255)
             (/ (+ 50 (random (- 255 50))) 255))))
    (let ((list '(a c t g)) (complement '(t g a c)) (*random-state* (make-random-state t)) 
          (color-a (make-random-color)) (color-c (make-random-color)) (color-t (make-random-color)) (color-g (make-random-color)))
      
(with-room-for-graphics (t)
(surrounding-output-with-border (t)
    (with-drawing-options (t :text-size 20)
      (with-text-family (t :fix)
       (formatting-table (t)
        (formatting-row (t)
         (if (not m)
         (dolist (x list) (declare (ignore x)) (dotimes (seq n)
           (formatting-cell (t)
             (surrounding-output-with-border (t :align-x :center) 
               (with-drawing-options (t :ink (cond ((eq x 'a) color-a)
                                                   ((eq x 't) color-t)
                                                   ((eq x 'g) color-g)
                                                   ((eq x 'c) color-c)))
                                     (princ x))))))
         (dolist (x list) (declare (ignore x))
           (formatting-cell (t)
             (surrounding-output-with-border (t :align-x :center) 
               (with-drawing-options (t :ink (cond ((eq x 'a) color-a)
                                                   ((eq x 't) color-t)
                                                   ((eq x 'g) color-g)
                                                   ((eq x 'c) color-c)))
                                     (dotimes (seq n) (princ x))))))))
         
        (formatting-row (t)
         (if (not m)
         (dolist (x list) (declare (ignore x)) (dotimes (seq n)
          (formatting-cell (t :align-x :center)
                            (princ "|"))))
         (dolist (x list) (declare (ignore x))
          (formatting-cell (t :align-x :center)
                            (dotimes (seq n) (princ "|"))))))
        (formatting-row (t)
         (if (not m)
         (dolist (x complement) (declare (ignore x)) (dotimes (seq n)
          (formatting-cell (t)
           (surrounding-output-with-border (t :align-x :center)
            (with-drawing-options (t :ink (cond ((eq x 'a) color-a)
                                             ((eq x 't) color-t)
                                             ((eq x 'g) color-g)
                                             ((eq x 'c) color-c))) 
                                           (princ x))))))
         (dolist (x complement) (declare (ignore x))
          (formatting-cell (t)
           (surrounding-output-with-border (t :align-x :center)
            (with-drawing-options (t :ink (cond ((eq x 'a) color-a)
                                             ((eq x 't) color-t)
                                             ((eq x 'g) color-g)
                                             ((eq x 'c) color-c))) 
                                           (dotimes (seq n) (princ x)))))))))))))))
  (terpri)
  (values))

(sample-format6 4)
(sample-format6 4 t)

(locally
(handler-bind ((style-warning #'muffle-warning))
(defun sample-format6 (&optional (n 1) m)
  (flet ((make-random-color ()
            (make-rgb-color 
             (/ (+ 50 (random (- 255 50))) 255)
             (/ (+ 50 (random (- 255 50))) 255)
             (/ (+ 50 (random (- 255 50))) 255))))
    (let ((list '(a c t g)) (complement '(t g a c)) (*random-state* (make-random-state t)) 
          (color-a (make-random-color)) (color-c (make-random-color)) (color-t (make-random-color)) (color-g (make-random-color)))

(surrounding-output-with-border (t)
(with-room-for-graphics (t)
    (with-drawing-options (t :text-size 20)
      (with-text-family (t :fix)
       (formatting-table (t :x-spacing "M" :y-spacing "|" :equalize-column-widths t)
        (formatting-row (t)
         (if (not m)
         (dolist (x list) (declare (ignore x)) (dotimes (seq n)
           (formatting-cell (t)
             (surrounding-output-with-border (t :align-x :center :align-y :center :shape :rectangle :ink +black+ :filled t) 
               (with-drawing-options (t :ink (cond ((eq x 'a) color-a)
                                                   ((eq x 't) color-t)
                                                   ((eq x 'g) color-g)
                                                   ((eq x 'c) color-c)))
                                     (princ x))))))
         (dolist (x list) (declare (ignore x))
           (formatting-cell (t)
             (surrounding-output-with-border (t :align-x :center :align-y :center :shape :rectangle :ink +black+ :filled t) 
               (with-drawing-options (t :ink (cond ((eq x 'a) color-a)
                                                   ((eq x 't) color-t)
                                                   ((eq x 'g) color-g)
                                                   ((eq x 'c) color-c)))
                                     (dotimes (seq n) (princ x))))))))
         
        (formatting-row (t)
         (if (not m)
         (dolist (x list) (declare (ignore x)) (dotimes (seq n)
          (formatting-cell (t :align-x :center :align-y :center)
                            (princ "|"))))
         (dolist (x list) (declare (ignore x))
          (formatting-cell (t :align-x :center :align-y :center)
                            (dotimes (seq n) (princ "|"))))))
        (formatting-row (t)
         (if (not m)
         (dolist (x complement) (declare (ignore x)) (dotimes (seq n)
          (formatting-cell (t)
           (surrounding-output-with-border (t :align-x :center :align-y :center :shape :rectangle :ink +black+ :filled t)
            (with-drawing-options (t :ink (cond ((eq x 'a) color-a)
                                             ((eq x 't) color-t)
                                             ((eq x 'g) color-g)
                                             ((eq x 'c) color-c))) 
                                           (princ x))))))
         (dolist (x complement) (declare (ignore x))
          (formatting-cell (t)
           (surrounding-output-with-border (t :align-x :center :align-y :center :shape :rectangle :ink +black+ :filled t)
            (with-drawing-options (t :ink (cond ((eq x 'a) color-a)
                                             ((eq x 't) color-t)
                                             ((eq x 'g) color-g)
                                             ((eq x 'c) color-c))) 
                                           (dotimes (seq n) (princ x)))))))))))))))
  (terpri)
  (values))))

(sample-format6 4)
(sample-format6 4 t)
(sample-format6 2 t)
(sample-format6 2)


(locally
(handler-bind ((style-warning #'muffle-warning))
(defun sample-format6 (&optional (n 1) m s)
  (flet ((make-random-color ()
            (make-rgb-color 
             (/ (+ 50 (random (- 255 50))) 255)
             (/ (+ 50 (random (- 255 50))) 255)
             (/ (+ 50 (random (- 255 50))) 255))))
    (let ((list '(a c t g)) (complement '(t g a c)) (*random-state* (make-random-state t)) 
          (color-a (make-random-color)) (color-c (make-random-color)) (color-t (make-random-color)) (color-g (make-random-color)))

(surrounding-output-with-border (t)
(with-room-for-graphics (t)
    (with-drawing-options (t :text-size 20)
      (with-text-family (t :fix)
       (formatting-table (t :multiple-columns s :equalize-column-widths t)
        (formatting-row (t)
         (if (not m)
         (dolist (x list) (declare (ignore x)) (dotimes (seq n)
           (formatting-cell (t)
             (surrounding-output-with-border 
              (t :align-x :center :align-y :center :shape (if m :rectangle :oval) :ink +black+ :filled t) 
               (with-drawing-options (t :ink (cond ((eq x 'a) color-a)
                                                   ((eq x 't) color-t)
                                                   ((eq x 'g) color-g)
                                                   ((eq x 'c) color-c)))
                                     (princ x))))))
         (dolist (x list) (declare (ignore x))
           (formatting-cell (t)
             (surrounding-output-with-border 
              (t :align-x :center :align-y :center :shape (if m :rectangle :oval) :ink +black+ :filled t) 
               (with-drawing-options (t :ink (cond ((eq x 'a) color-a)
                                                   ((eq x 't) color-t)
                                                   ((eq x 'g) color-g)
                                                   ((eq x 'c) color-c)))
                                     (dotimes (seq n) (princ x))))))))

        (formatting-row (t)
         (if (not m)
         (dolist (x list) (declare (ignore x)) (dotimes (seq n)
          (formatting-cell (t :align-x :center :align-y :center)
                            (princ "|"))))
         (dolist (x list) (declare (ignore x))
          (formatting-cell (t :align-x :center :align-y :center)
                            (dotimes (seq n) (princ "|"))))))                         

        (formatting-row (t)
         (if (not m)
         (dolist (x complement) (declare (ignore x)) (dotimes (seq n)
          (formatting-cell (t)
           (surrounding-output-with-border 
            (t :align-x :center :align-y :center :shape (if m :rectangle :oval) :ink +black+ :filled t)
            (with-drawing-options (t :ink (cond ((eq x 'a) color-a)
                                             ((eq x 't) color-t)
                                             ((eq x 'g) color-g)
                                             ((eq x 'c) color-c))) 
                                           (princ x))))))
         (dolist (x complement) (declare (ignore x))
          (formatting-cell (t)
           (surrounding-output-with-border 
            (t :align-x :center :align-y :center :shape (if m :rectangle :oval) :ink +black+ :filled t)
            (with-drawing-options (t :ink (cond ((eq x 'a) color-a)
                                             ((eq x 't) color-t)
                                             ((eq x 'g) color-g)
                                             ((eq x 'c) color-c))) 
                                           (dotimes (seq n) (princ x)))))))))))))))
  (terpri)
  (values))))

(sample-format6 4)
(sample-format6 4 t)
(sample-format6 4 t t)

(in-listener
 (funcall
  (lambda ()
    (terpri)
    (with-room-for-graphics (t)
      (with-drawing-options (t :ink +green+)
        (formatting-table (t)
          (formatting-item-list (t :initial-spacing 40 :n-rows 2 :y-spacing "|" :x-spacing "m")
            (dolist (x '(a g t c))
              (formatting-cell (t :align-x :center :align-y :center)
                (surrounding-output-with-border (t :shape :oval :ink +black+ :filled t)
                  (princ x))))))))
    (terpri)
    (values))))
