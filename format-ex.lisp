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
    (window-clear *standard-output*)
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
                        (dotimes (i 1) (dolist (x complement) (format t "~a " x)))))))))))))))

;;some tries
(defun sample-format2 ()
  (formatting-table (t)
    (window-clear *standard-output*)
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
                            (dotimes (i 1) (dolist (x complement) (format t "~a " x)))))))))))))))))

;; still not good
(defun sample-format3 ()
  (formatting-table (t)
    (window-clear *standard-output*)
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
                            (dotimes (i 1) (dolist (x complement) (format t "~a " x)))))))))))))))))

;; base level done
;; but still additive composition of outputs with borders problematic
;; defaults not good ?
;; sample calculations in graph-formatting.lisp wrong ?
;; abstractions hosed ?
;; or are there still missing methods ?

(defun sample-format4 ()
  (formatting-table (t)
    (window-clear *standard-output*)
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
                                        (dolist (x complement) (format t "~a " x))))))))))))))))))))))

(defun sample-format5 ()
  (window-clear *standard-output*)
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
                          (dotimes (elem 1) (dolist (x complement) (format t "~a " x))))))))))))))))


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
    (formatting-table (t)
      (formatting-item-list (t)
        (formatting-cell (t)
          (dolist (x '(a b c d))
            (surrounding-output-with-border (t :ink +darkolivegreen+ :filled t)
              (terpri)
              (princ x))))))))
