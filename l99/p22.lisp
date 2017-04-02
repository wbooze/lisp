#-(and) "

P22 (*) Create a list containing all integers within a given range.
    If first argument is smaller than second, produce a list in decreasing order.
    Example:
    * (range 4 9)
    (4 5 6 7 8 9)
"


;; Recursive solution:

(defun range (start last)
  (if (<= start last)
      (cons start (range (1+ start) last))
      '()))


;; Recursive solution, with an accumulator:

(defun range (start last)
  (labels ((acc (current result)
             (if (<= current last)
                 (acc (1+ current) (cons current result))
                 (nreverse result))))
    (acc start '())))


;; Smartass solution, using Common  Lisp:

(defun range (start last)
  (loop :for i :from start :to last :collect i))

;;;; THE END ;;;;
