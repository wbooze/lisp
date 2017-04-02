#-(and) "

P09 (**) Pack consecutive duplicates of list elements into sublists.
    If a list contains repeated elements they should be placed in separate sublists.
   
    Example:
    * (pack '(a a a a b c c a a d e e e e))
    ((A A A A) (B) (C C) (A A) (D) (E E E E))
"


;; Ill choosen name...
(defun pack (list)
  (group list))



;; Nice recursive solution:

(defun group (list)
  (labels ((group-run (element group list)
             (cond
               ((null list) (list (cons element result)))
               ((eql element (first list)) (group-run element (cons element group) (rest list)))
               (t (cons (cons element group) (group-run (first list) '() (rest list)))))))
    (if (null list)
        '()
        (group-run (first list) '() (rest list)))))



;; Smartass solution, using Common Lisp reduce:

(defun group (list)
  (reduce (lambda (item result)
            (cond
              ((endp result)                     (list (list item)))
              ((eql (first (first result)) item) (cons (cons item (first result))
                                                       (rest result)))
              (t                                 (cons (list item) result))))
          list
          :from-end t
          :initial-value '()))


;;;; THE END ;;;;
