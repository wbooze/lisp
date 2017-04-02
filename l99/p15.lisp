#-(and) "

P15 (**) Replicate the elements of a list a given number of times.
    Example:
    * (repli '(a b c) 3)
    (A A A B B B C C C)
"

;; Nice functional solution:

(defun repli (list count)
  (mapcan (lambda (item) (make-list count :initial-element item)) list))


;; Iterative solution:

(defun repli (list count)
  (loop :for item :in list :nconc (loop :repeat count :collect item)))


;; Assembler solution:

(defun repli (list count)
  (let* ((result (cons :head nil)) ; to avoid a test
         (tail   result)
         i item)
    (tagbody
       (go :begin)
     :next
       (setq list (cdr list))
     :begin
       (if (endp list)
           (go :done))
       (setq item (first list))
       (setq i count)
     :add-them
       (if (zerop i)
           (go :next))
       (rplacd tail (cons item nil))
       (setq tail (cdr tail))
       (setq i (1- i))
       (go :add-them)
     :done)
    (cdr result)))


;; Late student recursive solution:

(defun repli (list count)
  (labels ((repli-one (item count)
             (if (zerop count)
                 '()
                 (cons item (repli-one item (1- count))))))
    (if (endp list)
        '()
        (append (repli-one (first list) count)
                (repli (rest list) count)))))

;;;; THE END ;;;;
