#-(and)"

P05 (*) Reverse a list.

"

;; The naive recursive solution, O(n²):

(defun naive-recursive-reverse (list)
  (if (endp list)
      '()
      (append (naive-recursive-reverse (rest list))
              (cons (first list) '()))))


;; A tail-recursive solution (with accumulator):

(defun recursive-reverse (list)
  (labels ((rev (list acc)
     (if (endp list)
          acc
          (rev (rest list) (cons (first list) acc)))))
      (rev list '())))


;; An iterative solution:

(defun iterative-reverse-do (list)
  (let ((acc '()))
    (do ((current list (rest current)))
        ((endp current) acc)
      (setf acc (cons (first current) acc)))))


(defun iterative-reverse-loop (list)
  (loop
     :with result = '()
     :for item :in list
     :do (push item result)
     :finally (return result)))


;; The smartass, Common Lisp solution:

(defun my-reverse (list)
  (reverse list))




;; (mapcar (lambda (fun) (mapcar fun '(() (a) (a b) (a b c))))
;;         (list (function iterative-reverse-do)
;;               (function iterative-reverse-loop)
;;               (function naive-recursive-reverse)
;;               (function recursive-reverse)
;;               (function my-reverse)))
;; --> ((NIL (A) (B A) (C B A))
;;      (NIL (A) (B A) (C B A))
;;      (NIL (A) (B A) (C B A))
;;      (NIL (A) (B A) (C B A))
;;      (NIL (A) (B A) (C B A)))






;; A tail-recursive solution for the reversing of the list in place.
;; Notice that cl:nreverse may be implemented without reusing the
;; input list, cl:nreverse may just call cl:reverse.               

(defun my-nreverse (list)
  (labels ((list-reverse (reverse list)
             (if (null list)
                 reverse
                 (let ((rest  (cdr list)))
                   (setf (cdr list) reverse)
                   (list-reverse list rest)))))
    (list-reverse nil list)))


;; (let* ((list     (list 1 2 3 4))
;;        (reversed (my-nreverse list)))
;;   (values list reversed))
;; --> (1)
;;     (4 3 2 1)


;;;; THE END ;;;;
