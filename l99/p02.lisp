#-(and) "

P02 (*) Find the last but one box of a list.
    Example:
    * (my-but-last '(a b c d))
    (C D)
"

;; The nice, recursive solution:

(defun my-but-last (list)
  (cond
    ((endp list)                (error "Empty list"))
    ((endp (rest list))         (error "List too short"))
    ((endp (rest (rest list)))  list)
    (t                          (my-last (rest list)))))


;; The efficient, iterative solution:

(defun my-but-last (list)
  (cond
    ((endp list)                (error "Empty list"))
    ((endp (rest list))         (error "List too short"))
    (t  (loop
           :for result :on list
           :until (endp (rest (rest result)))
           :finally (return result)))))


;; The smartass, Common Lisp solution:

(defun my-but-last (list)
  (cond
    ((endp list)                (error "Empty list"))
    ((endp (rest list))         (error "List too short"))
    (t                          (last list 2))))


;;;; THE END ;;;;
