#-(and) "

P12 (**) Decode a run-length encoded list.
    Given a run-length code list generated as specified in problem P11. Construct its uncompressed version.
"


;; Nice functional solution:

(defun decode (encoded)
  (mapcan (lambda (item)
            (if (atom item)
                (list item)
                (make-list (first item) :initial-element (second item))))
          encoded))

;; Iterative solution:

(defun decode (encoded)
  (loop
     :with result = '()
     :for item :in (reverse encoded)
     :do (if (atom item)
             (push item result)
             (loop
                :repeat (first item)
                :do (push (second item) result)))
     :finally (return result)))




#-(and)
(let ((list '(a a a a b c c a a d e e e e)))
  (assert (equal list (decode (encode          list))))
  (assert (equal list (decode (encode-modified list))))
  :success)

;;;; THE END ;;;;
