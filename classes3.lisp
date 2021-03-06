(defclass article ()
  ((title :accessor article-title :initarg :title)
   (author :accessor article-author :initarg :author)))

(defclass computer-article (article) ())
(defclass business-article (article) ())
(defclass political-article (article) ())

(defclass friend ()
  ((name :accessor friend-name :initarg :name)))

(defclass hacker-friend (friend) ())
(defclass entrepreneur-friend (friend) ())
(defclass philosopher-friend (friend) ())

(defvar friends)
(defvar articles)

(setf articles
      (list (make-instance 'business-article :title "Memory Prices Down")
            (make-instance 'computer-article :title "Memory Speeds Up")
            (make-instance 'political-article :title "Memory Impugned")))

(setf friends
      (list (make-instance 'hacker-friend :name 'Dan)
            (make-instance 'hacker-friend :name 'Gerry)
            (make-instance 'entrepreneur-friend :name 'Philip)
            (make-instance 'philosopher-friend :name 'David)))

(defun print-notification (article friend)
  (format t "~%Tell ~a about \"~a.\""
          (friend-name friend)
          (article-title article))
  t)

(defmethod process ((friend hacker-friend)
                    (article computer-article))
  (print-notification article friend))

(defmethod process ((friend entrepreneur-friend)
                    (article business-article))
  (print-notification article friend))

(defmethod process ((friend philosopher-friend)
                    (article article))
  (print-notification article friend))

(defmethod process (friend article))

(dolist (friend friends)
  (dolist (article articles)
    (process friend article)))

(defclass stocks-article (business-article) ())

(defclass new-stocks-article (stocks-article) ())

(defmethod process ((friend entrepreneur-friend) (article stocks-article)))

(defmethod process ((friend entrepreneur-friend)
                    (article new-stocks-article))
  (print-notification article friend))

(process
 (make-instance 'entrepreneur-friend :name 'jack)
 (make-instance 'stocks-article :title "Stock Prices Up"))

(process
 (make-instance 'entrepreneur-friend :name 'jill)
 (make-instance 'new-stocks-article :title "New-Stock Prices Up"))

