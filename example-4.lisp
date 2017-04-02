(defvar *alphabet* '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

(defun example4
  (&key (items *alphabet*)
    (stream *standard-output*) 
    n-columns n-rows y-spacing x-spacing max-width max-height)
  (formatting-item-list (stream :y-spacing y-spacing :x-spacing x-spacing :n-columns n-columns :n-rows n-rows
			  :max-width max-width :max-height max-height)
    (do ()
      ((null items))
      (formatting-cell (stream)
	(format stream "~a" (pop items))))))

;;(example4 :stream *standard-output*)
;;(example4 :stream *standard-output* :n-columns 8)
