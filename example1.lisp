(defvar *alphabet* '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

(defun example1 (&optional (items *alphabet*)    
		  &key (stream *standard-output*) (n-columns 6)
		  y-spacing x-spacing) 
  (clim:formatting-table 
    (stream :y-spacing y-spacing                                  
      :x-spacing x-spacing) 
    (do () 
      ((null items)) 
      (clim:formatting-row (stream)  
	(do ((i 0 (1+ i))) 
	  ((or (null items) (= i n-columns)))  
	  (clim:formatting-cell (stream) 
	    (format stream "~A" (pop items))))))))

;;(example1 *alphabet* :stream *standard-output*)
;;(example1 *alphabet* :stream *standard-output* :n-columns 10 :x-spacing 10 :y-spacing 10)

(example1 *alphabet*)
(example1 *alphabet* :n-columns 10 :x-spacing 10 :y-spacing 10)
