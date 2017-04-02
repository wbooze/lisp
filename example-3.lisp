(defun example3 (&optional (items *alphabet*)    
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
	    (clim:draw-polygon* stream  
	      (list 0 0 (* 10 (1+ (random 3))) 
		5 5 (* 10 (1+ (random 3)))) 
	      :filled nil) 
	    (pop items)))))))

(example3 *alphabet* :stream *standard-output*)
