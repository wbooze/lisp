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

(in-listener (example1 *alphabet* :n-columns 10 :x-spacing 10 :y-spacing 10))

(defun example2 (&key (stream *standard-output*) y-spacing x-spacing)
  (formatting-table (stream :y-spacing y-spacing :x-spacing x-spacing)
    (dotimes (i 3)
      (formatting-row (stream)
        (dotimes (j 3)
          (formatting-cell (stream)
            (draw-rectangle* stream 10 10 50 50)))))))

(example2 :y-spacing 5)

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
                                (list 0 0 (* 20 (1+ (random 3))) 
                                      10 10 (* 20 (1+ (random 3)))) 
                                :filled nil) 
            (pop items)))))))

(example3)
(dotimes (i 10) (progn (terpri) (funcall (lambda () (example3) (values))) (terpri)))

(defun example4
       (&optional (items *alphabet*)
        &key (stream *standard-output*) n-columns n-rows y-spacing x-spacing max-width max-height)
  (formatting-item-list (stream :y-spacing y-spacing :x-spacing x-spacing :n-columns n-columns :n-rows n-rows
                         :max-width max-width :max-height max-height)
    (do ()
        ((null items))
      (formatting-cell (stream)
        (format stream "~a" (pop items))))))
