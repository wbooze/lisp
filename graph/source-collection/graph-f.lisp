(defun |x,f(x)| (points x-min x-max f)
  "Call f repeatly to build an array tabulating f from
   x-min to x-max inclusive. Uses CLX representation so
   array is twice as long as the number of points evaluated" 
  (let ((a (make-array (* 2 points))))
    (dotimes (index points)
      (let ((x (+ x-min
	       (/ (* (- x-max x-min) index)
		  (- points 1)))))	     
	(setf (aref a (* 2 index))
	      x
	      (aref a (+ (* 2 index ) 1))
	      (funcall f x))))
    a))

(defun |x(t),y(t)| (points t-min t-max x y)
  (let ((a (make-array (* 2 points))))
    (dotimes (index points)
      (let ((tau (+ t-min
		    (/ (* (- t-max t-min) index)
		       (- points 1)))))	     
	(setf (aref a (* 2 index))
	      (funcall x tau)
	      (aref a (+ (* 2 index ) 1))
	      (funcall y tau))))
    a))

(defun |z(t)| (points t-min t-max z)
  (let ((a (make-array (* 2 points))))
    (dotimes (index points)
      (let ((z (funcall z
			(+ t-min
			   (/ (* (- t-max t-min) index)
			      (- points 1))))))
	(setf (aref a (* 2 index))
	      (realpart z)
	      (aref a (+ (* 2 index) 1))
	      (imagpart z))))
    a))
	
(defun cycloid(loop1 size1 loop2 size2)
  #'(lambda(x)(+ (* size1 (exp (* (complex 0f0 loop1) x)))
		 (* size2 (exp (* (complex 0f0 loop2) x))))))
	
	
		 
(defun bound-xy-vec(xys)
  (do ((index 0 (+ index 2))
       (x-min (aref xys 0)
	      (min x-min (aref xys index)))
       (x-max (aref xys 0)
	      (max x-max (aref xys index)))
       (y-min (aref xys 1)
	      (min y-min (aref xys (+ index 1))))
       (y-max (aref xys 1)
	      (max y-max (aref xys (+ index 1)))))
      ((>= index (length xys))
       (values x-min x-max y-min y-max))))

(defun fit-xy-to-window (xy-vec width height)
  (let ((a (make-array (length xy-vec))))
  (multiple-value-bind (x-min x-max y-min y-max)
		       (bound-xy-vec xy-vec)
    (loop for i from 0 below (length xy-vec) do
	  (setf (aref a i)
		(if (evenp i)
		 (round  (* width (- (aref xy-vec i) x-min))
			 (- x-max x-min))
		 (round (* height (- y-max (aref xy-vec i)))
			(- y-max y-min)))))
    a)))
		
(defun normalised-graph(points width height)
  (single-graph (fit-xy-to-window points width height)
		width
		height))


(defun single-graph (points width height &optional (host ""))
  (let* ((display (xlib:open-display host))
	 (screen (first (xlib:display-roots display)))
	 (black (xlib:screen-black-pixel screen))
	 (white (xlib:screen-white-pixel screen))
	 (root-window (xlib:screen-root screen))
	 (grackon (xlib:create-gcontext
		   :drawable root-window
		   :foreground white
		   :background black))
	 (my-window (xlib:create-window
		     :parent root-window
		     :x 0
		     :y 0
		     :width width
		     :height height
		     :background black
		     :event-mask (xlib:make-event-mask :exposure
						       :button-press))))
    (describe grackon)
    (xlib:map-window my-window)
    (xlib:event-case (display :force-output-p t
			      :discard-p t)
      (:exposure ()
		 (xlib:draw-lines my-window
				  grackon
				  points)
		 nil)
      (:button-press () t))
    (xlib:destroy-window my-window)
    (xlib:close-display display)))


