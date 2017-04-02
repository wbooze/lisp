(defun move-dot(two-value-path-function &optional (width 400)(height 300))
  (let* ((display (xlib:open-display ""))
	 (screen (first (xlib:display-roots display)))
	 (black (xlib:screen-black-pixel screen))
	 (white (xlib:screen-white-pixel screen))
	 (root-window (xlib:screen-root screen))
	 (grackon (xlib:create-gcontext
		   :drawable root-window
		   :foreground white
		   :background black))
	 (window
	  (xlib:create-window
	   :parent (xlib:screen-root screen)
	   :class :input-output
	   :x 0
	   :y 0
	   :width width
	   :height height
	   :background black
	   :event-mask (xlib:make-event-mask
			:exposure
			:button-press
			:key-press))))
    (xlib:map-window window)
    (do ((time 0 (+ time 0.1))
	 (status 'pause (xlib:event-case
		       (display :force-output-p t
				:discard-p t
				:timeout 0.1)
		       (:exposure()
				 'run)
		       (:button-press()
				     'end)
		       (:key-press()
				  'end))))
	((eq status 'end)
	 (xlib:unmap-window window)
	 (xlib:destroy-window window)
	 (xlib:close-display display))
	(when (member status '(run nil))
	  (print time)
	  (multiple-value-bind (x y)
			       (funcall two-value-path-function
					time)
	   (xlib:draw-arc window grackon
			  (round (+ x (/ width 2)))
			  (round (+ y (/ height 2)))
			  10 10
			  0 (* 2 pi)
			  'fill))))))
		       