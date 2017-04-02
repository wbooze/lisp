(defun show-exposure-events (width height &optional (host ""))
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
      (:exposure (count x y width height)
		 (format t "~A~%" count)
		 (xlib:draw-line my-window
			    grackon
			    x y
			    width height
			    t)
		 (xlib:draw-line my-window
			    grackon
			    x (+ y height)
			    (+ x width) y))
      (:button-press () t))
    (xlib:destroy-window my-window)
    (xlib:close-display display)))




