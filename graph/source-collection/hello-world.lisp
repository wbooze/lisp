(defun hello-world (width height &optional (host ""))
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
    (describe (xlib:gcontext-font grackon))
    (xlib:map-window my-window)
    (xlib:event-case (display :force-output-p t
			      :discard-p t)
      (:exposure (count)
	 (when (zerop count)
	   (xlib:draw-glyphs
	     my-window
	     grackon
	     20 50
	     "Hello World!"))
	 nil)
      (:button-press () t))
    (xlib:destroy-window my-window)
    (xlib:close-display display)))




