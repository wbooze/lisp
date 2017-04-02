(defun pick2numbers(x-range y-range)
  (let* ((display (xlib:open-display ""))
	 (screen (first (xlib:display-roots display)))
	 (black (xlib:screen-black-pixel screen))
	 (white (xlib:screen-white-pixel screen))
	 (root-window (xlib:screen-root screen))
	 (grackon (xlib:create-gcontext
		   :drawable root-window
		   :font "-*-lucida-medium-r-*-*-12-*-*-*-*-*-*"
		   :foreground white
		   :background black))
	 pointer-x
	 pointer-y
	 (window
	  (xlib:create-window
	   :parent (xlib:screen-root screen)
	   :class :input-output
	   :x 0
	   :y 0
	   :width x-range
	   :height y-range
	   :background black
	   :event-mask (xlib:make-event-mask
			:exposure
			:button-press
			:pointer-motion
			:enter-window
			:leave-window))))
    (xlib:change-property window
			  :wm_name
			  "Pick two numbers"
			  :string
			  8
			  :transform #'char-code)
    (xlib:map-window window)
    (xlib:event-case
     (display :force-output-p t
	      :discard-p t)
     (:button-press (x y)
		    (setf pointer-x x
			  pointer-y (- y-range (+ y 1)))
		    t)
     (:exposure (count)
		(when (and pointer-x (zerop count))
		  (xlib:draw-glyphs
		   window
		   grackon
		   (round x-range 3)
		   (round y-range 3)
		   (format nil "~d, ~d"
			   pointer-x
			   pointer-y)))
		nil)
     (:motion-notify(x y)
		    (setf pointer-x x
			  pointer-y (- y-range (+ y 1)))
		    (xlib:clear-area window)
		    (xlib:draw-glyphs
		     window
		     grackon
		     (round x-range 3)
		     (round y-range 3)
		     (format nil "~d, ~d"
			     pointer-x
			     pointer-y))
		    nil)
     (:enter-notify(x y)
		   (setf pointer-x x
			 pointer-y (- y-range (+ y 1)))
		   nil)
     (:leave-notify()
		   (setf pointer-x nil
			 pointer-y nil)
		   (xlib:clear-area window)
		   nil))
	   
    (xlib:unmap-window window)
    (xlib:destroy-window window)
    (xlib:close-display display)
    (cons pointer-x pointer-y)))





















