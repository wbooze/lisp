(defun pick2numbers(x-range y-range)
  (let* ((display (xlib:open-display ""))
	 (screen (first (xlib:display-roots display)))
	 (black (xlib:screen-black-pixel screen))
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
			  :button-press))))
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
	   (:button-press
	    (x y)
	    (xlib:unmap-window window)
	    (xlib:destroy-window window)
	    (xlib:close-display display)
	    (cons x (- y-range (+ y 1)))))))





















