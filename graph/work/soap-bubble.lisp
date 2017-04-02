(defun blow-bubble (&optional (host ""))
  (let* ((display (xlib:open-display host))
	 (screen (first (xlib:display-roots display)))
	 (black (xlib:screen-black-pixel screen))
	 (root-window (xlib:screen-root screen))
	 (my-window (xlib:create-window
		     :parent root-window
		     :x 0
		     :y 0
		     :width 200
		     :height 100
		     :background black
		     :event-mask (xlib:make-event-mask :exposure
						       :enter-window))))
    (xlib:map-window my-window)
    (xlib:event-case (display :force-output-p t
			      :discard-p t)
      (:exposure ()(format t "Exposed~%"))
      (:enter-notify () t))
    (xlib:destroy-window my-window)
    (xlib:close-display display)))
