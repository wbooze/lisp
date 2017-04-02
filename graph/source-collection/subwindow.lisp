(defun graphic-x (width height across down &optional (host ""))
  (let* ((display (xlib:open-display host))
	 (screen (first (xlib:display-roots display)))
	 (black (xlib:screen-black-pixel screen))
	 (white (xlib:screen-white-pixel screen))
	 (root-window (xlib:screen-root screen))
	 (green (xlib:alloc-color
		 (xlib:window-colormap root-window)
		 'green))
	 (blue (xlib:alloc-color
		 (xlib:window-colormap root-window)
		 'blue))
	 (red (xlib:alloc-color
		 (xlib:window-colormap root-window)
		 'red))
	 (top-window (xlib:create-window
		     :parent root-window
		     :x 0
		     :y 0
		     :width width
		     :height height
		     :background black
		     :event-mask (xlib:make-event-mask :key-press
						       :button-press)))
	 (red-window (xlib:create-window
		     :parent top-window
		     :x across
		     :y 0
		     :width (truncate width 4)
		     :height (truncate height 4)
		     :background red
		     :event-mask (xlib:make-event-mask :button-press)))
	 (green-window (xlib:create-window
		     :parent top-window
		     :x 0
		     :y down
		     :width (truncate width 4)
		     :height (truncate height 4)
		     :background green
		     :event-mask (xlib:make-event-mask :button-press)))
	 (blue-window (xlib:create-window
		     :parent top-window
		     :x across
		     :y down
		     :width (truncate width 4)
		     :height (truncate height 4)
		     :background blue
		     :border-width 5
		     :border white
		     :event-mask (xlib:make-event-mask :button-press))))
    (xlib:map-window top-window)
    (xlib:map-window red-window)
    (xlib:map-window green-window)
    (xlib:map-window blue-window)
    (xlib:event-case (display :force-output-p t
			      :discard-p t)
      (:button-press (window)
		     (cond ((eq window red-window)
			    (xlib:destroy-window red-window)
			    nil)
			   ((eq window green-window)
			    (xlib:destroy-window blue-window)
			    nil)
			   ((eq window blue-window)
			    (xlib:destroy-window green-window)
			    nil)
			   (t t)))
      (:key-press ()
		  (xlib:circulate-window-down top-window)
		  nil))
    (xlib:destroy-window top-window)
    (xlib:close-display display)))




