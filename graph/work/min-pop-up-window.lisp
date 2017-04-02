(defun pop-up-window (life-time &optional (host ""))
  (let* ((display (xlib:open-display host))
         (screen (first (xlib:display-roots display)))
         (root-window (xlib:screen-root screen))
         (my-window (xlib:create-window
                     :parent root-window
                     :x 0
                     :y 0
                     :width 200
                     :height 100)))
    (xlib:map-window my-window)
    (xlib:display-finish-output display)
    (format t "should appear now~%")
    (sleep life-time)
    (xlib:destroy-window my-window)
    (xlib:close-display display)))