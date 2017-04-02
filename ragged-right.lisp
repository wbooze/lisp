(defun constituent (c) 
(and (graphic-char-p c) (not (char= c #\ ))))

(defun white-space-split (string)
  (when (plusp (length string))
    (let ((cut-point (position-if
		      (complement #'constituent)
		      string)))
      (if cut-point
	  (cons (subseq string 0 cut-point)
		(white-space-split
		 (subseq string (1+ cut-point))))
	(list string)))))


(defun ragged-right
       (words &optional (requested-width 400) (requested-height 300) (host ""))
  (let* ((display (xlib:open-display host))
         (screen (first (xlib:display-roots display)))
         (black (xlib:screen-black-pixel screen))
         (white (xlib:screen-white-pixel screen))
         (root-window (xlib:screen-root screen))
         (grackon
          (xlib:create-gcontext :drawable root-window :font
                                "-*-terminus-bold-r-*-*-20-*-*-*-*-*-*"
                                :foreground white :background black))
         (my-window
          (xlib:create-window :parent root-window :x 0 :y 0 :width
                              requested-width :height requested-height
                              :background black :event-mask
                              (xlib:make-event-mask :exposure :button-press
                                                    :structure-notify)))
         (actual-height requested-height)
         (actual-width requested-width))
    (describe (xlib:gcontext-font grackon))
    (xlib:map-window my-window)
    (xlib:event-case (display :force-output-p t :discard-p t)
      (:configure-notify (width height)
       (setf actual-width width
             actual-height height)
       nil)
      (:exposure (count)
       (when (zerop count)
         (let* ((right-margin 5)
                (left-margin 10)
                (line-spacing
                 (+ 3 (xlib:font-ascent (xlib:gcontext-font grackon))))
                (inter-word-space (xlib:text-width grackon " "))
                (line 1)
                (x left-margin))
           (dolist (word words)
             (let ((width (xlib:text-width grackon word)))
               (when (> (+ x width right-margin) actual-width)
                 (incf line)
                 (setf x left-margin))
               (xlib:draw-glyphs my-window grackon x (* line line-spacing)
                                 word)
               (incf x (+ width inter-word-space))))))
       nil)
      (:button-press nil t))
    (xlib:destroy-window my-window)
    (xlib:close-display display)))
