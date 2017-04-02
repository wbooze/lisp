(defun constituent(c)
  (and (graphic-char-p c)
       (not (char= c #\space))))

(defun get-list-of-paragraphs(filename)
  (with-open-file
   (stream filename
	   :direction :input
	   :if-does-not-exist nil)
   (if stream (read stream)
     (list (concatenate 'string "Unable to open " filename)))))

(defun white-space-split (string)
  (when (plusp (length string))
    (let ((cut-point (position-if
		      (complement #'constituent)
		      string)))
      (if cut-point
	  (if (zerop cut-point)
	      (white-space-split
	       (subseq string 1))
	    (cons (subseq string 0 cut-point)
		  (white-space-split
		   (subseq string (1+ cut-point)))))
	(list string)))))

(defun render (list-of-paragraphs
	       window
	       window-width
	       window-height
	       font
	       grackon)
  (let ((right-margin 5)
	(left-margin 10)
	(line-spacing (+ 3 (xlib:font-ascent font)))
	(inter-word-space (xlib:text-width font " ")))
    (let ((line 1)
	  (x left-margin))
      (dolist (paragraph list-of-paragraphs)
	      (dolist (word (white-space-split paragraph))
		      (let ((width (xlib:text-width font word)))
			(when (> (+ x width right-margin) window-width)
			  (incf line)
			  (setf x left-margin))
			(xlib:draw-glyphs
			 window
			 grackon
			 x
			 (* line line-spacing)
			 word)
			(incf x (+ width inter-word-space))))
	      (incf line (if (= x left-margin) 1 2))
	      (setf x left-margin)))))

(defun paragraphs (filename &optional (requested-width 400)
			   (requested-height 300)(host ""))
  (let* ((display (xlib:open-display host))
	 (font (xlib:open-font
		display
		"-*-lucida-medium-r-*-*-12-*-*-*-*-*-*"))
	 (screen (first (xlib:display-roots display)))
	 (black (xlib:screen-black-pixel screen))
	 (white (xlib:screen-white-pixel screen))
	 (root-window (xlib:screen-root screen))
	 (grackon (xlib:create-gcontext
		   :drawable root-window
		   :font font
		   :foreground white
		   :background black))
	 (my-window (xlib:create-window
		     :parent root-window
		     :x 0
		     :y 0
		     :width requested-width
		     :height requested-height
		     :background black
		     :event-mask (xlib:make-event-mask :exposure
						       :button-press
						       :structure-notify)))
	 (actual-height 0)
	 (actual-width 0)
	 (list-of-paragraphs (get-list-of-paragraphs filename)))
    (describe (xlib:gcontext-font grackon))
    (xlib:map-window my-window)
    (xlib:event-case (display :force-output-p t
			      :discard-p t)
      (:configure-notify (width height)
	 (setf actual-width width actual-height height)
	 nil)
      (:exposure (count)
	 (when (zerop count)
	   (render list-of-paragraphs
		   my-window
		   actual-width
		   actual-height
		   font
		   grackon))
	 nil)
      (:button-press () t))
    (xlib:destroy-window my-window)
    (xlib:close-display display)))








