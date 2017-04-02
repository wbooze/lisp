(defstruct rect x y w h c)

(defun random-choice (item-list)
  (let ((options (length item-list)))
    (elt item-list (random options))))

(defun cons-up-rect-list (n)
  (cons-up n #'(lambda()
		 (make-rect :x (random 300)
			    :y (random 300)
			    :w (+ 20 (random 30))
			    :h (+ 20 (random 30))
			    :c (random-choice *colour-list*)))))

(defun cons-up (count constructor)
  (let (accumulator)
    (dotimes (index count accumulator)
      (push (funcall constructor) accumulator))))

(defvar *colour-list* '(red green blue yellow cyan magenta))

(defparameter *default-rect-list*
  (cons-up-rect-list 20))

(defvar *rect-list* *default-rect-list*)
; The program can be loaded and run, and will display
; some rectangles. If you have already created some 
; rectangles, it will not clobber them.
; In particular you can edit the source, reload the 
; the file, and you still have your own rectangle list.
; At any time, you can get back to the default with
; (setf *rect-list* *default-rect-list*)


(defun crossed-colours (pixel-combining-function width height &optional (host ""))
  (let* ((display (xlib:open-display host))
	 (screen (first (xlib:display-roots display)))
	 (black (xlib:screen-black-pixel screen))
	 (white (xlib:screen-white-pixel screen))
	 (root-window (xlib:screen-root screen))
	 (my-window (xlib:create-window
		     :parent root-window
		     :x 0
		     :y 0
		     :width width
		     :height height
		     :background black
		     :event-mask (xlib:make-event-mask :exposure
						       :button-press))))
    (dolist (colour-symbol *colour-list*)
	 (setf (get colour-symbol 'grackon)
	       (xlib:create-gcontext
		:drawable root-window
		:foreground (xlib:alloc-color
			     (xlib:window-colormap root-window)
			     (symbol-name colour-symbol))
		:background black
		:line-width 30
		:function pixel-combining-function)))
    (xlib:map-window my-window)
    (xlib:event-case (display :force-output-p t
			      :discard-p t)
		     (:exposure (count)
				(when (zerop count)
				  (xlib:draw-line
				   my-window
				   (get 'red 'grackon)
				   50 150 250 150)
				  (xlib:draw-line
				   my-window
				   (get 'green 'grackon)
				   150 50 250 250)
				  (xlib:draw-line
				   my-window
				   (get 'blue 'grackon)
				   150 50 50 250))
				nil)
		     (:button-press () t))
    (xlib:destroy-window my-window)
    (xlib:close-display display)))




