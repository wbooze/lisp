(define-application-frame stocks-window ()
  ()
  (:geometry :width 640 :height 480 :left 64 :top 32)
  (:panes
    (index-list (make-pane ':list-pane
		  :id 'index-list
		  :value '("A" "B")
		  :items '("A" "B" "C" "D")
		  :mode ':nonexclusive
		  :prefer-single-selection t
		  :test #'string=)))
  (:layouts
    (default
      (with-tab-layout ('tab-page :name 'layout :height 480)
	("A"
	  (spacing
	    (:thickness 8)
	    (horizontally
	      (:x-spacing 4 :y-spacing 4)
	      (labelling (:label "List" :height 128 :width 192)
		(scrolling (:scroll-bar t)
		  (spacing (:thickness 0)
		    index-list)))))))))
  (:menu-bar t))

(define-window-command (com-quit :menu t) ()
  (frame-exit *application-frame*)
  nil)

(defvar *window-frame* nil)

(defun window ()
  (flet ((run ()
	   (let ((frame (make-application-frame 'stocks-window)))
	     (setf *window-frame* frame)
	     (run-frame-top-level frame))))
    (make-thread #'run :name "Window")))
(window)