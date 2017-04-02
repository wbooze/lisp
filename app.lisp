(in-package :common-lisp-user)

(defpackage :app
  (:use :clim :clim-lisp)
  (:export :app-main))

(in-package :app)

(define-application-frame superapp ()
  (application-pane)
  (:pane (with-slots (application-pane) *application-frame*
		     (vertically ()
				 (setf application-pane 
				       (make-pane 'application-pane :width 600 :height 400 
                          :background (climi::xpm-find-named-color "white")
                          :foreground (climi::xpm-find-named-color "black")))))))

(defclass superapp-pane 
  (standard-extended-input-stream basic-pane permanent-medium-sheet-output-mixin) ())

(defmethod draw-my-line ((pane superapp-pane) region)
  (with-application-frame (*application-frame*)
    (call-next-method)
    (with-drawing-options (pane frame) :ink (climi::xpm-find-named-color "wheat4") 
                          (draw-line* pane (make-point 10 20) (make-point 100 300)))))

(defun app-main ()
  (progn (run-frame-top-level (make-application-frame 'superapp)))) 
;;		 (draw-my-line (application-pane *application-frame*) application-pane)))
