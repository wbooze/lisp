(define-application-frame draw-tri () ()
                          (:pane (make-pane 'draw-tri-pane)))

(defclass draw-tri-pane (clim-stream-pane)
  ())

(defmethod paint-tri ((pane draw-tri-pane) region)
  (let ((w (bounding-rectangle-width pane))
        (h (bounding-rectangle-height pane)))

    ;; Draw greeting in center of pane
    (draw-line pane (make-point 10 w) (make-point h 60)
               :ink +black+)))

(defun run-draw-tri ()
  (run-frame-top-level
   (make-application-frame 'draw-tri :height 300 :width 300)))
