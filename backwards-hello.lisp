(define-application-frame hello-world ()
                          ((greeting :initform "Hello World" :accessor greeting))
                          (:pane (make-pane 'hello-world-pane)))

(defclass hello-world-pane (clim-stream-pane)
  ())

(defmethod handle-repaint ((pane hello-world-pane) region)
  (let ((w (bounding-rectangle-width pane))
        (h (bounding-rectangle-height pane)))
    ;; Blank the pane out
    (draw-rectangle* pane 0 0 w h
                     :filled t
                     :ink (pane-background pane))
    ;; Draw greeting in center of pane
    (draw-text* pane
                (greeting *application-frame*)
                (floor w 2) (floor h 2)
                :toward-x 0 :toward-y 0)))       ; I expect to get "dlroW olleH"

(defun make-and-run-hello-world ()
  (run-frame-top-level
   (make-application-frame 'hello-world :height 300 :width 300)))
