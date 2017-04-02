(define-application-frame draw-tri () ()
                          (:panes
                           (draw-tri-pane (make-pane 'draw-tri-pane :height 400 :width 600 :incremental-redisplay t))
                           (int :interactor :height 20 :width 600 :scroll-bars nil))
                          (:layouts 
                           (default (vertically () draw-tri-pane int))))


(defclass draw-tri-pane (clim-stream-pane)
  ())

;; drawing triangles, constructors
(defun make-tri (pane &optional (stream *query-io*))
  (loop
   (fresh-line stream)
  (clim:accepting-values (stream :resynchronize-every-pass t :align-prompts t)
    (fresh-line stream)
  ;; c1, c2, c3 are the corner points of the triangle in sheet coordinates (or rather user coord.)
    (setq x1 (clim:accept 'integer :default 10 :prompt "x1" :stream stream)
          y1 (clim:accept 'integer :default 10 :prompt "y1" :stream stream)
          x2 (clim:accept 'integer :default 100 :prompt "x2" :stream stream)
          y2 (clim:accept 'integer :default 100 :prompt "y2" :stream stream)
          x3 (clim:accept 'integer :default 10 :prompt "x3" :stream stream)
          y3 (clim:accept 'integer :default 100 :prompt "y3" :stream stream))
    (let
        ((c1 (make-point x1 y1))
         (c2 (make-point x2 y2))
         (c3 (make-point x3 y3)))
      (draw-line pane c1 c2)
      (draw-line pane c2 c3)
      (draw-line pane c3 c1)))))

(defmethod handle-repaint ((pane draw-tri-pane) region)
  (let ((w (bounding-rectangle-width pane))
        (h (bounding-rectangle-height pane)))
    ;; Blank the pane out
    (draw-rectangle* pane 0 0 w h
                     :filled t
                     :ink (pane-background pane))
    ;; Draw greeting in center of pane
    (with-drawing-options (pane :ink +red+)
                          (make-tri pane))))

(defun run-draw-tri ()
  (run-frame-top-level
   (make-application-frame 'draw-tri :height 600 :width 600)))
