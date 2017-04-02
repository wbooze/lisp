;;;; Antialiased lines for McCLIM.  Crude proof of concept
;;;; implementation. Doesn't handle rounded edges or clipping against
;;;; X11 coordinate limit.

(in-package :xlib)

;;; Why -1? Was the intent to provide a nicer interface to this (the
;;; XTrapezoid structure is admittedly stupid, so sounds plausible).
(defun render-trapezoids-1 (picture op source src-x src-y mask-format coord-sequence)
  ;; coord-sequence is  top bottom
  ;;                    line-1-x1 line-1-y1 line-1-x2 line-1-y2
  ;;                    line-2-x1 line-2-y1 line-2-x2 line-2-y2 ...
  ;;
  (let ((display (picture-display picture)))
    (synchronise-picture-state picture)
    (synchronise-picture-state source)
    (with-buffer-request (display (extension-opcode display "RENDER"))
      (data +X-RenderTrapezoids+)
      (render-op op)                    ;op
      (card8 0)                         ;pad
      (card16 0)                        ;pad
      (resource-id (picture-id source))
      (resource-id (picture-id picture))
      ((or (member :none) picture-format) mask-format) ; <------
      (int16 src-x)
      (int16 src-y)
      ((sequence :format int32) coord-sequence) )))

(in-package :clim-clx)

;;; Need to patch this before opening the CLIM window (or at least
;;; before drawing any text on it), because the default picture
;;; settings don't appear to enable smoothing.
(defun mcclim-truetype::drawable-picture (drawable)
  (or (getf (xlib:drawable-plist drawable) 'picture)
      (setf (getf (xlib:drawable-plist drawable) 'picture)
            (xlib::render-create-picture 
             drawable
             :poly-edge :smooth :poly-mode :imprecise
             :format (xlib::find-window-picture-format drawable)))))

(defun fix-round-coordinate (x)
  (floor (* #x10000 x)))

;;; Fall back to xlib:draw-line. Currently needed only for rounded edges.
(defun draw-jaggy-line (medium x1 y1 x2 y2)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-transformed-position (tr x1 y1)
      (with-transformed-position (tr x2 y2)
        (with-clx-graphics (medium)
          (let ((x1 (round-coordinate x1))
                (y1 (round-coordinate y1))
                (x2 (round-coordinate x2))
                (y2 (round-coordinate y2)))
            (cond ((and (<= #x-8000 x1 #x7FFF) (<= #x-8000 y1 #x7FFF)
                        (<= #x-8000 x2 #x7FFF) (<= #x-8000 y2 #x7FFF))
                   (xlib:draw-line mirror gc x1 y1 x2 y2))
                  (t
                   (let ((line (region-intersection (make-rectangle* #x-8000 #x-8000 #x7FFF #x7FFF)
                                                    (make-line* x1 y1 x2 y2))))
                     (when (linep line)
                       (multiple-value-bind (x1 y1) (line-start-point* line)
                         (multiple-value-bind (x2 y2) (line-end-point* line)
                           (xlib:draw-line mirror gc
                                           (min #x7FFF (max #x-8000 (round-coordinate x1)))
                                           (min #x7FFF (max #x-8000 (round-coordinate y1)))
                                           (min #x7FFF (max #x-8000 (round-coordinate x2)))
                                           (min #x7FFF (max #x-8000 (round-coordinate y2))))))))))))))))

(defun %render-thick-line (picture op source x1 y1 x2 y2 dx dy tx ty)
  (assert (<= y1 y2))
  (assert (>= ty 0))
  ;;; FIXME: clipping (yuck!)
  (let ((nx (- ty))
        (ny tx)
        (coords (make-array 36 :initial-element 0 
                               :element-type '(signed-byte 32)
                               :adjustable nil
                               :fill-pointer 0)))
    (declare (dynamic-extent coords))
    (labels 
        ((rect (x1 y1 x2 y2)
           (let* ((x1 (fix-round-coordinate x1))
                  (y1 (fix-round-coordinate y1))
                  (x2 (fix-round-coordinate x2))
                  (y2 (fix-round-coordinate y2))
                  (coords (vector y1 y2 x1 y1 x1 y2 x2 y1 x2 y2))) ; signed-byte 32 ..
             (declare (dynamic-extent coords))
             (xlib::render-trapezoids-1 picture op source 0 0 :none coords)))
         (trap (top-x0 top-x1 top-y bottom-x0 bottom-x1 bottom-y)
           (let ((top-x0 (fix-round-coordinate top-x0))
                 (top-x1 (fix-round-coordinate top-x1))
                 (top-y (fix-round-coordinate top-y))
                 (bottom-x0 (fix-round-coordinate bottom-x0))
                 (bottom-x1 (fix-round-coordinate bottom-x1))
                 (bottom-y (fix-round-coordinate bottom-y)))
             (vector-push top-y coords)
             (vector-push bottom-y coords)
             (vector-push top-x0 coords)
             (vector-push top-y coords)
             (vector-push bottom-x0 coords)
             (vector-push bottom-y coords)
             (vector-push top-x1 coords)
             (vector-push top-y coords)
             (vector-push bottom-x1 coords)
             (vector-push bottom-y coords)))
         (render () (xlib::render-trapezoids-1 picture op source 0 0 :none coords)))
      (when (> ny 0)
        (setf (values nx ny) (values (- nx) (- ny))))
    (cond
      ((zerop dx)
       (rect (- (min x1 x2) (abs nx)) y1
             (+ (max x1 x2) (abs nx)) y2))
      ((zerop dy)
       (rect (min x1 x2) (- y1 (abs ny))
             (max x1 x2) (+ y2 (abs ny))))
      ((> (+ y1 (abs ny)) (- y2 (abs ny)))
       (multiple-value-bind (tip-x tip-y base-x base-y bottom-x bottom-y)
           (values (+ x1 nx) (+ y1 ny) (+ x2 nx) (+ y2 ny) (- x2 nx) (- y2 ny))
         (assert (> base-y tip-y))
         (let* ((clip-dx (* (/ nx ny) (- base-y tip-y)))
                (clip-x  (+ tip-x clip-dx)))
           (trap tip-x tip-x tip-y 
                 (min clip-x base-x) (max clip-x base-x) base-y)
           (let ((sbx1 (- x1 nx))
                 (sbx2 (- bottom-x clip-dx)))
             ;; I get a subpixel gap between these three in my test. Odd.
             (trap (min clip-x base-x) (max clip-x base-x) base-y
                   (min sbx1 sbx2) (max sbx1 sbx2) (- y1 ny))
             (trap (min sbx1 sbx2) (max sbx1 sbx2) (- y1 ny)
                   bottom-x bottom-x bottom-y)
             (render)))))
      (t
       (let* ((ax1 (- x1 nx))
              (clip-dx (* (/ dx dy) -2 ny))
              (ax2 (+ x1 nx clip-dx)))
         (when (> ax1 ax2) (rotatef ax1 ax2))
         (trap (+ x1 nx) (+ x1 nx) (+ y1 ny) ax1 ax2 (- y1 ny))
         (trap ax1 ax2 (- y1 ny) 
               (+ ax1 (- x2 x1 clip-dx)) (+ ax2 (- x2 x1 clip-dx)) (+ y2 ny))
         (trap (+ ax1 (- x2 x1 clip-dx)) (+ ax2 (- x2 x1 clip-dx)) (+ y2 ny)
               (- x2 nx) (- x2 nx) (- y2 ny))
         (render)))))))
       
(defmethod medium-draw-line* ((medium clx-medium) x1 y1 x2 y2)
  (let ((tr (sheet-native-transformation (medium-sheet medium))))
    (with-transformed-position (tr x1 y1)
      (with-transformed-position (tr x2 y2)
        (with-clx-graphics (medium)
          ;; Bail out to regular X11 lines under various
          ;; circumstances.  Ignore the rounded cap on really thin
          ;; lines, since you won't be able to see it anyway.
          (when (or (not (eql (xlib:gcontext-line-style gc) :solid))
                    (and (> (xlib:gcontext-line-width gc) 2)
                         (eql (xlib:gcontext-cap-style gc) :round)))
            (draw-jaggy-line medium x1 y1 x2 y2)
            (return-from medium-draw-line*))

          (cond 
            ;; There was a special case here to draw thin lines faster
            ;; by omitting the caps, but it didn't work right.

            ;; Draw thick line with squared cap:
            (t (multiple-value-bind (x1 y1 x2 y2)
                   (if (< y1 y2)
                       (values x1 y1 x2 y2)
                       (values x2 y2 x1 y1))
                 (let* ((width  (- x2 x1))
                        (height (- y2 y1))
                        (len (sqrt (+ (* width width) (* height height)))))
                   (unless (zerop len)
                     (let* ((dx (/ width len))
                            (dy (/ height len))
                            (thickness/2 (* 0.5 (max 1 (xlib:gcontext-line-width gc))))
                            (tx (* thickness/2 dx))
                            (ty (* thickness/2 dy)))
                       (when (eql (xlib:gcontext-cap-style gc) :projecting)
                         (setf x1 (- x1 tx)
                               y1 (- y1 ty)
                               x2 (+ x2 tx)
                               y2 (+ y2 ty)))
                       (%render-thick-line (mcclim-truetype::drawable-picture mirror)
                                           :over
                                           (first (mcclim-truetype::gcontext-picture mirror gc)) 
                                           x1 y1 x2 y2 dx dy tx ty))))))))))))
