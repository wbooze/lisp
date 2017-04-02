(in-package :clim-user)

;;;; 3D Logic flash game (http://www.newgrounds.com/portal/view/315702),
;;;; translated into CL/McCLIM.
;;;;   Author: Andy Hefner <ahefner@gmail.com>
;;;;     Date: June 29, 2006

(defclass logic-cube-pane (basic-gadget)
  ((background :initform (make-rgb-color 0.65 0.65 0.86) :reader background-color)
   (density :initform 5 :accessor density)
   (playfield :reader playfield)
   (drag-color :initform nil :accessor drag-color)
   (dragging :initform nil :accessor dragging)
   (pitch :initform 0.0 :accessor pitch)
   (yaw   :initform 0.0 :accessor yaw)))

(defun reset-logic-cube (cube new-density)
  (with-slots (density playfield) cube
    (setf density new-density
          playfield (make-array (list 3 density density) :initial-element (list nil nil)))
    ;; stupid hack
    (setf (aref playfield 0 0 0) (list +red+ 'terminal)
          (aref playfield 2 2 3) (list +red+ 'terminal)
          (aref playfield 1 1 2) (list +green+ 'terminal)
          (aref playfield 2 3 2) (list +green+ 'terminal)
          (aref playfield 1 2 3) (list +blue+ 'terminal)
          (aref playfield 0 4 3) (list +blue+ 'terminal)
          (aref playfield 1 2 1) (list +orange+ 'terminal)
          (aref playfield 0 2 0) (list +orange+ 'terminal)
          (aref playfield 0 1 4) (list +yellow+ 'terminal)
          (aref playfield 1 4 2) (list +yellow+ 'terminal)
          (aref playfield 2 2 2) (list nil t))))

;; Playfield squares are a pair of color and {nil, t, terminal}

(defmethod initialize-instance :after ((pane logic-cube-pane) &rest args)
  (declare (ignore args))
  (reset-logic-cube pane (density pane)))

(defmethod compose-space ((pane logic-cube-pane) &key width height)
  (declare (ignore width height))
  ;; TODO: How in CLIM does one constrain the aspect ratio of a pane?
  (make-space-requirement :min-width 200
                          :min-height 200
                          :width 550
                          :height 550))

(defun lc-m3xv3 (a b)                   ; multiply 3x3 matrix by vector
  (flet ((f (i) (loop for j from 0 below 3 sum (* (aref a i j) (elt b j)))))
    (vector (f 0) (f 1) (f 2))))

(defun lc-m3xm3 (a b)                   ; multiply two 3x3 matrices
  (let ((matrix (make-array '(3 3) :initial-element 0.0)))
    (dotimes (row 3)
      (dotimes (col 3)
        (dotimes (i 3)
          (incf (aref matrix row col) (* (aref a row i) (aref b i col))))))
    matrix))

(defun lc-rotation-matrix (theta axis-a axis-b)
  (let ((matrix (make-array '(3 3) :initial-element 0.0)))
    (dotimes (i 3) (setf (aref matrix i i) 1.0))
    (setf (aref matrix axis-a axis-a) (cos theta)
          (aref matrix axis-a axis-b) (sin theta)
          (aref matrix axis-b axis-a) (- (sin theta))
          (aref matrix axis-b axis-b) (cos theta))
    matrix))

(defun lc-v+ (a b) (map 'vector #'+ a b)) ; 3-vector addition a+b
(defun lc-v- (a b) (map 'vector #'- a b)) ; 3-vector subtract a-b
(defun lc-scale (a s) (map 'vector (lambda (x) (* x s)) a)) ; 3-vector multiply by scalar

(defun lc-cross (a b)                   ; 3-vector cross product
  (macrolet ((woo (p q)
              `(- (* (elt a ,p) (elt b ,q ))
                  (* (elt a ,q) (elt b ,p)))))
    (vector (woo 1 2)
            (woo 2 0)
            (woo 0 1))))

;; Corner of hemicube is at origin.
;; Sides: 0=XY 1=XZ 2=YZ
(defun apply-to-hemicube-faces (n fn)
  (let ((size (/ n)))
    (dotimes (d 3)
      (flet ((permute (x y)
               ; SBCL warns (erroneously?) below, but the code works.
               (flet ((f (i) (elt (vector x y 0) (mod (+ d i) 3)))) 
                 (vector (f 0) (f 1) (f 2)))))
        (dotimes (i n)
          (dotimes (j n)
            (let ((base-x (* i size))
                  (base-y (* j size)))
              (funcall fn d i j
                       (permute base-x base-y)
                       (permute (+ base-x size) base-y)
                       (permute (+ base-x size) (+ base-y size))
                       (permute base-x (+ base-y size))))))))))

(defun lc-point-transformer (view-matrix)
  (lambda (point)
    (setf point (map 'vector (lambda (x) (- x 0.5)) point))
    (setf point (lc-m3xv3 view-matrix point))
    (let ((z (+ 2.0 (elt point 2)))
          (zoom 2.0))
      (vector (* zoom (/ (elt point 0) z))
              (* zoom (/ (elt point 1) z))
              z))))

(defun lc-scale-polygon (polygon amount)
  (let ((center  (reduce (lambda (a b) (lc-v+ a (lc-scale b (/ (length polygon))))) polygon
                        :initial-value #(0.0 0.0))))
    (mapcar (lambda (v) (lc-v+ center (lc-scale (lc-v- v center) amount))) polygon)))

(defun draw-polygon-3d (pane points &rest polygon-args)
  (apply #'draw-polygon pane
         (mapcar (lambda (p) (make-point (elt p 0) (elt p 1))) points)
         polygon-args))

(defun apply-to-logic-cube-faces (pane continuation)
  (let ((transformer (lc-point-transformer
                       (lc-m3xm3 (lc-rotation-matrix (pitch pane) 1 2)
                                 (lc-rotation-matrix (yaw pane)   0 2)))))
    (apply-to-hemicube-faces (density pane)
      (lambda (side i j &rest points)
        (apply continuation side i j (mapcar transformer points))))))

(defun lc-face-normal (points)
  (lc-cross (lc-v- (elt points 2)
                   (elt points 1))
            (lc-v- (elt points 0)
                   (elt points 1))))

(defun backface-p (points)
  (<= (elt (lc-face-normal points) 2) 0))

(defun face-light (color side)
  (compose-over (compose-in color (make-opacity 0.75))
                (elt (vector +gray40+ +white+ color) side)))

(defun polygon-edges (points)
  (maplist (lambda (list)
             (lc-v- (or (second list) (first points)) (first list)))
           points))

(defun draw-polygon-outline-3d (pane a b &rest polygon-args)
  (maplist (lambda (a* b*)
             (apply #'draw-polygon-3d pane
                    (list (first a*)
                          (first b*)
                          (or (second b*) (first b))
                          (or (second a*) (first a)))
                    polygon-args))
           a b))
  
(defun draw-logic-cube (pane)
  (apply-to-logic-cube-faces pane
    (lambda (side i j &rest camera-points)
      (unless (backface-p camera-points)
        (destructuring-bind (color type) (aref (playfield pane) side i j)
          (cond
            ((null type)
             (draw-polygon-3d pane (lc-scale-polygon camera-points 0.8)
                              :filled t :ink (face-light (or color +gray80+) side)))
            ((eql type 'terminal)
             (when (eql color (drag-color pane))
               (draw-polygon-3d pane camera-points
                                :filled t :ink color))
             (draw-polygon-outline-3d pane camera-points (lc-scale-polygon camera-points 0.7)
                                      :filled t :ink (face-light (or color +gray80+) side)))))))))

(defun invoke-in-lc-space (pane continuation) ; "logic-cube space" =p
  (let* ((width  (bounding-rectangle-width pane))
         (height (bounding-rectangle-height pane))
         (radius (/ (min width height) 2)))
    (with-translation (pane (/ width 2) (/ height 2))
      (with-scaling (pane radius)
        (funcall continuation pane)))))

(defmethod handle-repaint ((pane logic-cube-pane) region)
  (with-bounding-rectangle* (x0 y0 x1 y1) region
    (climi::with-double-buffering ((pane x0 y0 x1 y1) (wtf-wtf-wtf))
      (declare (ignore wtf-wtf-wtf))
      (draw-rectangle* pane x0 y0 x1 y1 :filled t :ink (background-color pane))
      (invoke-in-lc-space pane #'draw-logic-cube))))

(defun square (x) (* x x))

(defun point-in-poly-p (x y points)
  (every (lambda (point edge)
           (let* ((edge-length (sqrt (+ (square (elt edge 0)) (square (elt edge 1)))))
                  (nx (/ (- (elt edge 1)) edge-length))
                  (ny (/ (elt edge 0) edge-length))
                  (c (+ (* nx (elt point 0))
                        (* ny (elt point 1)))))
             (< c (+ (* nx x) (* ny y)))))
         points
         (polygon-edges points)))

(defun drag-on-square (pane cell)
  (if (and (dragging pane) (null (second cell)) (drag-color pane))
      (list (drag-color pane) nil)
      cell))

(defun xy-to-viewport-coordinates (pane x y)
  (let* ((width  (bounding-rectangle-width pane)) ; ..
         (height (bounding-rectangle-height pane))
         (radius (/ (min width height) 2)))
    (values (/ (- x (/ width 2)) radius)
            (/ (- y (/ height 2)) radius))))

(defun find-poly-under-point (pane x y)
  (apply-to-logic-cube-faces pane
    (lambda (side i j &rest points)
      (unless (backface-p points)
        (when (point-in-poly-p x y points)
          (return-from find-poly-under-point (values side i j))))))
  (values nil nil nil))

(defmethod handle-event ((pane logic-cube-pane) (event pointer-exit-event))
  (setf (dragging pane) nil))

(defmethod handle-event ((pane logic-cube-pane) (event pointer-button-release-event))
  (setf (dragging pane) nil))

(defun square+ (pane side i j di dj)
  (let ((ni (+ i di))
        (nj (+ j dj)))
    (if (or (> 0 ni)
            (> 0 nj)
            (>= ni (density pane))
            (>= nj (density pane)))
        nil
        (list side ni nj))))

(defun adjacent-squares (pane side i j)
  (remove nil                           ; Ouch!
          (list (square+ pane side i j 1 0)
                (square+ pane side i j 0 1)
                (or (square+ pane side i j -1 0)
                    (and (= side 2) (list 1 j 0))
                    (and (= side 0) (list 2 j 0))
                    (and (= side 1) (list 0 j 0)))
                (or (square+ pane side i j 0 -1)
                    (and (= side 2) (list 0 0 i))
                    (and (= side 1) (list 2 0 i))
                    (and (= side 0) (list 1 0 i))))))

(defun check-victory (pane)
  (let ((color-roots (make-hash-table))
        (coverage (make-hash-table :test 'equal)))
    (dotimes (side 3)
      (dotimes (i (density pane))
        (dotimes (j (density pane))
          (destructuring-bind (color type) (aref (playfield pane) side i j)
            (when (eql type 'terminal)
              (setf (gethash color color-roots) (list side i j)))))))
    (maphash
     (lambda (color root-indices)
       (clrhash coverage)
       (labels ((searching (indices)
                  (setf (gethash indices coverage) t)
                  (some (lambda (indices)
                          (destructuring-bind (color-2 type) (apply #'aref (playfield pane) indices)
                            (and (eql color color-2)
                                 (not (gethash indices coverage))
                                 (or (eql type 'terminal)
                                     (searching indices)))))
                        (apply #'adjacent-squares pane indices))))
         (when (searching root-indices) ; Remove the root from the table if we are connection           
           (remhash color color-roots))))
       color-roots)
    (zerop (hash-table-count color-roots)))) ; Successful if no unconnected roots remained

(defmethod handle-event ((pane logic-cube-pane) (event pointer-button-press-event))
  (multiple-value-bind (side i j)
      (multiple-value-call #'find-poly-under-point pane
                           (xy-to-viewport-coordinates pane (pointer-event-x event) (pointer-event-y event)))
    (when side
      (destructuring-bind (color type) (aref (playfield pane) side i j)
        (setf (dragging pane) t
              (drag-color pane) (if (eql type 'terminal) color (drag-color pane))))))
  (repaint-sheet pane (sheet-region pane)))

(defun won-logic-cube (pane)
  (draw-text* pane "You Win!!" 13 13 :text-style (make-text-style :serif :bold :huge) :ink +black+ :align-y :top)
  (draw-text* pane "You Win!!" 10 10 :text-style (make-text-style :serif :bold :huge) :ink +white+ :align-y :top)
  (sleep 3)
  (reset-logic-cube pane 5)
  (repaint-sheet pane (sheet-region pane)))

(defmethod handle-event ((pane logic-cube-pane) (event pointer-motion-event))
  (multiple-value-bind (x y)
      (xy-to-viewport-coordinates pane (pointer-event-x event) (pointer-event-y event))
    (multiple-value-bind (side i j)
        (find-poly-under-point pane x y)
      (when side
        (symbol-macrolet ((cell (aref (playfield pane) side i j)))
          (when (not (equalp cell (setf cell (drag-on-square pane cell)))) ; when we've genuinely change the state..
            (when (check-victory pane)  ; .. check for win
              (repaint-sheet pane (sheet-region pane))
              (won-logic-cube pane))))))
    (setf (yaw pane)   (- (* (/ pi 4) (max -1.0 (min 1.0 x))) (/ pi 4))
          (pitch pane) (min 0.0 (- (* (/ pi 4) (max -1.0 (min 1.0 y))) (/ pi 8))))
    (repaint-sheet pane (sheet-region pane))))
   
(define-application-frame logic-cube () ()
  (:panes (logic-cube (make-pane 'logic-cube-pane)))
  (:layouts (:default logic-cube))
  (:menu-bar nil))
