(let ((points
       (sb-c::flatten-list
        (loop for x from 0 to (* 2 pi) by 0.001
              collect (list x (sin x))))))
  (with-room-for-graphics (t :height 260)
    (fresh-line)
    (with-scaling (t 120)
      (with-translation (t -1 1.3)
        (draw-points* *standard-output* points :ink +green+ :line-thickness 2)
        (medium-draw-lines* *standard-output* (list 0 0 (* 2 pi) 0))
        (medium-draw-lines* *standard-output* (list 0 -1 0 1))
        (medium-draw-lines* *standard-output* (list 0 1 (* 2 pi) 1))
        (medium-draw-lines* *standard-output* (list (* 2 pi) 1 (* 2 pi) 0))
        (medium-draw-lines* *standard-output* (list 0 -1 (* 2 pi) -1))
        (medium-draw-lines* *standard-output* (list (* 2 pi) -1 (* 2 pi) 0))))))

(let ((graph1 (fit-xy-to-window (|x,f(x)| 1000 0 (* 2 pi) #'sin) 1000 500))
      (graph2 (fit-xy-to-window (|x,f(x)| 1000 0 (* 2 pi) #'cos) 1000 500))
      (graph3 (fit-xy-to-window (|x,f(x)| 1000 0 (* 2 pi) #'tan) 1000 500)))
  (funcall
   (lambda ()
     (with-room-for-graphics (t :height 250)
       (with-translation (t 0 60)
         (draw-points* *standard-output* graph1 :ink +green+ :line-thickness 2)
         (draw-points* *standard-output* graph2 :ink +red+ :line-thickness 2)
         (draw-points* *standard-output* graph3 :ink +blue+ :line-thickness 2)
         (medium-draw-lines* *standard-output* (list 0 0 (* 2 pi) 0))))
     (values))))

(let ((graph1 (fit-xy-to-window (|x,f(x)| 1000 0 (* 2 pi) #'sin) 1000 500))
      (graph2 (fit-xy-to-window (|x,f(x)| 1000 0 (* 2 pi) #'cos) 1000 500))
      (graph3 (fit-xy-to-window (|x,f(x)| 1000 0 (* 2 pi) #'tan) 1000 500))
      (graph4 (fit-xy-to-window (|x(t),y(t)| 10000 0 (* 2 pi) #'cos #'sin) 500 500))
      (graph5 (fit-xy-to-window (|z(t)| 1000 0 (* 2 pi) (cycloid 3 10 13 5)) 500 500)))
  (funcall
   (lambda ()
     (with-room-for-graphics (t :height 250)
       (with-translation (t 0 60)
         (draw-points* *standard-output* graph4 :ink +cyan+ :line-thickness 2)
         (draw-points* *standard-output* graph5 :ink +yellow+ :line-thickness 2)
         (medium-draw-lines* *standard-output* (list 0 0 (* 2 pi) 0))))
     (values))))

(let ((graph1 (fit-xy-to-window (|x,f(x)| 1000 0 (* 2 pi) #'sin) 1000 500))
      (graph2 (fit-xy-to-window (|x,f(x)| 1000 0 (* 2 pi) #'cos) 1000 500))
      (graph3 (fit-xy-to-window (|x,f(x)| 1000 0 (* 2 pi) #'tan) 1000 500))
      (graph4 (fit-xy-to-window (|x(t),y(t)| 10000 0 (* 2 pi) #'cos #'sin) 500 500))
      (graph5 (fit-xy-to-window (|z(t)| 1000 0 (* 2 pi) (cycloid 3 10 13 5)) 500 500))
      (graph6
       (fit-xy-to-window
        (|z(t)| 1000 0 (* 3 pi) #'(lambda (theta) (+ theta (exp (* #c(0 1) (- (* 3/2 pi) theta)))))) 750
        260)))
  (funcall
   (lambda ()
     (with-room-for-graphics (t :height 250)
       (with-translation (t 0 60)
         (draw-points* *standard-output* graph5 :ink +yellow+ :line-thickness 2)
         (draw-points* *standard-output* graph6 :ink +magenta+ :line-thickness 2)
         (medium-draw-lines* *standard-output* (list 0 0 (* 2 pi) 0))))
     (values))))


(defun make-random-color ()
  (make-rgb-color (/ (random 255) 255)
                  (/ (random 255) 255)
                  (/ (random 255) 255)))

(defun draw-rosette (stream x y radius n &rest drawing-options)
  (loop with alpha = (/ (* 2 pi) n)
        and radius = (/ radius 2)
        for i below n
        do (apply #'draw-circle* stream
                  (+ (* radius (cos (* alpha i))) x)
                  (+ (* radius (sin (* alpha i))) y)
                  radius
                  :filled nil
                  drawing-options)))

(let ((stream *standard-output*))
  (funcall
   (lambda ()
     (with-room-for-graphics (t)
       (draw-rosette stream 300 300 200 18 :ink +steel-blue+ :line-thickness 2))
     (values))))

(let ((stream *standard-output*))
  (funcall
   (lambda ()
     (with-room-for-graphics (t :height 240)
       (with-translation (t 0 70)
         (draw-rosette stream 100 100 100 18 :ink +steel-blue+ :line-thickness 2)))
     (values))))

(let ((stream *standard-output*))
  (funcall
   (lambda ()
     (with-room-for-graphics (t :height 240)
       (with-translation (t 0 70)
         (draw-rosette stream 100 100 100 18 :ink (make-random-color) :line-thickness 2)))
     (values))))

(let ((x 100) (y 100) (radius 200) (n 100) (stream *standard-output*))
  (with-room-for-graphics (t)
    (loop with alpha = (/ (* 2 pi) n)
          and radius = (/ radius 2)
          for i below n
          do (apply #'draw-circle* stream (+ (* radius (cos (* alpha i))) x)
                    (+ (* radius (sin (* alpha i))) y) radius :filled nil :ink
                    (list
                     (let ()
                       (funcall (lambda () (make-random-color))))))))
  (values))

(funcall
 (lambda ()
   (lol::fast-progn
    (with-drawing-options (t :line-thickness 3)
      (with-room-for-graphics (t)
        (with-scaling (t 30)
          (medium-draw-points* *standard-output* (|x,f(x)| 10 0 (* 2 pi) 0 :vertical t))
          (medium-draw-points* *standard-output* (|x,f(x)| 10 0 (* 2 pi) 0))))))
   (values)))


(let ((stream *standard-output*)
      (sine (fit-xy-to-window (|x,f(x)| 1000 0 (* 2 pi) #'sin) 500 250))
      (cosine (fit-xy-to-window (|x,f(x)| 1000 0 (* 2 pi) #'cos) 500 250))
      (y-axis (fit-xy-to-window (|x,f(x)| 1000 0 (* 2 pi) 0 :vertical t) 500 250))
      (x-axis (fit-xy-to-window (|x,f(x)| 1000 0 (* 2 pi) 0) 500 250)))
  (funcall
   (lambda ()
     (lol::fast-progn
      (with-drawing-options (t :line-thickness 4)
        (with-room-for-graphics (t)
          (with-translation (t 400 120)
            (with-scaling (t 1)
              (medium-draw-points* stream x-axis)
              (medium-draw-points* stream y-axis)
              (draw-points* stream sine :ink +green+)
              (draw-points* stream cosine :ink +red+))))))
     (values))))

;;anomaly
(let* ((stream *standard-output*)
       (width 600)
       (height 300)
       (x-range (list 1 (* 2 pi)))
       (sine (fit-xy-to-window (|x,f(x)| 1000 (first x-range) (second x-range) #'sin) width height))
       (cosine (fit-xy-to-window (|x,f(x)| 1000 (first x-range) (second x-range) #'cos) width height))
       (y-axis
        (fit-xy-to-window (|x,f(x)| 1000 (first x-range) (second x-range) 0 :vertical t) width height))
       (x-axis (fit-xy-to-window (|x,f(x)| 1000 (first x-range) (second x-range) 0) width height)))
  (funcall
   (lambda ()
     (lol::fast-progn
      (with-drawing-options (t :line-thickness 2)
        (with-room-for-graphics (t :height 300)
          (with-translation (t 300 120)
            (with-scaling (t 1)
              (medium-draw-points* stream x-axis)
              (medium-draw-points* stream y-axis)
              (draw-points* stream sine :ink +green+)
              (draw-points* stream cosine :ink +red+))))))
     (values))))

(let ((*trace-output* *standard-output*))
  (lol::safe-progn
   (time
    (let* ((stream *standard-output*)
           (width 600)
           (height 300)
           (x-range (list 1 (* 2 pi)))
           (sine (fit-xy-to-window (|x,f(x)| 1000 (first x-range) (second x-range) #'sin) width height))
           (cosine (fit-xy-to-window (|x,f(x)| 1000 (first x-range) (second x-range) #'cos) width height))
           (y-axis
            (fit-xy-to-window (|x,f(x)| 1000 (- (first x-range)) (second x-range) 0 :vertical t) width
                              height))
           (x-axis
            (fit-xy-to-window (|x,f(x)| 1000 (- (first x-range)) (second x-range) 0 :vertical nil) width
                              height)))
      (funcall
       (lambda ()
         (with-drawing-options (t :line-thickness 2)
           (with-room-for-graphics (t)
             (with-scaling (t 1)
               (lol::fast-progn (draw-points* stream x-axis) (draw-points* stream y-axis)
                                (with-translation (t (* (first x-range) 100) 200)
                                  (draw-points* stream cosine :ink +red+)
                                  (draw-points* stream sine :ink +green+))))))
         (values)))))))


(let ((*trace-output* *standard-output*))
  (lol::safe-progn
   (time
    (let* ((stream *standard-output*)
           (width 600)
           (height 300)
           (x-range (list 1 (* 2 pi)))
           (sine (fit-xy-to-window (|x,f(x)| 1000 (first x-range) (second x-range) #'sin) width height))
           (cosine (fit-xy-to-window (|x,f(x)| 1000 (first x-range) (second x-range) #'cos) width height))
           (y-axis
            (fit-xy-to-window (|x,f(x)| 1000 (- (first x-range)) (second x-range) 0 :vertical t) width
                              height))
           (x-axis
            (fit-xy-to-window (|x,f(x)| 1000 (- (first x-range)) (second x-range) 0 :vertical nil) width
                              height)))
      (funcall
       (lambda ()
         (with-drawing-options (t :line-thickness 2)
           (with-room-for-graphics (t)
             (draw-points* stream y-axis)
             (with-scaling (t 0.02)
               (lol::fast-progn (draw-points* stream x-axis)
                                (with-translation (t (* (first x-range) 100) 200)
                                  (draw-points* stream cosine :ink +red+)
                                  (draw-points* stream sine :ink +green+))))))
         (values)))))))

(let ((*trace-output* *standard-output*))
  (let* ((stream *standard-output*)
         (width 600)
         (height 300)
         (x-range (list 1 (* 2 pi)))
         (sine (fit-xy-to-window (|x,f(x)| 1000 (first x-range) (second x-range) #'sin) width height))
         (cosine (fit-xy-to-window (|x,f(x)| 1000 (first x-range) (second x-range) #'cos) width height))
         (y-axis
          (fit-xy-to-window (|x,f(x)| 1000 (- (first x-range)) (second x-range) 0 :vertical t) width
                            height))
         (x-axis
          (fit-xy-to-window (|x,f(x)| 1000 (- (first x-range)) (second x-range) 0 :vertical nil) width
                            height)))
    (funcall
     (lambda ()
       (with-drawing-options (t :line-thickness 2)
         (with-room-for-graphics (t)
           (draw-points* stream y-axis)
           (with-scaling (t 0.02)
             (draw-points* stream x-axis)
             (draw-points* stream cosine :ink +red+)
             (draw-points* stream sine :ink +green+))))
       (values)))))

;;without zooming in via fit-xy-to-window
(let* ((stream *standard-output*)
       (width 600)
       (height 300)
       (x-range (list 0 (* 2 pi)))
       (sine (|x,f(x)| 1000 (first x-range) (second x-range) #'sin))
       (cosine (|x,f(x)| 1000 (first x-range) (second x-range) #'cos))
       (y-axis (|x,f(x)| 1000 (first x-range) (second x-range) 0 :vertical t))
       (x-axis (|x,f(x)| 1000 (first x-range) (second x-range) 0)))
  (funcall
   (lambda ()
     (lol::fast-progn
      (with-drawing-options (t :line-thickness 2)
        (with-room-for-graphics (t :height 300)
          (with-translation (t 300 120)
            (with-scaling (t 100)
              (medium-draw-points* stream x-axis)
              (medium-draw-points* stream y-axis)
              (draw-points* stream sine :ink +green+)
              (draw-points* stream cosine :ink +red+))))))
     (values))))

(let* ((stream *standard-output*)
       (my-pic
        (lambda ()
          (with-room-for-graphics (t)
            (loop repeat 200
                  do (draw-line* stream (random 600) (random 900) (random 600) (random 900) :ink
                                 (make-random-color))))
          (values))))
  (funcall my-pic))


(let ((stream *standard-output*))
  (funcall
   (lambda ()
     (with-room-for-graphics (t)
       (draw-design stream (make-line* 0 0 100 100) :clipping-region +everywhere+ :transformation
                    +identity-transformation+))
     (values))))
