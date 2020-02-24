;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

(require 'cl "/phw/modules/cl.lisp")	;Delete if using full Common Lisp

;;; F2 is a patch for GC Lisp's lack of format control.

;;;; Main Procedures

;;; Print lines:

(defun points (n) (* 72 n))

(defun plines (points)
  (do* ((points points (rest (rest points)))
	(p1 (first points) (first points))
	(p2 (second points) (second points)))
       ((endp points))
    (format *terminal-io* "~%~a ~a ~a ~a"
	    (f2 (first p1)) (f2 (second p1))
	    (f2 (first p2)) (f2 (second p2))))
  (pause)
  (send *terminal-io* :clear-screen)
  points)

(defun p-ai-header ()
  (format t "%!PS-Adobe-2.0 EPSF-1.2
%%Creator:Adobe Illustrator 88(TM)  Micrografx, Inc.
%%BoundingBox:72 0 433 577
%%TemplateBox:252 288 252 288
%%EndComments
%%EndProlog
%%BeginSetup
%%EndSetup
0 A
u
01.00 01.00 01.00 0.00 k 
01.00 01.00 01.00 0.00 K 
0 O 
0 R 
1 j 
1 J 
0.00 w 
10 M 
%%Note: Export AI Micrografx"))

(defun p-ai-trailer ()
  (format t "~%U
%%Trailer
"))

(defun p-ai-lines (points)
  (do* ((points points (rest (rest points)))
	(p1 (first points) (first points))
	(p2 (second points) (second points)))
       ((endp points))
    (format t "~%01.00 01.00 1.00 0.00 k ~
	    ~%0.00 w ~
	    ~%~a ~a m~
	    ~%~a ~a l~
	    ~%S"
	    (first p1) (second p1)
	    (first p2) (second p2))))

;;; Scale, translate, rotate, etc.:

(defun scale-points (points scale)
  (mapcar #'(lambda (p) (mapcar #'(lambda (e) (* scale e)) p))
	  points))

(defun translate-points (points offsets)
  (mapcar #'(lambda (p) (mapcar #'(lambda (x y) (+ x y)) p offsets))
	  points))

(defun invert-y (points &optional (middle 7.5))
  (mapcar #'(lambda (e)
	      (setf (second e)
		    (- middle (second e)))
	      e)
	  points))

(defun rotate-points-x (points angle &aux result)
  ;;This procedure rotates the coordinates of the points through the angle
  ;;around the y axis.
  (labels ((rotate-point (xyz r)
	     (let* ((x (first xyz))
		    (y (second xyz))
		    (z (third xyz))
		    (vector (list x
				  (+ (* z (sin r)) (* y (cos r)))
				  (- (* z (cos r)) (* y (sin r))))))
	       vector)))
    (let ((r (/ (* pi angle) 180.0)))
      (dolist (p points (reverse result))
	(push (rotate-point p r) result)))))

(defun rotate-points-y (points angle &aux result)
  ;;This procedure rotates the coordinates of the points through the angle
  ;;around the y axis.
  (labels ((rotate-point (xyz r)
	     (let* ((x (first xyz))
		    (y (second xyz))
		    (z (third xyz))
		    (vector (list (- (* x (cos r)) (* z (sin r)))
				  y
				  (+ (* x (sin r)) (* z (cos r))))))
	       vector)))
    (let ((r (/ (* pi angle) 180.0)))
      (dolist (p points (reverse result))
	(push (rotate-point p r) result)))))

(defun rotate-points-z (points angle &aux result)
  ;;This procedure rotates the coordinates of the points through the angle
  ;;around the z axis.
  (labels ((rotate-point (xyz r)
	     (let* ((x (first xyz))
		    (y (second xyz))
		    (z (third xyz))
		    (vector (list (+ (* y (sin r)) (* x (cos r)))
				  (- (* y (cos r)) (* x (sin r)))
				  z)))
	       vector)))
    (let ((r (/ (* pi angle) 180.0)))
      (dolist (p points (reverse result))
	(push (rotate-point p r) result)))))

(defun orthographic-projection (points &aux result)
  (labels ((project-point (point)
	     ;; Point is (x, y, z)
	     (let ((vector
		     (let ((x (float (first point)))
			   (y (float (second point))))
		       (list x y))))
	       ;(setf vector (mapcar #'(lambda (e) (* e scale)) vector))
	       ;(incf (first vector) (first offsets))
	       ;(incf (second vector) (second offsets))
	       ;(when invert-y (setf (second vector) (- (second vector))))
	       vector)))
    (dolist (p points (reverse result))
      (let ((projection (project-point p)))
	(push projection result)))))

(defun movex-scale (points &optional (offset 0.0) (scale 1.0))
  (scale-points (translate-points points (list (float offset) 0.0 0.0))
		(float scale)))

(defun perspective-projection (points y-z-lens z-plane scale offsets
			      &optional invert-y
			      &aux result)
  ;;This procedure does a perspective projection onto a plane perpedicular
  ;;to the z axis located at Z-PLANE via a lens center located at Y-Z-LENS
  ;;The result is scaled by SCALE and offset by OFFSETS
  ;;To make drawing easier, the y axis can be inverted by setting INVERT-Y to T
  (labels ((project-point (point)
	     ;; Point is (x, y, z)
	     ;; Lens point is just (y, z) because x = 0
	     ;; Focal plane is just z because it is perpendicular to z axis
	     ;; Lens to plane distance:
	     (let* ((lens-to-plane (float (- z-plane (second y-z-lens))))
		    (vector
		      (let ((x (float (first point)))
			    (y (float (- (second point) (first y-z-lens))))
			    (z (float (- (third point) (second y-z-lens)))))
			(list (/ x z) (/ y z)))))
	       (setf vector
		     (mapcar #'(lambda (e) (* e lens-to-plane scale)) vector))
	       (incf (first vector) (first offsets))
	       (incf (second vector) (second offsets))
	       (when invert-y
		 (setf (second vector) (- (second vector))))
	       vector)))
    (dolist (p points (reverse result))
      (let ((projection (project-point p)))
	(format t "~%x = ~a, y = ~a"
		(f2 (first projection)) (f2 (second projection)))
	(push projection result)))))
       
(defun predict (xcoefficients ycoefficients &rest images)
  (mapcar #'(lambda (x y) (list x y))
	  (predictaux #'first xcoefficients images)
	  (predictaux #'second ycoefficients images)))

(defun predictaux (function coefficients images)
  (let* ((point-lists (apply #'transpose-lists images))
	 (value-lists (mapcar #'(lambda (images) (mapcar function images))
			      point-lists)))
    (mapcar #'(lambda (value-list)
		(apply #'+ (mapcar #'*
				   coefficients
				   (append value-list (list 1.0)))))
	    value-lists)))

;;; Compare predicted results with reality:

;; One axis only, 2 images:

(defun print-overlay (predicted actual)
  (p-ai-header)
  (p-ai-lines (movex-scale predicted 6 18))
  (p-ai-lines (movex-scale actual 0 18))
  (p-ai-trailer))

(defun print-comparison-table (predicted actual)
  (format t "~%Predicted,	Actual")
  (mapcar #'(lambda (x y) (format t "~%~a, ~a	~a, ~a"
				  (f2 (first x))
				  (f2 (second x))
				  (f2 (first y))
				  (f2 (second y))))
	  (odd predicted)
	  (odd actual)))

(defun compare2 (offset scale u &rest models)
  (print-comparison-table
    (movex-scale
      (apply #'predict
	     (apply #'solve-for-prediction-coefficients
		    #'first
		    u
		    models)
	     (apply #'solve-for-prediction-coefficients
		    #'second
		    u
		    models)
	     models)
      offset
      scale)
    (movex-scale u offset scale))
  (values))

;; General situation, 3 images:

(defun compare3 (u &rest models)
  (let ((actual u)
	(predicted
	  (apply #'predict
	    (apply #'solve-for-prediction-coefficients
		   #'first
		   u
		   models)
	    (apply #'solve-for-prediction-coefficients
		   #'second
		   u
		   models)
	    models)))
    (values actual predicted)))

;;; Solve linear equations:

(defun solve-homogeneous-equations (rows)
  ;;Rows are to be of the following form:
  ;;c11 x + c12 y + c13 z + d = c1
  ;;c21 x + c22 y + c23 z + d = c2
  ;;c31 x + c32 y + c33 z + d = c3
  ;;c41 x + c42 y + c43 z + d = c4
  (setf rows (mapcar #'(lambda (e) (append e nil)) rows))
  (setf rows (mapcar #'(lambda (e) (append (butlast e) (cons 1 (last e))))
		     rows))
  (solve-equations rows))

(defun solve-equations (rows &aux solution)
  ;;Rows are to be of the following form:
  ;;c11 x + c12 y + c13 z = c1
  ;;c21 x + c22 y + c23 z = c2
  ;;c31 x + c32 y + c33 z = c3
  (setf rows (mapcar #'(lambda (e) (append e nil)) rows))
  (dotimes (n (length rows))
    (setf (nth n rows)
	  (scale-row (/ (float (nth n (nth n rows)))) (nth n rows)))
    (dotimes (m (length rows))
      (unless (= m n)
	(setf (nth m rows)
	      (add-two-rows (nth m rows)
			    (scale-row (- (nth n (nth m rows)))
				       (nth n rows)))))))
  (setf solution (apply #'append (mapcar #'last rows)))
  (format t "~%Equation solution:")
  (mapcar #'(lambda (e) (format t " ~a" (f2 e))) solution)
  solution)

(defun add-two-rows (r1 r2) (mapcar #'+ r1 r2))

(defun scale-row (c r) (mapcar #'(lambda (e) (* c e)) r))

(defun solve-for-prediction-coefficients (function unknown &rest models)
  ;;Assumes points are grouped in pairs to define lines---
  ;;Skips even points.
  (let* ((size (length models))
	 (images
	   (mapcar #'(lambda (image) (odd image size))
		   (append models (list unknown))))
	 (coordinates
	   (mapcar #'(lambda (image) (mapcar function image)) images)))
    (setf coordinates (apply #'transpose-lists coordinates))
    (solve-homogeneous-equations coordinates)))

(defun transpose-lists (&rest lists)
  (if (not (apply #'append lists))
      nil
    (cons (mapcar #'first lists)
	  (apply #'transpose-lists (mapcar #'rest lists)))))

(defun odd (list &optional (n -1))
  (if (or (zerop n) (endp list))
      nil
    (cons (first list) (odd (rest (rest list)) (1- n)))))

(defmacro make-object (name &rest points)
  `(progn (setf ,name ',(floatpoints points)) ',name))

(defun floatpoints (points) (mapcar #'floatpoint points))

(defun floatpoint (p) (mapcar #'float p))

;;; Data; Obelisk, Jukebox, and Sofa coordinates:

(make-object triangle
  (-1	13	2)	(3	10	2)
  (3	10	2)	(0	13	-1)
  (0	13	-1)	(-1	13	2)
  )
(make-object triangle1
  (-1	13	3)	(3	10	2)
  (3	10	2)	(0	13	0)
  (0	13	0)	(-1	13	3)
  )
(make-object triangle2
  (-1	13	2)	(3	10	0)
  (3	10	0)	(0	13	-1)
  (0	13	-1)	(-1	13	2)
  )

(make-object obelisk
  (-2	13	+2)	(2	13	+2)	;1-->2
  (2	13	+2)	(2	10	0)	;2-->3
  (-2	0	+2)	(-2	13	+2)	;7-->1
  (2	10	-0)	(2	8	-2)	;3-->4
  (2	8	-2)	(2	0	-2)	;4-->5
  (2	0	-2)	(-2	0	-2)	;5-->6
  (-2	0	-2)	(-2	0	+2)	;6-->7
  (-2	13	+2)	(-2	10	0)	;1-->8
  (-2	10	0)	(2	10	-0)	;8-->3
  (-2	10	0)	(-2	8	-2)	;8-->9
  (-2	8	-2)	(2	8	-2)	;9-->4
  (-2	8	-2)	(-2	0	-2)	;9-->6
  )

(make-object jukebox
  (-3	13	+2)	(3	13	+2)	;1-->2
  (3	13	+2)	(3	10	0)	;2-->3
  (-3	0	+1)	(-3	13	+2)	;7-->1
  (3	10	0)	(3	8	-3)	;3-->4
  (3	8	-3)	(3	0	-2)	;4-->5
  (3	0	-2)	(-3	0	-2)	;5-->6
  (-3	0	-2)	(-3	0	+1)	;6-->7
  (-3	13	+2)	(-3	10	0)	;1-->8
  (-3	10	0)	(3	10	0)	;8-->3
  (-3	10	0)	(-3	8	-3)	;8-->9
  (-3	8	-3)	(3	8	-3)	;9-->4
  (-3	8	-3)	(-3	0	-2)	;9-->6
  )

(make-object sofa
  (-3	8	+2)	(3	8	+2)	;1-->2
  (3	8	+2)	(3	3	0)	;2-->3
  (-3	0	+1)	(-3	8	+2)	;7-->1
  (3	3	0)	(3	3	-3)	;3-->4
  (3	3	-3)	(3	0	-2)	;4-->5
  (3	0	-2)	(-3	0	-2)	;5-->6
  (-3	0	-2)	(-3	0	+1)	;6-->7
  (-3	8	+2)	(-3	3	0)	;1-->8
  (-3	3	0)	(3	3	0)	;8-->3
  (-3	3	0)	(-3	3	-3)	;8-->9
  (-3	3	-3)	(3	3	-3)	;9-->4
  (-3	3	-3)	(-3	0	-2)	;9-->6
  )


;;;; Tests

;;; Perspective attempt

#+comment
(with-open-file (output "/phw/ai3/perspect.ai" :direction :output)
  (let ((*standard-output* output))
    (p-ai-header)
    (p-ai-lines
      (movex-scale
	(perspective-projection (append (translate-points obelisk '(-6 0 0))
					(translate-points sofa '(6 0 0)))
				'(10 20)
				10
				.25
				'(3 -5)
				t)
	0 32))
    (p-ai-trailer)))

(with-open-file (output "/phw/ai3/ojs.ai" :direction :output)
  "Look at three objects."
  (let ((*standard-output* output))
    (p-ai-header)
    (do ((objects (list obelisk jukebox sofa) (rest objects))
	 (offsets '(3 9 15) (rest offsets)))
	((endp objects))
      (let* ((object (first objects))
	     (offset (first offsets)))
	(p-ai-lines
	  (movex-scale (rotate-points-y object 45) offset 18))))
    (p-ai-trailer)))

(with-open-file (output "/phw/ai3/oooo.ai" :direction :output)
  "Rotate obelisk, with offsets, for drawing models."
  (let ((*standard-output* output))
    (p-ai-header)
    (do ((angles '(0 30 60 90) (rest angles))
	 (offsets '(0 5 10 15) (rest offsets)))
	((endp angles))
      (let* ((angle (first angles))
	     (offset (first offsets)))
	(p-ai-lines (movex-scale (rotate-points-y obelisk angle) offset 18))))
    (p-ai-trailer)))

(with-open-file (output "/phw/ai3/offset1.ai" :direction :output)
  "Rotate obelisk for explaining correspondence by proximity."
  (let ((*standard-output* output))
    (p-ai-header)
    (do ((angles '(30 40 50 60) (rest angles))
	 (offsets '(0  0  0  0) (rest offsets)))
	((endp angles))
      (let* ((angle (first angles))
	     (offset (first offsets)))
	(p-ai-lines (movex-scale (rotate-points-y obelisk angle) offset 18))))
    (p-ai-trailer)))

(with-open-file (output "/phw/ai3/offset2.ai" :direction :output)
  "Rotate obelisk for explaining correspondence by proximity."
  (let ((*standard-output* output))
    (p-ai-header)
    (do ((angles '(0 10 20 30 40) (rest angles))
	 (offsets '(0 0  0  0  0) (rest offsets)))
	((endp angles))
      (let* ((angle (first angles))
	     (offset (first offsets)))
	(p-ai-lines
	  (movex-scale (rotate-points-y obelisk angle) offset 18))))
    (p-ai-trailer)))

;;; Compare Obelisk to obelisk model:

(trace predict)

#+comment
(compare2 0 1
  ;;Unknown is rotated:
  (rotate-points-y triangle2 30)
  (rotate-points-y triangle 15)
  (rotate-points-y triangle 0))
t2: Equation solution:  +1.44  -0.28
Equation solution:  -0.00  +1.00
Predicted,	Actual
 -1.86, +13.00	 -1.86, +13.00
 +2.59, +10.00	 +2.59, +10.00
 +0.37, +13.00	 +0.49, +13.00
t1: Equation solution:  +2.65  -1.57
Equation solution:  -0.00  +1.00
Predicted,	Actual
 -2.36, +13.00	 -2.36, +13.00
 +1.59, +10.00	 +1.59, +10.00
 +0.68, +13.00	 -0.00, +13.00
T0: Equation solution:  +1.93  -0.99
Equation solution:  -0.00  +1.00
Predicted,	Actual
 -1.86, +13.00	 -1.86, +13.00
 +1.59, +10.00	 +1.59, +10.00
 +0.49, +13.00	 +0.49, +13.00


#+comment
(compare2 0 1
  ;;Unknown is rotated:
  (rotate-points-y obelisk 45)
  (rotate-points-y obelisk 30)
  (rotate-points-y obelisk 60))

;;; Compare Jukebox to obelisk model:

#+comment
(compare2 0 1
  ;;Unknown is rotated:
  (rotate-points-y jukebox 45)
  (rotate-points-y obelisk 30)
  (rotate-points-y obelisk 60))

;;; 2D Treatment

(with-open-file (output "/phw/ai3/2do-to-o.ai" :direction :output)
  "Obelisk to obelisk in three-d."
  (multiple-value-bind (actual predicted)
    (compare3
      ;;Unknown is rotated and tilted:
      (rotate-points-y obelisk 45)
      (rotate-points-y obelisk 30)
      (rotate-points-y obelisk 60))
    (let ((*standard-output* output))
      #+comment
      (print-comparison-table actual predicted)
      (print-overlay actual predicted))))

(with-open-file (output "/phw/ai3/2dj-to-o.ai" :direction :output)
  "Jukebox to obelisk in three-d."
  (multiple-value-bind (actual predicted)
    (compare3
      ;;Unknown is rotated and tilted:
      (rotate-points-y jukebox 45)
      (rotate-points-y obelisk 30)
      (rotate-points-y obelisk 60))
    (let ((*standard-output* output))
      #+comment
      (print-comparison-table actual predicted)
      (print-overlay actual predicted))))

(with-open-file (output "/phw/ai3/2ds-to-o.ai" :direction :output)
  "Sofa to obelisk in three-d."
  (multiple-value-bind (actual predicted)
    (compare3
      ;;Unknown is rotated and tilted:
      (rotate-points-y sofa 45)
      (rotate-points-y obelisk 30)
      (rotate-points-y obelisk 60))
    (let ((*standard-output* output))
      #+comment
      (print-comparison-table actual predicted)
      (print-overlay actual predicted))))

(with-open-file (output "/phw/ai3/3dooo.ai" :direction :output)
  "Obelisk Models"
  (let ((*standard-output* output))
    (p-ai-header)
    (do ((objects (list
		    (rotate-points-x obelisk 30)
		    (rotate-points-y obelisk 30)
		    (rotate-points-z obelisk 30))
		  (rest objects))
	 (offsets '(0 8 16) (rest offsets)))
	((endp objects))
      (let* ((object (first objects))
	     (offset (first offsets)))
	(p-ai-lines (movex-scale object offset 18))))
    (p-ai-trailer)))

(with-open-file (output "/phw/ai3/o-to-o.ai" :direction :output)
  "Obelisk to obelisk in three-d."
  (multiple-value-bind (actual predicted)
    (compare3
      ;;Unknown is rotated and tilted:
      (rotate-points-y (rotate-points-x obelisk 15) 45)
      (rotate-points-x obelisk 30)
      (rotate-points-y obelisk 30)
      (rotate-points-z obelisk 30))
    (let ((*standard-output* output))
      #+comment
      (print-comparison-table actual predicted)
      (print-overlay actual predicted))))

(with-open-file (output "/phw/ai3/j-to-o.ai" :direction :output)
  "Jukebox to obelisk in three-d."
  (multiple-value-bind (actual predicted)
    (compare3
      ;;Unknown is rotated and tilted:
      (rotate-points-y (rotate-points-x jukebox 15) 45)
      (rotate-points-x obelisk 30)
      (rotate-points-y obelisk 30)
      (rotate-points-z obelisk 30))
    (let ((*standard-output* output))
      #+comment
      (print-comparison-table actual predicted)
      (print-overlay actual predicted))))

(with-open-file (output "/phw/ai3/s-to-o.ai" :direction :output)
  "Sofa to obelisk in three-d."
  (multiple-value-bind (actual predicted)
    (compare3
      ;;Unknown is rotated and tilted:
      (rotate-points-y (rotate-points-x sofa 15) 45)
      (rotate-points-x obelisk 30)
      (rotate-points-y obelisk 30)
      (rotate-points-z obelisk 30))
    (let ((*standard-output* output))
      #+comment
      (print-comparison-table actual predicted)
      (print-overlay actual predicted))))

