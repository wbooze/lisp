
;;;;; doin the Triangles!
(progn
  (defclass shape () () 
    (:documentation "The foundation of all shapes."))


  ;; this one implements a triangle via sides!
  ;; we later on want one with via angles just!
  ;; and a default scale of 1!

  (defclass triangle (shape)
    ((a :reader side-a :initarg :side-a)
      (b :reader side-b :initarg :side-b)
      (c :reader side-c :initarg :side-c)))

  (defun make-triangle (a b c)
    ;;all sides should be represented as floats
    (make-instance 'triangle 
      :side-a (coerce a 'float)
      :side-b (coerce b 'float)
      :side-c (coerce c 'float)))

;;; Return the angle A between adjacent sides b and c
;;; and opposite side a in radians, given all sides of a triangle
;;; Law of Cosines: a^2 = b^2 + c^2 - 2bc(cos A)
  (defun three-sides-to-angle (a b c)
    (acos (/ (- (+ (expt b 2)
		  (expt c 2))
	       (expt a 2))
	    (* 2 b c))))

  (defmethod angle-A ((tri triangle) &key)
    (three-sides-to-angle
      (side-a tri) (side-b tri) (side-c tri)))

  (defmethod angle-A :around ((tri triangle) &key deg)
    (let ((result (call-next-method)))
      (format t "~A rad, ~A degrees " result (truncate (rad->deg result)))
      (if (eq deg t)
        (rad->deg result)
        result)))

  (defmethod angle-B ((tri triangle) &key deg)
    (three-sides-to-angle
      (side-b tri) (side-c tri) (side-a tri)))

  (defmethod angle-B :around ((tri triangle) &key deg)
    (let ((result (call-next-method)))
      (format t "~A rad, ~A degrees " result (truncate (rad->deg result)))
      (if (eq deg t)
        (rad->deg result)
        result)))

  (defmethod angle-C ((tri triangle) &key deg)
    (three-sides-to-angle
      (side-c tri) (side-a tri) (side-b tri)))

  (defmethod angle-C :around ((tri triangle) &key deg)
    (let ((result (call-next-method)))
      (format t "~A rad, ~A degrees " result (truncate (rad->deg result)))
      (if (eq deg t)
        (rad->deg result)
        result)))

;;; here we tried to define an after method for the primary method of the class, which is the wrong
;;; way. We intended to print a symbol/string not after the primary method returns but it's writer!
  ;; (defmethod angle-A :after ((tri triangle))
  ;;  (princ 'pi))

;;; so instead of the above we should have defined an after method for the primary writer method
;;; of the slot accessor gf.

  (defgeneric dimensions (shape)
    (:documentation "Returns list of angles."))

  (defgeneric area (shape)
    (:documentation "Returns area of the shape."))

  (defmethod dimensions ((tri triangle))
    (list 
      (side-a tri)
      (side-b tri)
      (side-c tri)))

  (defun deg->rad (deg &rest args)
    (/ (* deg pi) 180))

  (defun rad->deg (rad &rest args)
    (/ (* rad 180) pi))

  (defmethod angles ((tri triangle) &key deg)
    (let* ((gf1 (symbol-function 'angle-A))
	    (gf2 (symbol-function 'angle-B))
	    (gf3 (symbol-function 'angle-C))
	    (meth1 (find-method gf1 '(:around) (list (find-class 'triangle))))
	    (meth2 (find-method gf2 '(:around) (list (find-class 'triangle))))
	    (meth3 (find-method gf3 '(:around) (list (find-class 'triangle)))))
      (if (eq deg t)
        (mapcar #'rad->deg
	  (list
	    (angle-A tri)
	    (angle-B tri)
	    (angle-C tri))) 
        (list
	  (angle-A tri)
	  (angle-B tri)
	  (angle-C tri)))))

;;; Return the area of a triangle
;;; Algorithm is: area = ab(sin C)/2

  (defmethod area ((tri triangle))
    (let* ((gf (symbol-function 'angle-C))
	    (meth (find-method gf '(:around) (list (find-class 'triangle)))))
      (remove-method gf meth)
      (* (side-a tri) (side-b tri) (sin (angle-C tri)) .5)))

  (defmethod area :around ((tri triangle))
    (let* ((result (call-next-method)))
      (format t "~A unit square " result) result)))


;;; test
;; (setq my-tri (make-triangle 60 60 60))
;; (angle-a my-tri :deg t)
;; => 1.0471976 RAD
;; => 60.000000151832296d
