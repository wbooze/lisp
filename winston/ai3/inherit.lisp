
(require 'clos "/phw/modules/clos")

(defclass a (b)
	  ((size :initform (progn (print '(hacking class a)) 'small))))

(defclass b (c)
	  ((size :initform (progn (print '(hacking class b)) 'large))
	   (weight :initform (progn (print '(hacking class b)) 'heavy))))


(describe (make-instance 'a))