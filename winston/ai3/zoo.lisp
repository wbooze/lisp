;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; TEST RULES

(defun make-zoo-rules ()
  (clear-rules)
  (remember-rule '(Z1 IF
		      ((? animal) has hair)
		      THEN ((? animal) is a mammal)))
  (remember-rule '(Z2 IF
		      ((? animal) gives milk)
		      THEN ((? animal) is a mammal)))
  (remember-rule '(Z3 IF
		      ((? animal) has feathers)
		      THEN ((? animal) is a bird)))
  (remember-rule '(Z4 IF
		      ((? animal) flies)
		      ((? animal) lays eggs)
		      THEN ((? animal) is a bird)))
  (remember-rule '(Z5 IF
		      ((? animal) is a mammal)
		      ((? animal) eats meat)
		      THEN ((? animal) is a carnivore)))
  (remember-rule '(Z6 IF
		      ((? animal) is a mammal)
		      ((? animal) has pointed teeth)
		      ((? animal) has claws)
		      ((? animal) has forward-pointing eyes)
		      THEN ((? animal) is a carnivore)))
  (remember-rule '(Z7 IF
		      ((? animal) is a mammal)
		      ((? animal) has hoofs)
		      THEN ((? animal) is a ungulate)))
  (remember-rule '(Z8 IF
		      ((? animal) is a mammal)
		      ((? animal) chews cud)
		      THEN ((? animal) is a ungulate)))
  (remember-rule '(Z9 IF
		      ((? animal) is a carnivore)
		      ((? animal) has tawny color)
		      ((? animal) has dark spots)
		      THEN ((? animal) is a cheetah)))
  (remember-rule '(Z10 IF
		       ((? animal) is a carnivore)
		       ((? animal) has tawny color)
		       ((? animal) has black stripes)
		       THEN ((? animal) is a tiger)))
  (remember-rule '(Z11 IF
		       ((? animal) is a ungulate)
		       ((? animal) has long neck)
		       ((? animal) has long legs)
		       ((? animal) has dark spots)
		       THEN ((? animal) is a giraffe)))
  (remember-rule '(Z12 IF
		       ((? animal) is a ungulate)
		       ((? animal) has black stripes)
		       THEN ((? animal) is a zebra)))
  (remember-rule '(Z13 IF
		       ((? animal) is a bird)
		       ((? animal) does not fly)
		       ((? animal) has long legs)
		       ((? animal) has long neck)
		       ((? animal) is black and white)
		       THEN ((? animal) is a ostrich)))
  (remember-rule '(Z14 IF
		       ((? animal) is a bird)
		       ((? animal) does not fly)
		       ((? animal) swims)
		       ((? animal) is black and white)
		       THEN ((? animal) is a penguin)))
  (remember-rule '(Z15 IF
		       ((? animal) is a bird)
		       ((? animal) flies well)
		       THEN ((? animal) is a albatross)))
  #+comment
  (remember-rule '(Z16 IF
		       ((? animal) is a parent of (? child))
		       ((? animal) is a (? species))
		       THEN ((? child) is a (? species))))
  (format t "~%Zookeeper rules recorded.")
  (values))

;;;; TEST DATA

(defun make-stretch ()
  (clear-assertions)
  (remember-assertion '(Stretch has hair))
  (remember-assertion '(Stretch chews cud))
  (remember-assertion '(Stretch has long legs))
  (remember-assertion '(Stretch has long neck))
  (remember-assertion '(Stretch has tawny color))
  (remember-assertion '(Stretch has dark spots))
  (format t "~%Stretch's characteristics recorded:")
  (display-assertions)
  (values))

(defun make-swifty ()
  (clear-assertions)
  (remember-assertion '(Swifty has hair))
  (remember-assertion '(Swifty has pointed teeth))
  (remember-assertion '(Swifty has claws))
  (remember-assertion '(Swifty has forward-pointing eyes))
  (remember-assertion '(Swifty has tawny color))
  (remember-assertion '(Swifty has dark spots))
  (format t "~%Swifty's characteristics recorded:")
  (display-assertions)
  (values))

(defun make-splashy ()
  (clear-assertions)
  (remember-assertion '(Splashy has feathers))
  (remember-assertion '(Splashy lays eggs))
  (remember-assertion '(Splashy does not fly))
  (remember-assertion '(Splashy is black and white))
  (remember-assertion '(Splashy swims))
  (format t "~%Splashy's characteristics recorded:")
  (display-assertions)
  (values))

(defun make-sandy ()
  (clear-assertions)
  (remember-assertion '(Sandy has long legs))
  (remember-assertion '(Sandy has long neck))
  (remember-assertion '(Sandy has feathers))
  (remember-assertion '(Sandy lays eggs))
  (remember-assertion '(Sandy does not fly))
  (remember-assertion '(Sandy is black and white))
  (remember-assertion '(Sandy swims))
  (format t "~%Sandy's characteristics recorded:")
  (display-assertions)
  (values))

