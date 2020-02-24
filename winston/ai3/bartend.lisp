;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

(defun make-bartender-rules ()
  (clear-rules)
  (remember-rule '(B1 IF
		      (expensive wine is indicated)
		      (it is New Years Eve)
		      NOT
		      (choose (? _))
		      THEN (choose Bonds-Champagne)))  
  (remember-rule '(B2 IF
		      (expensive wine is indicated)
		      (entree is steak)
		      NOT
		      (choose (? _))
		      THEN (choose Chateau-Earl-of-Bartonville-Red)))	
  (remember-rule '(B3 IF
		      (cheap wine is indicated)
		      (entree is chicken)
		      (guest is not well liked)
		      NOT
		      (choose (? _))
		      THEN (choose Honest-Henrys-Apple-Wine)))  
  (remember-rule '(B4 IF
		      (cheap wine is indicated)
		      (entree is unknown)
		      NOT
		      (choose (? _))
		      THEN (choose Toe-Lakes-Rose)))  
  (remember-rule '(B5 IF
		      (beer is indicated)
		      (entree is Mexican)
		      NOT
		      (choose (? _))
		      THEN (choose Dos-Equis))) 
  (remember-rule '(B6 IF
		      (beer is indicated)
		      NOT
		      (choose (? _))
		      THEN (choose Coors)))
  (remember-rule '(B7 IF
		      (guest is a health nut)
		      NOT
		      (choose (? _))
		      THEN (choose Glop)))  
  (remember-rule '(B8 IF
		      (guest is a health nut)
		      (carrots are not to be served)
		      NOT
		      (choose (? _))
		      THEN (choose carrot-juice)))  
  (remember-rule '(B9 IF
		      (wine is indicated)
		      (guest should be impressed)
		      THEN (expensive wine is indicated)))  
  (remember-rule '(B10 IF
		       (wine is indicated)
		       THEN (cheap wine is indicated)))  
  (remember-rule '(B11 IF
		       (guest is sophisticated)
		       THEN (wine is indicated)))  
  (remember-rule '(B12 IF
		       (entree is Mexican)
		       THEN (beer is indicated)))  
  (remember-rule '(B13 IF
		       (guest is not well liked)
		       (entree is catered by Death-wish Caterers)
		       THEN (beer is indicated)))  
  (remember-rule '(B14 IF
		       (entree is (? anything))
		       NOT
		       (choose (? _))
		       THEN (choose water)))
  (format t "~%Bartender rules recorded.")
  (values))

(defun make-dinner-assertions ()
  (clear-assertions)
  (remember-assertion '(Entree is catered by Death-wish Caterers))
  (remember-assertion '(Entree is Mexican))
  (remember-assertion '(Guest is not well liked))
  (remember-assertion '(Guest is sophisticated))
  (remember-assertion '(It is New Years Eve))
  (remember-assertion '(Entree is chicken))
  (format t "~%Dinner assertions recorded."))


