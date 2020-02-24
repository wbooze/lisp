;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; TEST RULES

(defun make-bagger-rules ()
  (clear-rules)
  (remember-rule '(B1 IF
		      (step is check-order)
		      ((? _) potato-chips (? _) (? _) (? _))
		      AND-IF
		      (= 0 (count-assertions '((? _) pepsi (? _) (? _) (? _))))
		      ADD (item0 Pepsi Bottle Large No)
		      SAYING
		      ("Add a bottle of Pepsi to the order.")))
  (remember-rule '(B2 IF
		      (step is check-order)
		      ((? bag) is current-bag)
		      DELETE (step is check-order) 
		      ADD (step is bag-large-items)
		      SAYING
		      ("Bagging large items.")))
  (remember-rule '(B3 IF
		      (step is bag-large-items)
		      ((? bag) is current-bag)
		      ((? item) (? class) (? _) large (? _))
		      ((? item) (? _) bottle (? _) (? _))
		      AND-IF
		      (< (count-assertions '((? bag) contains (? _) large)) 3)
		      DELETE ((? item) (? _) (? _) (? _) (? _)) 
		      ADD ((? bag) contains (? item) large)
		      SAYING ("Put ~a, ~a, in ~a."
			      (? item) (? class) (? bag))))
  (remember-rule '(B4 IF
		      (step is bag-large-items)
		      ((? bag) is current-bag)
		      ((? item) (? class) (? _) large (? _))
		      AND-IF
		      (< (count-assertions '((? bag) contains (? _) large)) 3)
		      DELETE ((? item) (? _) (? _) (? _) (? _)) 
		      ADD ((? bag) contains (? item) large)
		      SAYING ("Put ~a, ~a, in ~a."
			      (? item) (? class) (? bag))))
  (remember-rule '(B5 IF
		      (step is bag-large-items)
		      ((? bag) is current-bag)
		      ((? item) (? _) (? _) large (? _))
		      ((? new-bag) is available-bag)
		      DELETE
		      ((? bag) is current-bag)
		      ((? new-bag) is available-bag)
		      ADD ((? new-bag) is current-bag)
		      SAYING ("Start a fresh bag.")))
  (remember-rule '(B6 IF
		      (step is bag-large-items)
		      ((? bag) is current-bag)
		      DELETE (step is bag-large-items) 
		      ADD (step is bag-medium-items)
		      SAYING
		      ("Bagging medium items.")))
  (remember-rule '(B7 IF
		      (step is bag-medium-items)
		      ((? bag) is current-bag)
		      ((? item) (? name) (? _) medium yes)
		      ADD ((? item) in freezer-bag)
		      SAYING ("Put ~a, ~a, in a freezer bag."
			      (? item) (? name))))
  (remember-rule '(B8 IF
		      (step is bag-medium-items)
		      ((? bag) is current-bag)
		      ((? item) (? class) (? _) medium (? _))
		      AND-IF
	      (= (count-assertions '((? bag) contains (? _) large)) 0)
	      (< (count-assertions '((? bag) contains (? _) medium)) 6)
		      DELETE ((? item) (? _) (? _) (? _) (? _)) 
		      ADD ((? bag) contains (? item) medium)
		      SAYING ("Put ~a, ~a, in ~a."
			      (? item) (? class) (? bag))))
  (remember-rule '(B9 IF
		      (step is bag-medium-items)
		      ((? bag) is current-bag)
		      ((? item) (? _) (? _) medium (? _))
		      ((? new-bag) is available-bag)
		      DELETE
		      ((? bag) is current-bag)
		      ((? new-bag) is available-bag)
		      ADD ((? new-bag) is current-bag)
		      SAYING ("Start a fresh bag.")))
  (remember-rule '(B10 IF
		      (step is bag-medium-items)
		      ((? bag) is current-bag)
		      DELETE (step is bag-medium-items) 
		      ADD (step is bag-small-items)
		      SAYING
		      ("Bagging small items.")))
  (remember-rule '(B11 IF
		      (step is bag-small-items)
		      ((? bag) is current-bag)
		      ((? item) (? class) (? _) small (? _))
		      AND-IF
		      (= (count-assertions '((? bag) contains (? _) large)) 0)
		      (= (count-assertions '((? bag) contains (? _) medium)) 0)
		      (< (count-assertions '((? bag) contains (? _) small)) 12)
		      DELETE ((? item) (? _) (? _) (? _) (? _)) 
		      ADD ((? bag) contains (? item) small)
		      SAYING ("Put ~a, ~a, in ~a."
			      (? item) (? class) (? bag))))
  (remember-rule '(B12 IF
		      (step is bag-small-items)
		      ((? bag) is current-bag)
		      ((? item) (? _) (? _) small (? _))
		      ((? new-bag) is available-bag)
		      DELETE
		      ((? bag) is current-bag)
		      ((? new-bag) is available-bag)
		      ADD ((? new-bag) is current-bag)
		      SAYING ("Start a fresh bag.")))
  (remember-rule '(B13 IF
		      (step is bag-small-items)
		      ((? bag) is current-bag)
		      DELETE
		      ((? bag) is current-bag)
		      (step is bag-small-items)
		      ADD (step is done)
		      SAYING
		      ("Bagging done.")))
  (format t "~%Bagger rules recorded.")
  (values))

;;;; TEST DATA

(defun make-groceries ()
  (clear-assertions)
  (remember-assertion '(step is check-order))
  (remember-assertion '(bag1 is current-bag))
  (remember-assertion '(bag2 is available-bag))
  (remember-assertion '(bag3 is available-bag))
  (remember-assertion '(bag4 is available-bag))
  (remember-assertion '(item1 Bread Plastic-bag Medium No))
  (remember-assertion '(item2 Glop Jar Small No))
  (remember-assertion '(item3 Granola Cardboard-box Large No))
  (remember-assertion '(item4 Ice-cream Cardboard-carton Medium Yes))
  (remember-assertion '(item5 Potato-chips Plastic-bag Medium No))
  (format t "~%Bagger grocery list recorded:")
  (display-assertions)
  (values))



