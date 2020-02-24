;;;; -*- mode:Lisp; package:user -*- ;;;;
;;;; Created: 22 August 1991

(defun alter (ignore x y)
  (let ((xoff 108) (yoff 115))
    (list (floor xoff) (floor yoff)
	  (floor (+ xoff x))
	  (floor (+ yoff y)))))
       
(alter 'groups.7 289 252)
(108 115 397 367) 

(126 108 414 360) 
(alter 'analogy.25 3.1 8.88)
(126 108 349 747) 
(alter 'search3.8 4.7 5.9)
(126 108 464 532) 
(alter 'search3.14 4.7 5.9)
(126 108 464 532) 
(alter 'contest.20 3.6 7.7)
(126 108 385 662) 
(alter 'numbers.25 3.75 1.75)
(126 108 396 234) 
(alter 'throw.1 2.0 3.1)
(126 108 270 331) 
(mapcar #'(lambda (x) (- x 72)) (alter 'train.16 6.7 2.7))
(54 36 536 230) 

