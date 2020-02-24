;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; REMARKS

#|

City distances differ slightly from those reported in the
text---neither set is likely to be particularly accurate.

|#

;;;; TEST DATA FOR SEARCH PROCEDURES

(defun make-text-cities ()
  (defplace s (a d)	(-.89 2.75))
  (defplace a (s b d)	(1.0 5.1))
  (defplace d (s a e)	(2.0 0.0))
  (defplace e (b d f)	(4.0 0.0))
  (defplace b (a c e)	(5.0 5.1))
  (defplace f (e g)	(8.0 0.0))
  (defplace c (b)	(9.0 5.1))
  (defplace g (f)	(9.89 2.35)))

