;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; REMARKS

#|

The segments are laid out like this:

  *-2-*
  :   :
  1   3
  :   :
  *-0-*
  :   :
  6   4
  :   :
  *-5-*

|#

;;;; SAMPLE DATA

(setf *samples*
      '((0 0 1 1 1 1 1 1 1)
	(9 1 1 1 1 1 1 0 1)
	(8 1 1 1 1 1 1 1 1)
	(7 0 0 1 1 1 0 0 1)
	(6 1 1 1 0 1 1 1 1)
	(5 1 1 1 0 1 1 0 1)
	(4 1 1 0 1 1 0 0 1)
	(3 1 0 1 1 1 1 0 1)
	(2 1 0 1 1 0 1 1 1)
	(1 0 0 0 1 1 0 0 1)))





