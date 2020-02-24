;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;
;;;; Created: 21 November 1990
;;;; Purpose: Support for discussion of rete procedure

;;; Get missing database functions:

(unless (fboundp 'db-call)
  (load "/gclisp2/explorer/slides/ch30.lisp"))

;;; Create data

(db-make-relation data
  (first	second		third)
  (dd		is-a		horse)
  (sugar	is-a		horse)
  (dd		is-a-parent-of	brassy)
  (dd		is-a-parent-of	sugar)
  (sugar	is		fast)
  (brassy	is-a-parent-of	tacky)
  (tacky	is		fast)
  (tacky	is-a		horse)
  (brassy	is-a		horse))

#|

Rule

IDENTIFY1
IF   ((? x) is-a horse)
     ((? x) is-a-parent-of (? y))
     ((? y) is fast)
THEN ((? x) is valuable)

|#

(db-show data)
| FIRST  | SECOND         | THIRD  |
| ------ | -------------- | ------ |
| DD     | IS-A           | HORSE  |
| SUGAR  | IS-A           | HORSE  |
| DD     | IS-A-PARENT-OF | BRASSY |
| DD     | IS-A-PARENT-OF | SUGAR  |
| SUGAR  | IS             | FAST   |
| BRASSY | IS-A-PARENT-OF | TACKY  |
| TACKY  | IS             | FAST   |
| TACKY  | IS-A           | HORSE  |
| BRASSY | IS-A           | HORSE  |


(db-show (setf a1 (db-project
		    (db-select data with second eq is-a and third eq horse)
		    over first)))
| FIRST  |
| ------ |
| DD     |
| SUGAR  |
| TACKY  |
| BRASSY |


(db-show (setf a2 (db-project
		    (db-select data with second eq is-a-parent-of)
		    over first and third)))
| FIRST  | THIRD  |
| ------ | ------ |
| DD     | BRASSY |
| DD     | SUGAR  |
| BRASSY | TACKY  |

|
(db-show (setf a3 (db-project (db-select data with third eq fast)
			      over first)))
| FIRST |
| ----- |
| SUGAR |
| TACKY |


(db-show (setf b1 (db-project (db-join a1 a2 with first eq first)
			      over first and third)))
| FIRST  | THIRD  |
| ------ | ------ |
| DD     | BRASSY |
| DD     | SUGAR  |
| BRASSY | TACKY  |

(db-show (setf b2 (db-project (db-join a3 b1 with first eq third)
			      over first and third)))
| FIRST | THIRD |
| ----- | ----- |
| SUGAR | SUGAR |
| TACKY | TACKY |

(db-show (setf c1 (db-project b2 over first)))
| FIRST |
| ----- |
| SUGAR |
| TACKY |
