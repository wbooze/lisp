;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; REMARK

#|

Test data looks like this:

6	*	Red	*	*	Yellow	*
5	*	Orange	*	*	*	Purple
4	U1	*	*	U2	*	*
3	*	*	*	*	*	*
2	Red	*	*	Blue	*	*
1	*	Violet	*	*	*	Green
0	1	2	3	4	5	6

|#

;;;; TEST DATA

(setf samples '((b1 red		1 2)
		(b2 violet	2 1)
		(b3 blue	4 2)
		(b4 green	6 1)
		(b5 red		2 6)
		(b6 yellow	5 6)
		(b7 orange	2 5)
		(b8 purple	6 5)))

(make-record-for-unknown u1	1 4)
(make-record-for-unknown u2	4 4)
(make-record-for-unknown u3	1 6)
(make-record-for-unknown u4	6 1)
(make-record-for-unknown u5	1 1)
(make-record-for-unknown u6	6 6)
