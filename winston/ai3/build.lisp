;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; Fisher's Exact Test

;;Redo following!

(defun choose (x y &aux n (d 1.0))
  "From set x, choose y things, independant of order."
  ;;x!/((x - y)! * y! = (x * (x-1) *...*(x-y+1) / y!)
  ;;Done floating and oddly to avoid overflow!
  (setf n (dotimes (m y (/ d)) (setf d (* d (1+ m)))))
  (dotimes (m y n) (setf n (* n (- x m)))))

(defun analyze (l sr1 sp1 sp2 threshold)
  (distribution sr1 sp1 sp2)
  (limits l sr1 sp1 sp2 threshold))

(defun limits (l sr1 sp1 sp2 threshold)
  (let* ((n (- sr1 l)) lower upper)
    (format t "~%[~a, ~a]"
	    (do ((count (max 0 (- sr1 sp2)) (1+ count)) (P 0))
		((> P threshold) (setf lower (1- count)))
	      (incf P (pl count sp1 sp2 (+ l n))))
	    (do ((count (min sr1 sp1) (1- count)) (P 0))
		((> P threshold) (setf upper (1+ count)))
	      (incf P (pl count sp1 sp2 (+ l n)))))
    (if (<= lower l upper)
	(format t "~%Arrangement suggests independance.")
      (format t "~%Arrangement suggests dependance.")))
  (values))

(defun pl (l sp1 sp2 sr1)
  (/ (* (choose sp1 l) (choose sp2 (- sr1 l)))
     (choose (+ sp1 sp2) sr1)))

(defun distribution (sr1 sp1 sp2)
  (do ((l (max 0 (- sr1 sp2)) (1+ l)))
      ((> l (min sr1 sp1)) (values))
    (format t "~%~a	~a	~a	~a	-->	~a"
	    l (- sp1 l)
	    (- sr1 l) (- sp2 (- sr1 l))
	    (patch (pl l sp1 sp2 sr1)))))

(defun patch (n &aux (string "."))
  (cond ((>= n 1.0)
	 (string-append (format nil "~a" (floor n))
			(patch (- n (floor n)))))
	((= n 0.0) ".0")
	(t (loop (when (> (setf n (* n 10)) 1.0)
		   (return (format nil "~a~a" string (truncate (* n 100)))))
		 (setf string (string-append string "0"))))))

#|

;|

(distribution 10 10 10)
0	10	10	0	-->	.00000541
1	9	9	1	-->	.000541
2	8	8	2	-->	.0109
3	7	7	3	-->	.0779
4	6	6	4	-->	.238
5	5	5	5	-->	.343
6	4	4	6	-->	.238
7	3	3	7	-->	.0779
8	2	2	8	-->	.0109
9	1	1	9	-->	.000541
10	0	0	10	-->	.00000541

(distribution 10 10 20)
0	10	10	10	-->	.00614
1	9	9	11	-->	.0559
2	8	8	12	-->	.188
3	7	7	13	-->	.309
4	6	6	14	-->	.270
5	5	5	15	-->	.130
6	4	4	16	-->	.0338
7	3	3	17	-->	.00455
8	2	2	18	-->	.000284
9	1	1	19	-->	.00000665
10	0	0	20	-->	.0000000332

(distribution 10 10 40)
0	10	10	30	-->	.0825
1	9	9	31	-->	.266
2	8	8	32	-->	.336
3	7	7	33	-->	.217
4	6	6	34	-->	.0784
5	5	5	35	-->	.0161
6	4	4	36	-->	.00186
7	3	3	37	-->	.000115
8	2	2	38	-->	.00000341
9	1	1	39	-->	.0000000389
10	0	0	40	-->	.0000000000973

(distribution 2 2 2)
0	2	2	0	-->	.166
1	1	1	1	-->	.666
2	0	0	2	-->	.166

(distribution 4 4 4)
0	4	4	0	-->	.0142
1	3	3	1	-->	.228
2	2	2	2	-->	.514
3	1	1	3	-->	.228
4	0	0	4	-->	.0142

(distribution 10 10 10)
0	10	10	0	-->	.00000541
1	9	9	1	-->	.000541
2	8	8	2	-->	.0109
3	7	7	3	-->	.0779
4	6	6	4	-->	.238
5	5	5	5	-->	.343
6	4	4	6	-->	.238
7	3	3	7	-->	.0779
8	2	2	8	-->	.0109
9	1	1	9	-->	.000541
10	0	0	10	-->	.00000541


|#