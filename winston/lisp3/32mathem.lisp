;;;; -*- mode:Lisp; package:user -*- ;;;;
;;;; Created: 10 December 1992
;;;; Copyright 1992 Patrick H. Winston and Berthold K. P. Horn.
;;;; All rights reserved.
;;;;
;;;; Version 1.0.1, copied from master file on 23 Apr 93       
;;;; 
;;;; This software is licensed by Patrick H. Winston and Berthold K. P. Horn
;;;; (licensors) for instructional use with the textbooks ``Lisp,'' by Patrick
;;;; H. Winston and Berthold K. P. Horn, and ``Artificial Intelligence,'' by
;;;; Patrick H. Winston.  Your are free to make copies of this software and
;;;; modify it for such instructional use as long as:
;;;; 1. You keep this notice intact.
;;;; 2. You cause any modified files to carry a prominent notice stating
;;;;    that you modified the files and the date of your modifications.
;;;; This software is licensed ``AS IS'' without warranty and the licensor
;;;; shall have no liability for any alleged defect or damages.

;;;; PROCEDURES

;;; INFIX TO PREFIX

(defun weight (operator)         ;Determine weight of operator.
  (case operator
    (= 0)
    (+ 1)
    (- 1)
    (* 2)
    (/ 2)
    (\\\\ 2)
    (\carret 3)
    (t 9)))                      ;Unrecognized operator.

(defun opcode (operator)           ;Get appropriate primitive
  (case operator
    (= 'setf)
    (+ '+)
    (- '-)
    (* '*)
    (/ '/ )
    (\\\\ 'rem)
    (\carret 'expt)
    (t operator)))                 ;Unrecognized operator.

(defun inf-to-pre (ae)
  (if (atom ae) ae                          ;Check for easy case.
      (inf-aux ae nil nil)))                ;Start with empty stacks.

(defun inf-aux (ae operators operands)
  (inf-iter (rest ae)                           ;Work on rest after
            operators
            (cons (inf-to-pre (first ae))       ; recursing on first.
                  operands)))                  

(defun inf-iter (ae operators operands)
  (cond ((and (endp ae) (endp operators))       ;Termination?
         (first operands))                      ;Result.
        ((and (not (endp ae))                   ;Not end of \sy{AE}?
              (or (endp operators)              ;Empty stack?
                  (> (weight (first ae))        ;Compare weights.
                     (weight (first operators)))))
         (inf-aux (rest ae)
                  (cons (first ae) operators)   ;Push operator
                  operands))                    ; and continue.
        (t (inf-iter ae
                     (rest operators)                  ;Pop operator,
                     (cons (list (opcode (first operators))
                                 (second operands)     ; construct
                                 (first operands))     ; result,
                           (rest (rest operands))))))) ; pop operands.

;;; SPARSE MATRICES

(defun sparse-scale-v (scale v)
  (if (zerop scale) nil                           ;Special case.
      (if (endp v) nil                            ;Termination?
          (let ((fv (first v)))                   ;Grab first entry.
            (cons (list (first fv)                ;Copy index.
                        (* scale (second fv)))    ;Scale component.
                  (sparse-scale-v scale (rest v)))))))    ;Recurse.

(defun sparse-dot-product (a b)
  (if (or (endp a) (endp b)) 0                    ;Termination?
      (let ((fa (first a)) (fb (first b)))        ;Grab components.
        (cond ((< (first fa) (first fb))
               (sparse-dot-product (rest a) b))   ;Shorten \sy{A}.
              ((< (first fb) (first fa))
               (sparse-dot-product a (rest b)))   ;Shorten \sy{B}.
              (t (+ (* (second fa) (second fb))   ;Multiply.
                    (sparse-dot-product (rest a)
                                        (rest b))))))))    ;Recurse.

(defun sparse-v-plus (a b)
  (cond ((endp a) b)                              ;Termination?
        ((endp b) a)                              ;Termination?
        (t (let ((fa (first a)) (fb (first b)))   ;Grab components.
             (cond ((< (first fa) (first fb))
                    (cons fa                      ;Copy component.
                          (sparse-v-plus (rest a) b)))
                   ((< (first fb) (first fa))
                    (cons fb                      ;Copy component.
                          (sparse-v-plus a (rest b))))
                   (t (cons (list (first fa)      ;Copy index.
                                  (+ (second fa)
                                     (second fb)))      ;Add.
                            (sparse-v-plus (rest a) 
                                           (rest b))))))))) ;Recurse.

(defun sparse-m-times-v (m v)
  (if (or (endp v) (endp m)) nil                  ;Termination?
      (let ((fv (first v)) (fm (first m)))        ;Grab components.
        (cond ((< (first fv) (first fm))
               (sparse-m-times-v m (rest v)))     ;Shorten \sy{V}.
              ((< (first fm) (first fv))
               (sparse-m-times-v (rest m) v))     ;Shorten \sy{M}.
              (t (sparse-v-plus
                   (sparse-scale-v (second fv) (second fm))
                   (sparse-m-times-v (rest m) (rest v)))))))) 

(defun sparse-m-times (ma mb)
  (if (endp mb) nil                             ;Termination?
      (let ((fb (first mb)))                    ;Grab first component.
        (cons (list (first fb)                  ;Copy index.
                    (sparse-m-times-v ma (second fb)))
              (sparse-m-times ma (rest mb)))))) ;Recurse.

;;;; ROOTS

(defun linear (a b)
  (if (zerop a)
      (if (zerop b)
          (error "Eq. homogeneous  ~a*x+~a=0" a b)   ;a $=$ 0 & b $=$ 0
          (error "Eq. inconsistent ~a*x+~a=0" a b))  ;a $=$ 0 & b $\ne$ 0
      (if (zerop b)
          (list 0)                             ;a $\neq$ 0 & b $=$ 0.
          (list (/ (- b) a)))))                ;a $\neq$ 0 & b $\neq$ 0.

(defun quadratic (a b c)
  (cond ((minusp a) (quadratic (- a) (- b) (- c)))
        ((zerop a) (linear b c))                ;a = 0.
        ((zerop c) (cons 0 (linear a b)))       ;c = 0.
        (t (quadratic-aux a b c (- (* b b) (* 4 a c))))))

(defun check-quadratic (a b c)
  (dolist (x (quadratic a b c))
    (format t "~%For x = ~a, the quadratic yields ~a."
            x
            (+ (* (+ (* a x) b) x) c)))
  (values))

(defun quadratic-aux (a b c discriminant)
  (cond ((minusp discriminant)                  ;Conjugate pair. 
         (quadratic-conjugate (/ (- b) (* 2 a))
                              (/ (sqrt (- discriminant)) (* 2 a))))
        ((zerop discriminant)                   ;Double root. 
         (quadratic-equal (/ (- b) (* 2 a))))
        ((minusp b)                             ;Real roots b $<$ 0.
         (quadratic-real-p a
                           (- (sqrt discriminant) b)
                           c))
        (t (quadratic-real-m a                  ;Real roots b $\geq$ 0.
                             (- (+ (sqrt discriminant) b))
                             c))))

(defun quadratic-equal (x) (list x x))

(defun quadratic-conjugate (real imaginary)
  (list (complex real imaginary)
        (complex real (- imaginary))))

(defun quadratic-real-p (a rat c)       ;Two real roots & b $<$ 0.
  (list (/ rat (* 2 a))                 ;If a $>$ 0, most positive first.
        (/ (* 2 c) rat)))

(defun quadratic-real-m (a rat c)       ;Two real roots & b $>$ 0.
  (list (/ (* 2 c) rat)                 ;If a $>$ 0, most positive first.
        (/ rat (* 2 a))))

(defun cubic (a b c d)
  (cond ((minusp a) (cubic (- a) (- b) (- c) (- d)))
        ((zerop a) (quadratic b c d))               ;a = 0.
        ((zerop d) (cons 0 (quadratic a b c)))      ;d = 0.
        (t (cubic-aux a
                      b
                      (quadratic 1                  ;Resolvent.
                                 (+ (* 2 b b b)
                                    (* 9 a (- (* 3 a d) (* b c))))
                                 (expt (- (* b b) (* 3 a c))
                                       3))))))

(defun check-cubic (a b c d)
  (dolist (x (cubic a b c d))
    (format t "~%For x = ~a, the cubic yields ~a."
            x
            (+ (* (+ (* (+ (* a x) b) x) c) x) d)))
  (values))

(defun cube-root (y)
  (if (zerop y) 0
      (if (minusp y) (- (cube-root (- y)))
          (cube-root-iter y (expt y 1/3)))))

(defun cube-root-iter (y x)  (/ (+ x x (/ y (* x x))) 3))

(defun cubic-aux (a b roots)
  (if (complexp (first roots))               ;Check resolvent roots.
      (cubic-real a                          ;Roots complex.
                  b 
                  (abs (first roots))        ;Modulus, and
                  (phase (first roots)))     ; argument.
      (cubic-conjugate a                     ;Resolvent roots real.
                       b
                       (cube-root (first roots))     ;Pick out the
                       (cube-root (second roots))))) ; two roots.

(defun cubic-conjugate (a b r s)                  ;r & s are cube roots.
  (let ((sroot (/ (- (+ r s) b) (* a 3))) 
        (real (/ (- (- (/ (+ r s) 2)) b) (* a 3))))
    (if (= r s) 
        (cubic-conjugate-equal sroot real)           ;Roots equal.
        (let ((imag (/ (* (- r s) (/ (sqrt 3) 2)) (* a 3))))
          (cubic-conjugate-aux sroot real imag)))))  ;Roots not equal.

(defun cubic-conjugate-equal (sroot droot)        ;Two of roots equal.
  (cons sroot (list droot droot)))

(defun cubic-conjugate-aux (real-root real imag)
  (list real-root                                 ;Real root first,
        (complex real imag)                       ; then comes the
        (complex real (- imag))))                 ; conjugate pair.

(defun cubic-real (a b rho theta)      ;Rho & theta of complex root.
  (cubic-real-aux a
                  b
                  (* 2 (cube-root rho))
                  (/ (cos (/ theta 3)) -2)
                  (/ (* (sin (/ theta 3)) (sqrt 3)) 2)))

(defun cubic-real-aux (a b rd cd sd)        ;Most positive root first.
  (list (/ (- (* -2 rd cd) b) (* 3 a))
        (/ (- (* rd (+ cd sd)) b) (* 3 a)) 
        (/ (- (* rd (- cd sd)) b) (* 3 a))))

(defun quartic (a b c d e)
  (cond ((minusp a) (quartic (- a) (- b) (- c) (- d) (- e)))
        ((zerop a) (cubic b c d e))             ;a = 0.
        ((zerop e) (cons 0 (cubic a b c d)))    ;e = 0.
        (t (quartic-aux a
                        b
                        c
                        d
                        e
                        (first (cubic 1         ;Resolvent cubic.
                                      (- c)
                                      (- (* b d) (* 4 a e))
                                      (- (* 4 a c e)
                                         (+ (* a d d) 
                                            (* b b e)))))))))

(defun quartic-aux (a b c d e s)              ;s is root of resolvent. 
  (quartic-split a
                 b
                 (sqrt (- (* b b) (* 4 a (- c s))))
                 s
                 (sqrt (- (* s s) (* 4 a e)))
                 (- (* b s) (* 2 a d))))

(defun quartic-split (a b r1 s r2 bs-2ad)              
  (if (minusp (* r1 r2 bs-2ad))          ; sign of r1 r2 same as bs-2ad?
      (append (quadratic (* 2 a) (- b r1) (+ s r2))     ;No.
              (quadratic (* 2 a) (+ b r1) (- s r2)))
      (append (quadratic (* 2 a) (- b r1) (- s r2))     ;Yes.
              (quadratic (* 2 a) (+ b r1) (+ s r2)))))

(defun check-quartic (a b c d e)
  (dolist (x (quartic a b c d e))
    (format t "~%For x = ~a, the quartic yields ~a."
            x
            (+ (* (+ (* (+ (* (+ (* a x) b) x) c) x) d) x) e)))
  (values))


