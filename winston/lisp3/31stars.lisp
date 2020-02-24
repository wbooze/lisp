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

;;; POINTS

(defstruct star (name) (x) (y) (magnitude))
(defstruct spot (number) (u) (v) (brightness))

(defun similar-magnitude (star spot)            ;Similar magnitude?
  (let ((star-m (star-magnitude star))          ;Get star's magnitude.
        (spot-m (spot-brightness spot)))        ;Get spot's brightness.
    (< (abs (- star-m spot-m)) 1.0)))           ;Within tolerance?

(defun distance-stars (a b)                    ;Distance between stars.
  (distance (star-x a) (star-y a) (star-x b) (star-y b)))

(defun distance-spots (a b)                    ;Distance between spots.
  (distance (spot-u a) (spot-v a) (spot-u b) (spot-v b)))

(defun distance (xs ys xf yf)                  ;Euclidean distance.
  (let ((dx (- xf xs)) (dy (- yf ys)))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun similar-distances (pair-a pair-b)          ;Distances similar?
  (let* ((star-a (first pair-a)) (spot-a (second pair-a))
         (star-b (first pair-b)) (spot-b (second pair-b))
         (dstars (distance-stars star-a star-b))  ;Between stars.
         (dspots (distance-spots spot-a spot-b))) ;Between spots.
    (< (abs (- dstars dspots)) 0.1)))             ;Within tolerance?

(defun distances-check (new pairs)              ;Check out new pair,
  (dolist (pair pairs t)                        ; against other pairs.
    (unless (similar-distances new pair) (return nil))))

(defstruct transform (c) (s) (xo) (yo))

(defun print-transform (trans)
  (let ((c (transform-c trans)) (s (transform-s trans))
        (xo (transform-xo trans)) (yo (transform-yo trans)))
    (format t "~%c = ~a s = ~a xo = ~a yo = ~a" c s xo yo)))

(defun map-uv-to-xy (u v trans)             ;From image to model.
  (let ((c (transform-c trans)) (s (transform-s trans))
        (xo (transform-xo trans)) (yo (transform-yo trans)))
    (values (+ (- (* c u) (* s v)) xo)
            (+ (+ (* s u) (* c v)) yo))))

(defun find-transform (pairs)            ;Get transformation.
  (if (< (length pairs) 2)               ;Need two pairs.
      (error "Too few pairs for transformation")
      (find-transform-sub (first pairs) (second pairs))))

(defun find-transform-sub (pair-a pair-b)            ;Minimal version.
  (let* ((star-a (first pair-a)) (spot-a (second pair-a))
         (star-b (first pair-b)) (spot-b (second pair-b))
         (xa (star-x star-a)) (ya (star-y star-a))
         (xb (star-x star-b)) (yb (star-y star-b))
         (ua (spot-u spot-a)) (va (spot-v spot-a))
         (ub (spot-u spot-b)) (vb (spot-v spot-b)))
    (find-transform-aux xa ya xb yb ua va ub vb)))

(defun find-transform-aux (xa ya xb yb ua va ub vb)
  (let ((dx (- xb xa)) (dy (- yb ya))               ;Vector ($dx$, $dy$).
        (du (- ub ua)) (dv (- vb va))               ;Vector ($du$, $dv$).
        (xm (/ (+ xb xa) 2)) (ym (/ (+ yb ya) 2))   ;Centroid $x$ \& $y$.
        (um (/ (+ ub ua) 2)) (vm (/ (+ vb va) 2)))  ;Centroid $u$ \& $v$.
    (multiple-value-bind (c s) (dot-and-cross dx dy du dv)
      (let ((xo (- xm (- (* c um) (* s vm))))
            (yo (- ym (+ (* s um) (* c vm)))))
        (make-transform :c c :s s :xo xo :yo yo)))))

(defun dot-and-cross (dx dy du dv)
  (let ((kc (+ (* dx du) (* dy dv)))            ;Dot-product.
        (ks (- (* dy du) (* dx dv))))           ;Cross-product.
    (normalize-c-s kc ks)))

(defun normalize-c-s (kc ks)                ;Normalize two values
  (if (and (zerop kc) (zerop ks))           ; that are proportional
      (error "K C = 0 and K S = 0")
      (let ((k (sqrt (+ (* kc kc) (* ks ks)))))
        (values (/ kc k) (/ ks k)))))       ; to cosine and sine.

(defun check-matched-pairs (pairs trans)       ;Do pairs really match?
  (dolist (pair pairs t)
    (let* ((star (first pair)) (spot (second pair))
           (x (star-x star)) (y (star-y star))
           (u (spot-u spot)) (v (spot-v spot)))
      (multiple-value-bind (xs ys) (map-uv-to-xy u v trans)
        (unless (point-match x y xs ys) (return nil))))))

(defun point-match (x y xs ys)                 ;Similar coordinates?
  (and (< (abs (- xs x)) 0.1) (< (abs (- ys y)) 0.1)))

(defun print-pairs (pairs)                      ;Third version.
  (format t "~%")
  (dolist (pair pairs)
    (let ((star (first pair)) (spot (second pair)))
      (format t "(~a = ~a) " (star-name star) (spot-number spot)))))

(defun print-find-match (stars spots)           ;Find match
  (let ((result (find-match stars spots)))      ; and print it.
    (when result
      (let ((pairs (first result)) (trans (second result)))
        (print-pairs pairs)
        (print-transform trans)))))

(defun find-match-check (pairs)             ;Obtain transformation,
  (unless (< (length pairs) 2)              ; and check it.
    (let ((trans (find-transform pairs)))
      (when (check-matched-pairs pairs trans)  ;\sy{NIL} if no go,
        (list pairs trans)))))                 ; pairs \& transform.

(defun find-match (stars spots)            ;Find spots to match stars.
  (find-match-sub nil stars spots 0))      ;Start---no wild-cards.

(defstruct constellation (name) (stars))

(defun recognize-constellation (spots constellations)
  (dolist (constellation constellations)
    (let* ((stars (constellation-stars constellation))
           (ans (find-match stars spots)))
      (when ans 
        (let ((trans (second ans)))
          (return (list constellation trans)))))))

(defun find-match-sub (pairs stars spots k)     ;Sixth version.
  (if (endp stars)                              ;More stars to match?
      (find-match-check (reverse pairs))        ;No, check the match.
      (unless (> k 3)                           ;Too many wild-cards?
        (let ((star (first stars))              ;First star in list.
              (best nil)                        ;Best match so far.
              (m-b 0))                          ;Pairs in best match.
          (dolist (spot spots)                  ;Try each spot in turn.
            (when (similar-magnitude star spot) ;Same brightness?
              (let ((new (list star spot)))     ;New pair to check.
                (when (distances-check new pairs)   ;Compatible?
                  (let* ((ans (find-match-sub (cons new pairs)
                                              (rest stars)
                                              (remove spot spots)
                                              k))
                         (m (length (first ans)))) ;How many pairs?
                    (when (> m m-b) (setf m-b m best ans)))))))
          (let* ((ans (find-match-sub pairs      ;Ignore one star.
                                      (rest stars)
                                      spots 
                                      (+ k 1)))
                 (m (length (first ans))))     ;How many pairs?
              (when (> m m-b) (setf m-b m best ans)))
          best))))                              ;Return the best.

;;; EDGES

(defstruct line (xs) (ys) (xf) (yf))
(defstruct edge (us) (vs) (uf) (vf))

(defun line-length (l)                       ;Length of line.
  (distance (line-xs l) (line-ys l) (line-xf l) (line-yf l)))

(defun edge-length (e)                       ;Length of edge.
  (distance (edge-us e) (edge-vs e) (edge-uf e) (edge-vf e)))

(defun similar-length (line edge)            ;Similar length?
  (let ((dl (line-length line)) (el (edge-length edge)))
    (< (abs (- dl el)) 0.1)))

(defun angle-between-lines (line-a line-b)
  (let ((xs-a (line-xs line-a)) (ys-a (line-ys line-a))
        (xf-a (line-xf line-a)) (yf-a (line-yf line-a))
        (xs-b (line-xs line-b)) (ys-b (line-ys line-b))
        (xf-b (line-xf line-b)) (yf-b (line-yf line-b)))
    (angle-between-sub xs-a ys-a xf-a yf-a xs-b ys-b xf-b yf-b)))

(defun angle-between-edges (edge-a edge-b)
  (let ((us-a (edge-us edge-a)) (vs-a (edge-vs edge-a))
        (uf-a (edge-uf edge-a)) (vf-a (edge-vf edge-a))
        (us-b (edge-us edge-b)) (vs-b (edge-vs edge-b))
        (uf-b (edge-uf edge-b)) (vf-b (edge-vf edge-b)))
    (angle-between-sub us-a vs-a uf-a vf-a us-b vs-b uf-b vf-b)))

(defun angle-between-sub (xs-a ys-a xf-a yf-a xs-b ys-b xf-b yf-b)
  (let ((dx-a (- xf-a xs-a)) (dy-a (- yf-a ys-a))
        (dx-b (- xf-b xs-b)) (dy-b (- yf-b ys-b)))
    (dot-and-cross dx-a dy-a dx-b dy-b)))

(defun similar-angles (pair-a pair-b)
  (let ((line-a (first pair-a)) (edge-a (second pair-a))
        (line-b (first pair-b)) (edge-b (second pair-b)))
    (multiple-value-bind (c-l s-l)
        (angle-between-lines line-a line-b)
      (multiple-value-bind (c-e s-e)
          (angle-between-edges edge-a edge-b)
        (< (abs (- (* c-l s-e) (* c-e s-l))) 0.1)))))

(defun angles-check (new pairs)                 ;Check out new pair,
  (dolist (pair pairs t)                        ; against other pairs.
    (unless (similar-angles new pair) (return nil))))

(defun find-edge-transform (pairs)           ;Get edge transformation,
  (if (null pairs)                           ; using single pair.
      (error "Not enough pairs for transformation")
      (find-edge-transform-sub (first pairs))))

(defun find-edge-transform-sub (pair)        ;Minimal version.
  (let* ((line (first pair)) (edge (second pair))
         (xs (line-xs line)) (ys (line-ys line))
         (xf (line-xf line)) (yf (line-yf line))
         (us (edge-us edge)) (vs (edge-vs edge))
         (uf (edge-uf edge)) (vf (edge-vf edge)))
    (find-transform-aux xs ys xf yf us vs uf vf)))

(defun find-edge-check (pairs)                 ;Obtain transformation,
  (unless (endp pairs)                         ; and check it.
    (let ((trans (find-edge-transform pairs))) ;Transformation.
      (when (check-edge-pairs pairs trans)     ;\sy{NIL} if no go,
        (list pairs trans)))))                 ; pairs \& transform.

(defun check-edge-pairs (pairs trans)           ;Do pairs really match?
  (dolist (pair pairs t)
    (let* ((line (first pair)) (edge (second pair))
           (xs (line-xs line)) (ys (line-ys line))
           (xf (line-xf line)) (yf (line-yf line))
           (us (edge-us edge)) (vs (edge-vs edge))
           (uf (edge-uf edge)) (vf (edge-vf edge)))
      (multiple-value-bind (xsd ysd) (map-uv-to-xy us vs trans)
        (multiple-value-bind (xfd yfd) (map-uv-to-xy uf vf trans)
          (unless (and (point-match xs ys xsd ysd)
                       (point-match xf yf xfd yfd))
            (return nil)))))))

(defun find-edge-match (lines edges)         ;Find edges to match lines.
  (find-edge-match-sub nil lines edges 0)    ;Start---no wild-cards.
  (if (endp lines)                              ;More lines to match?
      (find-match-check (reverse pairs))        ;No, check the match.
      (unless (> k 3)                           ;Too many wild-cards?
        (let ((line (first lines))              ;First line in list.
              (best nil)                        ;Best match so far.
              (m-b 0))                          ;Pairs in best match.
          (dolist (edge edges)                  ;Try each edge in turn.
            (when (similar-length line edge)    ;Similar length?
              (let ((new (list line edge)))     ;New pair to check.
                (when (angles-check new pairs)  ;Angles compatible?
                  (let* ((ans (find-edge-match-sub
                                (cons new pairs)
                                (rest lines)
                                (remove edge edges)
                                k))
                         (m (length (first ans)))) ;How many pairs?
                    (when (> m m-b) (setf m-b m best ans)))))))
          (let* ((ans (find-match-sub pairs      ;Ignore one edge. 
                                      (rest lines)
                                      edges 
                                      (+ k 1)))
                 (m (length (first ans))))     ;How many pairs?
            (when (> m m-b) (setf m-b m best ans)))
          best))))                              ;Return the best.

(defstruct object (name) (lines))

(defun recognize-object (edges objects)
  (dolist (object objects)
    (let* ((lines (object-lines object))
           (ans (find-match lines edges)))
      (when ans 
        (let ((trans (second ans)))
          (return (list object trans)))))))
