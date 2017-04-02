(defun maplist-nreverse (f list)
  (do ((sub list (cdr sub))
       (r nil (cons (funcall f sub) r)))
    ((null sub) (nreverse r))))


(defun maplist-rplacd (f list)
  (let ((r (cons nil nil)))
    (do ((sub list (cdr sub))
         (end r (let ((x (cons
                          (funcall f sub)
                          nil)))
                  (rplacd end x)
                  x)))
      ((null sub) (cdr r)))))

(defun my-nreverse (list)
  (prog ((prev nil) next)
    (when (null list) (return nil))
    lp (setq next (cdr list))
    (rplacd list prev)
    (when (not next) (return list))
    (setq prev list)
    (setq list next)
    (go lp)))

(defun nreverse-unrolled (list)
  (prog ((prev nil) next)
    (when (null list) (return nil))
    lp (setq next (cdr list))
    (rplacd list prev)
    (when (not next) (return list))
    (setq prev (cdr next))
    (rplacd next list)
    (when (not prev) (return next))
    (setq list (cdr prev))
    (rplacd prev next)
    (when (not list) (return prev))
    (go lp)))

(defun maplist-loop (f list)
  (loop for sub on list
        collect (funcall f sub)))

(defun maplist-series (f list)
  (series:collect
   (series:map-fn t f (series:scan-sublists list))))

(do ((i 1 (1+ i))
     (accum 0 accum)
     (result nil result))
  ((> i 10) (nreverse result))
  (setf result (push (+ accum i) result)))


(let ((answer))
  (dolist (y '(1 2 3 4) (nreverse answer))
    (setf answer
            (push
             (values
              (mapcar (lambda (x) (expt x y))
                      (do ((i 1 (1+ i))
                           (accum 0 accum)
                           (result nil result))
                          ((> i 10) (nreverse result))
                        (setf result (push (+ accum i) result)))))
             answer))))


(let ((answer))
  (dolist
      (y
       (do ((i 1 (1+ i))
            (accum 0 accum)
            (result nil result))
           ((> i 10) (nreverse result))
         (setf result (push (+ accum i) result)))
       (nreverse answer))
    (setf answer
            (push
             (values
              (mapcar (lambda (x) (expt x y))
                      (do ((i 1 (1+ i))
                           (accum 0 accum)
                           (result nil result))
                          ((> i 10) (nreverse result))
                        (setf result (push (+ accum i) result)))))
             answer))))
