(defmacro fcons (first list)
  (let ((f-var (gensym)))
    `(let (,f-var)
       (lambda ()
  (if (null ,f-var)
      (progn (setf ,f-var ,list)
      ,first)
      (funcall ,f-var))))))

(defun zip (l1 l2)
  (lambda ()
    (list (funcall l1) (funcall l2))))

(defun fmap (f iter)
  (lambda ()
    (funcall f (funcall iter))))

(defun ftail (list)
  (funcall list)
  list)

(defun take (n list &optional (collect nil))
  (eval `(loop repeat ,n
        ,(if collect 'collect 'do) (funcall ,list))))

(defun fib ()
  (fcons 1
     (fcons 1
         (fmap (lambda (x)
                 (+ (car x) (cadr x)))
               (zip (fib) (ftail (fib)))))))

(defun fib-gen ()
  (let ((q (cons 1 1)))
    (lambda ()
      (let ((f (car q))
            (s (cdr q)))
        (rplaca q s)
        (rplacd q (+ f s))
        f))))

(defparameter a (fib-gen))
(defparameter b (fib))

;;(take 100 (fmap #'print a))
;; (take 100 a t)
