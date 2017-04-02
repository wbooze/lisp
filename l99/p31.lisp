#-(and) "

P31 (**) Determine whether a given integer number is prime.
    Example:
    * (is-prime 7)
    T
"

(defun is-prime (n)
  (primep n))


(defun primep (n)
  (cond
    ((minusp n)          (primep (- n)))
    ((= 1    n)          nil)
    ((member n '(2 3 5 7)) t) ; primes up to 3²=9
    ((evenp  n)          nil)
    (t
     (loop
        :with root     = (isqrt n)
        :with divisors = (loop :for i :from 3 :to root :by 2 :collect i)
        :for d = (pop divisors)
        :if (zerop (mod n d))
        :do (return nil)
        :else :do (setf divisors (delete-if (lambda (x) (zerop (mod x d))) divisors))
        :while divisors
        :finally (return t)))))

;;;; THE END ;;;;
