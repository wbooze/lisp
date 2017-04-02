(defun insertr (new old lat)
  (cond 
    ((null lat) 'nil) 
    ((eq (car lat) old) (cons new (cdr lat)))
    (t (cons (car lat) (insertr new old (cdr lat))))))
