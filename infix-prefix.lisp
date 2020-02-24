(defun weight (operator)         ;Determine weight of operator.
  (case operator
    (= 0)
    (+ 1)
    (- 1)
    (* 2)
    (/ 2)
    (\\ 2)
    (\^ 3)
    (t 9)))                      ;Unrecognized operator.

(defun opcode (operator)           ;Get appropriate primitive
  (case operator
    (= 'setf)
    (+ '+)
    (- '-)
    (* '*)
    (/ '/ )
    (\\ 'rem)
    (\^ 'expt)
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


(defun pre-to-inf (l)
  (cond ((null l) nil)
        ((atom l) l)
        (t (list (pre-to-inf (second l))
                 (opsymbol (first l))
                 (pre-to-inf (third l))))))

(defun opsymbol (x)
  (case x
    (setf '=)
    (+ '+)
    (- '-)
    (* '*)
    (/ '/)
    (rem '|\\|)
    (expt '^)
    (t x)))

(defun precedence (x)
  (case x
    (setf 0)
    (+ 1)
    (- 1)
    (* 2)
    (/ 3)
    (rem 3)
    (expt 4)
    (t 9)))

(defun pre-to-inf (l)
  (pre-to-inf-aux l -1))

(defun pre-to-inf-aux (l win)
  (cond ((null l) l)
        ((atom l) (list l))
        (t (let ((wout (precedence (first l))))
             (if (< wout win)
                 (list (pre-to-inf-sub l wout))
                 (pre-to-inf-sub l wout))))))

(defun pre-to-inf-sub (l wout)
  (append (pre-to-inf-aux (second l) wout)
          (list (opsymbol (first l)))
          (pre-to-inf-aux (third l) wout)))

;; My Part !

(defun pre2in (expr)
  "translate prefix to infix expressions.
  handles operators with any number of args."
  
  (labels ((intersperse (thing list)
              "Put thing between each of the elements in list. 
              (intersperse '|,| '(1 2 3)) => (1 |,| 2 |,| 3)"
              
              (if (symbolp thing)
                  (rest (mapcan (lambda (x) (list thing x)) list))
                  (when (stringp thing)
                      (string-trim "()" (write-to-string (rest (mapcan (lambda (x) (list thing x)) list)))))))
           
           (remove-brackets (lst)
              "reduces lists with just one item to the item itself"
              (do ((result lst (car result))) ((or (not (consp result)) (not (null (cdr result)))) result)))

           (separate-list (lst separator test)
              "returns list of sub-sequences defined by separator"
              (if (not (consp lst))
                  lst
                  (let ((result (cons separator nil))
                        (end 0)
                        (sub)
                        (lst
                         (if (funcall test (car lst) separator)
                             (cdr lst)
                             lst)))
                    (do ()
                      ((null lst) result)
                      (setf end (position separator lst :test test))
                      (setf sub (cons (subseq lst 0 end) nil))
                      (setf result (append result sub))
                      (setf lst
                            (if end
                                (nthcdr (+ 1 end) lst)
                                nil)))
                    (setf (cdr result) (mapcar #'remove-brackets (cdr result)))
                    result)))

           (separate-tree (lst separator test)
              "apply separate-list on all sublists"
              (if (or (not (consp lst)) (eql (first lst) 'quote))
                  lst
                  (progn
                    (setf lst
                          (mapcar
                           #'(lambda (x)
                               (if (not (consp x))
                                   x
                                   (separate-tree x separator test)))
                           lst))
                    (if (not (find separator (rest lst)))
                        lst
                        (separate-list lst separator test))))))
                
           (let ((result
                  (if (atom expr)
                      expr
                      (intersperse (car expr) (mapcar #'pre2in (cdr expr)))))
                 (seps '((setf . =) (setq . =) (expt . ^) (rem . |\\|) (mod . |\\|))))
              (setf result (sublis seps result))
              result)))

(defun in2pre (infix-expr &key (test #'eql))
  "converts an infix expression to prefix"
    (labels ((intersperse (thing list)
              "Put thing between each of the elements in list.
              (intersperse '|,| '(1 2 3)) => (1 |,| 2 |,| 3)"
              
              (if (symbolp thing)
                  (rest (mapcan (lambda (x) (list thing x)) list))
                  (if (stringp thing)
                      (string-trim "()" (write-to-string (rest (mapcan (lambda (x) (list thing x)) list)))))))
           
           (remove-brackets (lst)
              "reduces lists with just one item to the item itself"
              (do ((result lst (car result))) ((or (not (consp result)) (not (null (cdr result)))) result)))

           (separate-list (lst separator test)
              "returns list of sub-sequences defined by separator"
              (if (not (consp lst))
                  lst
                  (let ((result (cons separator nil))
                        (end 0)
                        (sub)
                        (lst
                         (if (funcall test (car lst) separator)
                             (cdr lst)
                             lst)))
                    (do ()
                      ((null lst) result)
                      (setf end (position separator lst :test test))
                      (setf sub (cons (subseq lst 0 end) nil))
                      (setf result (append result sub))
                      (setf lst
                            (if end
                                (nthcdr (+ 1 end) lst)
                                nil)))
                    (setf (cdr result) (mapcar #'remove-brackets (cdr result)))
                    result)))

           (separate-tree (lst separator test)
              "apply separate-list on all sublists"
              (if (or (not (consp lst)) (eql (first lst) 'quote))
                  lst
                  (progn
                    (setf lst
                          (mapcar
                           #'(lambda (x)
                               (if (not (consp x))
                                   x
                                   (separate-tree x separator test)))
                           lst))
                    (if (not (find separator (rest lst)))
                        lst
                        (separate-list lst separator test))))))

      (let ((result infix-expr) 
            (separators `(+ - * / = ^ ** setf setq expt))
            (seps `((= . setf) (^ . expt) (|\\| . rem))))
        (dolist (sep separators) (setf result (separate-tree result sep test)))
        (sublis seps (remove-brackets result)))))

(defun calc (expr)
  (pre2in (in2pre expr)))

(let ((expr '(setf total (* principal (expt (+ 1.0 interest) years)))))
  (pre2in expr))



;;
(defun in-to-pre (expr)
  (cond ((numberp expr) expr)
        (t (list (elt expr 1)
                 (in-to-pre (elt expr 0))
                 (in-to-pre (elt expr 2))))))

(defun in-eval (&optional (stream *standard-output*))
  (with-drawing-options (stream :ink +yellow+ :text-size :very-large)
                        (print 'infix->))
  (setf *text-size* 20)
  (setf (slot-value (sheet-medium *standard-input*) 'climi:text-style) (make-text-style nil :bold :very-large))
  (setf (slot-value (sheet-medium stream) 'climi:foreground) +goldenrod+)
  (let ((form (accept t :prompt nil :stream *standard-input*)))
    (with-drawing-options (*standard-input* :ink +red+ :text-style (make-text-style nil :italic nil))
                          (clear-output)
                          (formatting-item-list (stream)
                          (formatting-cell (stream)
                                           (display-formula stream (eval (in2pre form))))))
    (in-eval)))
