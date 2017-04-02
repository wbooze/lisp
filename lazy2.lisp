(defpackage my-lazy
  (:use :cl)
  (:shadow cl:car cl:cdr cl:mapcar)
  (:export "CAR" "CDR" "MAPCAR" "DELAY" "FORCE" "REPEAT"))

(in-package :my-lazy)

;; clojure idiom
(require 'cl-ppcre)
(defun numbered-arg-as-string (arg)
  (cl-ppcre:scan-to-strings "^%\\d+$" (string arg)))

(defun single-arg-as-string (arg)
  (let ((sarg (string arg)))
    (when (string-equal "%" sarg)
      sarg)))

(defun arc-arg-as-string (arg)
  (let ((sarg (string arg)))
    (when (string-equal "_" sarg)
      sarg)))

(defun rest-arg-as-string (arg)
  (let ((sarg (string arg)))
    (when (string-equal "%&" sarg)
      sarg)))

(defun flatten (l)
  "flattens a list"
  (cond ((null l) l)
      ((atom l) (list l))
    (t (append (flatten (car l))
           (flatten (cdr l))))))

(defun make-arg-list (predicate delimited-list)
  (labels ((string-list (delimited-list)
         (mapcar (lambda (x)
               (cond ((symbolp x) (funcall predicate x))
                 ((listp x) (string-list x))))
             delimited-list)))
    (remove-duplicates (mapcar #'intern
                   (sort (flatten (string-list delimited-list))
                     #'string-lessp) ;; BUG: if more than 9 numbered arguments are used
                   ))))

;; first check for numbered args, 
;; then for a single % arg, 
;; finally default to a single _ arg
;; swallow the rest args to get around style warnings
(set-macro-character #\[
             (lambda (stream char)
               (let* ((sexp (read-delimited-list #\] stream t))
                  (args (make-arg-list #'numbered-arg-as-string sexp))
                  (rest-args (make-arg-list #'rest-arg-as-string sexp))
                  (rest-arg (or (car rest-args) (gensym))))
             (unless args
               (setf args (make-arg-list #'single-arg-as-string sexp)))
             (unless args
               (setf args (make-arg-list #'arc-arg-as-string sexp))) ;; arc idiom (_)
             `(lambda (,@args &rest ,rest-arg) (identity ,rest-arg) (,@sexp)))))

(set-macro-character #\]
             (get-macro-character #\)))

(defmethod to-string (arg) (string arg))

(defmethod to-string ((arg integer)) (write-to-string arg))

(defun str (&rest args)
  (apply 'concatenate-string  (mapcar #'to-string args)))

(defstruct thunk
  body)

(defun thunkp (arg)
  (eq (type-of arg) 'thunk))

(defmacro delay (expr)
  `(make-thunk
   :body (lambda () ,expr)))

(defun force (thunk)
  (if (thunkp thunk)
      (funcall (thunk-body thunk))
      thunk))

(defun repeat (arg)
  (cons arg
    (delay (repeat arg))))

(defun car (cons)
  "car for lists, force car for thunks"
  (force (cl:car cons)))

(defun cdr (cons)
  "cdr for lists, force cdr for thunks"
  (force (cl:cdr cons)))

(defun mapcar (f list &rest more-lists)
  "Apply FUNCTION to successive elements of LIST. 
Return list of FUNCTION return values.
lists can be lazy"
  (cons (apply f
           (car list)
           (cl:mapcar 'car more-lists))
    (when (and (cdr list) (every #'identity more-lists))
      (apply 'mapcar
         f
         (cdr list)

             (cl:mapcar 'cdr more-lists)))))

(defun partial (f &rest args)
  "currying function"
  (lambda (&rest more-args)
    (apply f (append args more-args))))
