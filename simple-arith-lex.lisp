(asdf:load-system :graylex)
(asdf:load-system :yacc)

(defpackage :simple-arith-parsing-test
  (:use :cl :graylex :yacc))

(in-package :simple-arith-parsing-test)

(defun arith-lexer (stream)
  (make-instance
   'graylex::lexer-input-stream
   :stream stream
   :rules '(("\\+" . +)
            ("\\*" . *)
            ("/"   . /)
            ("-" . -)
            ("[0-9]+" . int)
            ("[a-z]" . var)
            ("\\(" . |(| )
            ("\\)" . |)|)
            (" +" . space))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun i2p (a b c)
    "Infix to prefix"
    (list b a c))

  (defun k-2-3 (a b c)
    "Second out of three"
    (declare (ignore a c))
    b))

(define-parser *expression-parser*
  (:start-symbol expression)
  (:terminals (int var + - * / |(| |)|))
  (:precedence ((:left * /) (:left + -)))

  (expression
   (expression + expression #'i2p)
   (expression - expression #'i2p)
   (expression * expression #'i2p)
   (expression / expression #'i2p)
   term)

  (term var int (- term) (|(| expression |)| #'k-2-3)))


;; TEST    

(with-input-from-string (s " x*(-2)+3*y")
 (let ((lex (arith-lexer s)))
   (print (parse-with-lexer #'(lambda ()
			 (multiple-value-bind (a b) (graylex:stream-read-token lex)
					      (if (eql a 'space)
						  ;; ignore spaces and just read more.                         
						  (graylex:stream-read-token lex)
						(values a b))))
			  *expression-parser*))))
