;;;;**************************************************************************
;;;;FILE:               rdp.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements a simple recursive descent parser.
;;;;    
;;;;    http://en.wikipedia.org/wiki/Formal_grammar
;;;;    http://en.wikipedia.org/wiki/Recursive_descent_parser
;;;;    http://en.wikipedia.org/wiki/Parsing_expression_grammar
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-09-09 <PJB> Created
;;;;BUGS
;;;;
;;;;    The parse-<non-terminal> functions are generated in the current
;;;;    package, therefore if two <non-terminal> of same name exist in
;;;;    two different grammars, there's a collision and the last take
;;;;    precedence.
;;;;    --> either put each grammar in its own package,
;;;;        or use the grammar name in the non-terminal parser functions.
;;;;
;;;;    The first set of a non-terminal that can reduce to the empty string
;;;;    should include the follow set of this non-terminal, but for this,
;;;;    we'd have to normalize the grammar rules, and then generate parsing
;;;;    functions that don't correspond directly to the source rules.
;;;;
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2006 - 2006
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************


(defpackage "COM.INFORMATIMAGO.RDP"
  (:use "COMMON-LISP")
  (:export "DEFGRAMMAR" "SEQ" "REP" "OPT" "ALT"
           "GENERATE-GRAMMAR"
           
           "GRAMMAR" "MAKE-GRAMMAR" "COPY-GRAMMAR"
           "GRAMMAR-NAME" "GRAMMAR-TERMINALS" "GRAMMAR-START" "GRAMMAR-RULES"
           "GRAMMAR-ALL-TERMINALS" "GRAMMAR-ALL-NON-TERMINALS"

           "FIND-RULE" "TERMINALP" "NON-TERMINAL-P" "FIRST-RHS" "FIRST-SET"

           "SCANNER" "MAKE-SCANNER" "COPY-SCANNER"
           "SCANNER-SOURCE" "SCANNER-FUNCTION" "SCANNER-POSITION"
           "SCANNER-CURRENT-TOKEN" "SCANNER-CURRENT-TEXT"
           "SCANNER-CURRENT-POSITION"
           "SCANNER-END-OF-SOURCE" "ACCEPT" "*SPACES*"
           ))
(in-package "COM.INFORMATIMAGO.RDP")


(defstruct grammar
  name terminals start rules
  all-terminals
  all-non-terminals)


;;; First, we define a grammar, with actions.
;;; The scanner and parser is generated at macro expansion time.

(defvar *linenum* 0)

(defmacro defgrammar (name &key terminals start rules (target-language :lisp))
  "
DO:     This macros generates a simple scanner and recursive decent parser 
        for the language described by this grammar.
        For each <non-terminal> in the grammar, a function PARSE-<non-terminal>
        is generated, in addition to a function SCAN-<name> and PARSE-<name>.
        If :DEBUG is on the *FEATURES* list, then the grammar structure
        is also generated for run-time in the variable <name>.

SYNTAX:
    (defgrammar <name>
        :terminals (( <terminal>       \"regexp\") ...)
        :start        <non-terminal>
        :rules     ((--> <non-terminal> <items> ) ...))

    <items>            ::= | <item> <items>
    <item>             ::= <seq> | <rep> | <alt> | <opt>
                         | <non-terminal> | <literal-terminal> | <terminal>
    <seq>              ::= (seq <item> <items> <action>)
    <rep>              ::= (rep <item> <items> <action>)
    <opt>              ::= (opt <item> <items> <action>)
    <alt>              ::= (alt <item> <items>)
    <action>           ::= | :ACTION <forms>
    <forms>            ::= | <form> <forms>
    <form>             ::= form        -- any lisp form.
    <non-terminal>     ::= symbol      -- any lisp symbol (keywords reserved).
    <terminal>         ::= symbol      -- any lisp symbol (keywords reserved).
    <literal-terminal> ::= string      -- any lisp string.

SEMANTICS:

        The terminals are either named terminals listed in the :TERMINALS
        clause, or literal terminals used directly in the productions as 
        lisp strings.
        A basic regular expression regex(3) may be given that
        will be matched by the scanner to infer the given terminal.
        The literal terminals are matched first, the longest first,
        and with \> appended to terminals ending in a letter or digit.
        Then the regular expressions are matched in the order given.
         
        :START specifies the start non-terminal symbol.

        The non-terminal symbols are infered implicitely from the grammar rules.

        If there are more than one subforms, or an action,
        the REP and OPT forms take an implicit SEQ:
          (REP a b c :ACTION f)   -->  (REP (SEQ a b c :ACTION f))
          (OPT a b c :ACTION f)   -->  (OPT (SEQ a b c :ACTION f))
          (REP a :ACTION f)       -->  (REP (SEQ a :ACTION f))
          (OPT a :ACTION f)       -->  (OPT (SEQ a :ACTION f))
          (REP a)                 -->  (REP a)
          (OPT a)                 -->  (OPT a)

        Embedded ALT are flattened:
          (ALT a b (ALT c d) e f) --> (ALT a b c d e f)

        Actions are executed in a lexical environment where the symbols $1, $2,
        etc are bound to the results of the subforms. $0 is bound to the 
        list of the results of the subforms.
        
        The action for REP (normalized) is to return a possibly
        empty list of the results of its single subform repeated.
        (REP is 0 or more).

        The action for OPT (normalized) is to return either NIL,
        or the result of its single subform unchanged.

        The action for an ALT is to return the result of the selected 
        alternative unchanged.

        The default action for an internal SEQ is to return the list of the
        results of its subforms.

        The default action for an implicit SEQ for a given <non-terminal> lhs
        is to return the list of the results of its subforms prefixed by the
        <non-terminal> symbol.

TODO:   We could also flatten sequences without action, or even sequences with
        actions with renumbering.
"
  (let ((grammar (make-grammar :name name
                               :terminals terminals
                               :start start
                               :rules (normalize-rules rules)))
        (*linenum* 0))
    (compute-all-terminals     grammar)
    (compute-all-non-terminals grammar)
    `(progn
       #+debug ,@(let ((g (gensym)))
                      `((defparameter ,name
                          (let ((,g (make-grammar
                                     :name name
                                     :terminals terminals
                                     :start start
                                     :rules (normalize-rules rules))))
                            (compute-all-terminals     ,g)
                            (compute-all-non-terminals ,g)
                            ,g))))
       
       ,(gen-boilerplate target-language)
       ,(generate-scanner target-language grammar)
       ,@(mapcar (lambda (non-terminal)
                   (generate-nt-parser target-language grammar non-terminal))
                 (grammar-all-non-terminals grammar))
       ,(generate-parser target-language grammar))))



(defun generate-grammar (name &key terminals start rules (target-language :lisp))
  (let ((grammar (make-grammar :name name
                               :terminals terminals
                               :start start
                               :rules (normalize-rules rules)))
        (*linenum* 0))
    (compute-all-terminals     grammar)
    (compute-all-non-terminals grammar)
    (eval `(progn
             ,(gen-boilerplate target-language)
             ,(generate-scanner target-language grammar)
             ,@(mapcar (lambda (non-terminal)
                         (generate-nt-parser target-language grammar non-terminal))
                       (grammar-all-non-terminals grammar))
             ,(generate-parser target-language grammar)))
    grammar))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Normalization of  the grammar rules:

(defun split-action (rhs)
  (declare (inline))
  (let ((separator (position :action rhs)))
    (if separator
        (values (subseq rhs 0 separator) (subseq rhs (1+ separator)))
        (values rhs                      nil))))


(defun normalize-seq (expr)
  (multiple-value-bind (rhs actions) (split-action (cdr expr))
    (setf actions (or actions '($0)))
    (let ((items (mapcar (lambda (item) (normalize item)) rhs)))
      (if (and (null actions) (or (null items) (null (cdr items))))
          (car items)
          (list 'seq items actions)))))

(defun normalize-with-action (expr)
  (multiple-value-bind (rhs actions) (split-action (cdr expr))
    (if (null actions)
        (cond ((null rhs) nil)
              ((null (cdr rhs)) `(,(car expr) ,(normalize (car rhs))))
              (t `(,(car expr) ,(normalize-seq `(seq ,@rhs)))))
         `(,(car expr) ,(normalize-seq `(seq ,@rhs :action ,@actions))))))

(defun normalize-rep (expr) (normalize-with-action expr))
(defun normalize-opt (expr) (normalize-with-action expr))

(defun normalize-alt (expr)
  (assert (not (find :action expr)))
  (let ((items (mapcar (function normalize) (cdr expr))))
    (if (null (cdr items))
        (car items)
        `(alt ,@(mapcan (lambda (item)
                          (cond ((atom item) (list item))
                                ((eql 'alt (car item)) (cdr items))
                                (t (list item))))
                        items)))))

(defun normalize (expr)
  (if (atom expr)
      expr
      (ecase (car expr)
        ((seq) (normalize-seq expr))
        ((rep) (normalize-rep expr))
        ((alt) (normalize-alt expr))
        ((opt) (normalize-opt expr)))))

(defun normalize-rules (rules)
  (mapcar (lambda (rule)
            (destructuring-bind (--> non-term &rest items) rule
               (assert (string= --> '-->))
               `(,non-term ,(normalize
                             (if (find :action items)
                                 `(seq ,@items)
                                 `(seq ,@items :action `(,',non-term ,@$0)))))))
          rules))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun word-equal (a b)
  (or (and (stringp a) (stringp b) (string= a b))
      (eql a b)))

(defun compute-all-terminals (grammar)
  (labels  ((find-strings (items)
              (cond
                ((stringp items) (list items))
                ((atom items)    '())
                (t (ecase (car items)
                     ((seq)
                      (mapcan (function find-strings) (second items)))
                     ((rep alt opt)
                      (mapcan (function find-strings) (cdr items))))))))
    (setf (grammar-all-terminals grammar)
          (delete-duplicates
           (append
            (mapcar (function first) (grammar-terminals grammar))
            (mapcan (function find-strings)
                    (mapcar (function second) (grammar-rules grammar))))
           :test (function word-equal)))))


(defun compute-all-non-terminals (grammar)
  (labels ((find-symbols (items)
             (cond
               ((symbolp items) (list items))
               ((atom items)    '())
               (t (ecase (car items)
                    ((seq)
                     (mapcan (function find-symbols) (second items)))
                    ((rep alt opt)
                     (mapcan (function find-symbols) (cdr items))))))))
    (setf (grammar-all-non-terminals grammar)
          (set-difference
           (delete-duplicates
            (append
             (list (grammar-start grammar))
             (mapcar (function first) (grammar-rules grammar))
             (mapcan (function find-symbols)
                     (mapcar (function second) (grammar-rules grammar)))))
           (grammar-all-terminals grammar)))))


;;; To implement the follow-set function we'd need to put the grammar
;;; into a normal form, which we don't really need to map it simplistically
;;; to recursive descent parser functions.

;; (defun follow-set (grammar non-terminal)
;;   "return the set of terminal symbols that may follow the non-terminal
;; in the grammar."
;;   (mapcar (lambda (rule)
;;             (destructuring-bind (nt expr) rule
;; 
;;               ))
;;           (grammar-rules grammar)))


(defun find-rule (grammar non-terminal)
  (let ((rules (mapcar (function second)
                       (remove-if-not (lambda (rule) (eql non-terminal (first rule)))
                                      (grammar-rules grammar)))))
    (cond
      ((null rules) (error "~s is not a non-terminal" non-terminal))
      ((null (cdr rules)) (car rules))
      (t `(alt ,@(normalize-alt rules))))))


(defun terminalp (grammar item)
  (member item (grammar-all-terminals grammar)
          :test (function word-equal)))

(defun non-terminal-p (grammar item)
  (member item (grammar-all-non-terminals grammar)))


(defun first-rhs (grammar item)
  (if (atom item)
      (if (terminalp grammar item)
          (list item)
          (first-set grammar item))
      (ecase (car item)
        ((seq) (loop
                  :with all-firsts = '()
                  :for items :in (second item)
                  :for firsts = (first-rhs grammar items)
                  :while (member nil firsts)
                  :do (setf all-firsts
                            (union firsts (delete nil all-firsts)))
                  :finally (setf all-firsts
                                 (union firsts (delete nil all-firsts)))
                  (return all-firsts)))
        ((rep opt) (cons nil (first-rhs grammar (second item))))
        ((alt) (mapcan (lambda (item) (first-rhs grammar item)) (rest item))))))


(defun first-set (grammar non-terminal)
  "return the set of terminal symbols by which the non-terminal may start
in the grammar."
  (delete-duplicates (first-rhs grammar (find-rule grammar non-terminal))
                       :test (function word-equal)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generator -- LISP

(defmethod gen-boilerplate ((target (eql :lisp)))
  `(progn
     
    (defstruct scanner
      source
      function
      (position 0)
      (current-token nil)
      (current-text "")
      (current-position 0))

    (defun scanner-end-of-source (scanner)
      (<= (length (scanner-source scanner)) (scanner-position scanner)))

    (defun accept (scanner token)
      (if (word-equal token (scanner-current-token scanner))
          (prog1 (list (scanner-current-token scanner)
                       (scanner-current-text scanner)
                       (scanner-current-position scanner))
            (funcall (scanner-function scanner) scanner))
          (error "At position ~D, expected ~S, not ~S"
                 (scanner-current-position scanner)
                 token
                 (scanner-current-token scanner))))

    (defparameter *spaces*
      (format nil "^[~{~C~}]\\+" '(#\space #\newline #\tab)))))


(defmethod gen-scanner-function-name ((target (eql :lisp)) grammar-name)
  (intern (format nil "SCAN-~A" grammar-name)))

(defmethod generate-scanner ((target (eql :lisp)) grammar)
  #-clisp (error "This generator uses the clisp specific package REGEXP
Please, update it use whatever regexp package is available in ~A"
                 (lisp-implementation-type))
  (let ((lit-terminals-regexp
         (format nil "^\\(~{~A~^\\|~}\\)"
                 (mapcar
                  (lambda (item)
                    (let ((item (pregexp::pregexp-quote item)))
                      (if (alphanumericp (aref item (1- (length item))))
                          (concatenate 'string item "\\>")
                          item)))
                  (sort (remove-if-not (function stringp)
                                       (grammar-all-terminals grammar))
                        (function >) :key (function length))))))
    `(defun
         ,(gen-scanner-function-name target (grammar-name grammar))
         (scanner)
       (let ((match (pregexp::pregexp-match *spaces*
                                  (scanner-source scanner)
                                  :start (scanner-position scanner))))
         (when match
           (setf (scanner-position scanner) (pregexp::pregexp-match-positions match)))
         (setf (scanner-current-position scanner) (scanner-position scanner))
         (cond
           ((scanner-end-of-source scanner)
            (setf (scanner-position scanner)   (length (scanner-source scanner))
                  (scanner-current-text scanner)  "<END OF SOURCE>"
                  (scanner-current-token scanner) nil))
           ((setf match (pregexp::pregexp-match ',lit-terminals-regexp
                                      (scanner-source scanner)
                                      :start (scanner-position scanner)))
            (setf (scanner-position scanner)      (pregexp::pregexp-match-positions match)
                  (scanner-current-text scanner)  (pregexp::pregexp-match (scanner-source scanner) match)
                  (scanner-current-token scanner) (scanner-current-text scanner)))
           ,@(mapcar
              (lambda (terminal)
                `((setf match (pregexp::pregexp-match
                               ',(format nil "^\\(~A\\)" (second terminal))
                               (scanner-source scanner)
                               :start (scanner-position scanner)))
                  (setf (scanner-position scanner)      (pregexp::pregexp-match-positions match)
                        (scanner-current-text scanner)  (pregexp::pregexp-match (scanner-source scanner) match)
                        (scanner-current-token scanner) ',(first terminal))))
              (grammar-terminals grammar))
           (t (error "Invalid character ~C at position: ~D"
                     (aref (scanner-source scanner) (scanner-position scanner))
                     (scanner-position scanner))))))))


(defmethod gen-parse-function-name ((target (eql :lisp)) non-terminal)
  (intern (format nil "PARSE-~A" non-terminal)))

(defmethod gen-in-firsts ((target (eql :lisp)) firsts)
  (if (null (cdr firsts))
      `(word-equal (scanner-current-token scanner) ',(car firsts))
      `(member  (scanner-current-token scanner) ',firsts
                :test (function word-equal))))

(defmethod gen-parsing-statement ((target (eql :lisp)) grammar item)
  (if (atom item)
      (if (terminalp grammar item)
          `(accept scanner ',item)
          (let* ((firsts (first-rhs grammar item))
                 (emptyp (member nil firsts)))
            `(,(if emptyp 'when 'if) ,(gen-in-firsts target (remove nil firsts))
               (,(gen-parse-function-name target item) scanner)
               ,@(unless emptyp
                         '((error "Unexpected token ~S"
                            (scanner-current-token scanner)))))))
      (ecase (car item)
        ((seq)
         (destructuring-bind (seq items actions) item
           (declare (ignore seq))
           (let ((index 0))
             `(let ,(mapcar (lambda (item)
                              `(,(intern (format nil "$~D" (incf index)))
                                 ,(gen-parsing-statement target grammar item)))
                            items)
                (let (($0 (list ,@(loop :for i :from 1 :to index
                                     :collect (intern (format nil "$~D" i))))))
                  ,@actions)))))
        ((rep)
         `(loop
             :while ,(gen-in-firsts target (first-rhs grammar (second item)))
             :collect ,(gen-parsing-statement target grammar (second item))))
        ((opt)
         `(when ,(gen-in-firsts target (first-rhs grammar (second item)))
            ,(gen-parsing-statement target grammar (second item))))
        ((alt)
         `(cond
            ,@(mapcar (lambda (item)
                        `(,(gen-in-firsts target (first-rhs grammar item))
                           ,(gen-parsing-statement target grammar item)))
                      (cdr item)))))))


(defmethod generate-nt-parser ((target (eql :lisp)) grammar non-terminal)
  `(defun ,(gen-parse-function-name target non-terminal) (scanner)
     ,(gen-parsing-statement target grammar  (find-rule grammar non-terminal))))


(defmethod generate-parser ((target (eql :lisp)) grammar)
  (let ((scanner-function
         (gen-scanner-function-name target (grammar-name grammar))))
    `(defun ,(gen-parse-function-name target (grammar-name grammar))
         (source)
       (let ((scanner (make-scanner :source source
                                    :function (function ,scanner-function))))
         (,scanner-function scanner)
         (prog1 (,(gen-parse-function-name target
                                           (grammar-start grammar)) scanner)
           (unless (scanner-end-of-source scanner)
             (error "End of source NOT reached.")))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
