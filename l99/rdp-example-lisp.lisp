;;;;**************************************************************************
;;;;FILE:               example-lisp.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    An example grammar for the recusive descent parser generator.
;;;;    The actions are written in Lisp, to generate a lisp parser.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2006-09-10 <PJB> Created.
;;;;BUGS
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


(defpackage "EXAMPLE"
  (:use "COMMON-LISP" "COM.INFORMATIMAGO.RDP")
  (:export "PARSE-EXAMPLE"))
(in-package "EXAMPLE")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example Language 
;;; taken from: http://en.wikipedia.org/wiki/Recursive_descent_parser
;;;

(defgrammar example
    :terminals ((ident   "[A-Za-z][A-Za-z0-9]*")
                ;; real must come first to match the longest first.
                (real    "^\\([-+]\\?[0-9]\\+\\.[0-9]\\+\\([Ee][-+]\\?[0-9]\\+\\)\\?\\)")
                (integer "[-+]\\?[0-9]\\+"))
    :start program
    :rules ((--> factor
                 (alt ident
                      number
                      (seq "(" expression ")" :action $2))
                 :action $1)
            (--> number  (alt integer real) :action $1)
            (--> term
                 factor (rep (alt "*" "/") factor)
                 :action `(,$1 . ,$2))
            (--> expression
                 (opt (alt "+" "-"))
                 term
                 (rep (alt "+" "-") term :action `(,$1 ,$2))
                 :action `(+ ,(if $1 `(,$1 ,$2) $2)  . ,$3))
            (--> condition
                 (alt (seq "odd" expression
                           :action `(oddp ,$2))
                      (seq expression
                           (alt "=" "#" "<" "<=" ">" ">=")
                           expression
                           :action `(,$2 ,$1 ,$3)))
                 :action $1)
            (--> statement
                 (opt (alt (seq ident ":=" expression
                                :action `(setf ,$1 ,$3))
                           (seq "call" ident
                                :action `(call ,$2))
                           (seq "begin" statement
                                (rep ";" statement
                                     :action $2)
                                "end"
                                :action `(,$2 . ,$3))
                           (seq "if" condition "then" statement
                                :action `(if ,$2 ,$4))
                           (seq "while" condition "do" statement
                                :action `(while ,$2 ,$4))))
                 :action $1)
            (--> block
                 (opt "const" ident "=" number
                      (rep "," ident "=" number
                           :action `(,$2 ,$4))
                      ";"
                      :action `((,$2 ,$4) . ,$5))
                 (opt "var" ident
                      (rep "," ident :action $2)
                      ";"
                      :action `(,$2 . ,$3))
                 (rep "procedure" ident ";" block ";"
                      :action `(procedure ,$2 ,$4))
                 statement
                 :action `(block ,$1 ,$2 ,$3 ,$4))
            (--> program
                 block "." :action $1)))



(defpackage "EXAMPLE-WITHOUT-ACTION"
  (:use "COMMON-LISP" "COM.INFORMATIMAGO.RDP")
  (:export "PARSE-EXAMPLE-WITHOUT-ACTION"))
(in-package "EXAMPLE-WITHOUT-ACTION")

(defgrammar example-without-action
    :terminals ((ident   "[A-Za-z][A-Za-z0-9]*")
                ;; real must come first to match the longest first.
                (real    "^\\([-+]\\?[0-9]\\+\\.[0-9]\\+\\([Ee][-+]\\?[0-9]\\+\\)\\?\\)")
                (integer "[-+]\\?[0-9]\\+"))
    :start program
    :rules ((--> factor
                 (alt ident
                      number
                      (seq "(" expression ")")))
            (--> number  (alt integer real))
            (--> term
                 factor (rep (alt "*" "/") factor))
            (--> expression
                 (opt (alt "+" "-"))
                 term
                 (rep (alt "+" "-") term))
            (--> condition
                 (alt (seq "odd" expression)
                      (seq expression
                           (alt "=" "#" "<" "<=" ">" ">=")
                           expression)))
            (--> statement
                 (opt (alt (seq ident ":=" expression)
                           (seq "call" ident)
                           (seq "begin" statement
                                (rep ";" statement)
                                "end")
                           (seq "if" condition "then" statement)
                           (seq "while" condition "do" statement))))
            (--> block
                 (opt "const" ident "=" number
                      (rep "," ident "=" number) ";")
                 (opt "var" ident (rep "," ident) ";")
                 (rep "procedure" ident ";" block ";")
                 statement)
            (--> program
                 block ".")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
