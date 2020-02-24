;;;; -*- mode:Lisp; syntax:Common-Lisp; package:user -*- ;;;;

;;;; TOP-LEVEL INTERFACE

(defun run-interface ()
  (format t "~&Welcome to the tools database.  ~
	       Please type questions or commands,~
	     ~%followed by the return key, ~
	       or press the return key twice to stop.")
  (print '>)
  (do ((input (read-sentence) (read-sentence)))
      ((endp input)
       (format t "~&Ok---goodbye.")
       (values))
    (unless (interface input)
      (format t "~&Sorry---I can't understand that.~
              ~&Press the return key twice if you want to stop."))
    (format t "~&~%> ")))

#+comment
(defun read-sentence (&aux sentence)
  (with-input-from-string
    (input (string-trim ".?!" (read-line)))
    (do ((word (read input nil) (read input nil)))
        ((not word) (return (reverse sentence)))
      (push word sentence))))

(defun read-sentence (&aux sentence)
  "
  Remarks:	Use this alternate definition if your Lisp
		has trouble because the carriage returns
		required by UNIX are handed to read-line.
		Note that termination then requires two
		carriage returns in a row.  
  "
 (labels ((strange-read-line ()
           (let ((line (string-trim ".?!" (read-line))))
	     (if (zerop (length line))
		 (string-trim ".?!" (read-line))
	       line))))
  (with-input-from-string
    (input (strange-read-line))
    (do ((word (read input nil) (read input nil)))
        ((not word) (return (reverse sentence)))
      (push word sentence)))))

;;;; TREE DEFINITION AND COMPILATION

(defmacro compile-tree (name tree)
  "
  Purpose:	Launch COMPILE-ELEMENTS.
  "
  `(defun ,name (word-list)
     ,(compile-elements tree)))

(defun compile-elements (tree)
  "
  Purpose:	Translate grammar into a program.
  Remarks:	Contains a COND clause for every grammar element.
		See the grammar data file for sample use.
  "
  (cond ((endp tree) '(values t nil word-list))
        ((eq '> (first tree))
         `(multiple-value-bind (result ,(second tree) word-list)
              (,(second tree) word-list)
            (when result
                  ,(compile-elements (rest (rest tree))))))
        ((eq 'brnchs (first tree))
         (compile-branches (rest tree)))
        ((eq 'rtn (first tree))
         `(values t (progn ,@(rest tree)) word-list))
        ((eq 'if-end-rtn (first tree))
         `(when (null word-list)
            (values t (progn ,@(rest tree)) nil)))
        (t `(let ((current-word (first word-list))
                  (word-list (rest word-list)))
              (when (eq current-word ',(first tree))
                  ,(compile-elements (rest tree)))))))

(defun compile-branches (forms)
  (unless (endp forms)
    `(multiple-value-bind (result binding words-left-over)
         ,(compile-elements (first forms))
         (if result
             (values result binding words-left-over)
             ,(compile-branches (rest forms))))))

