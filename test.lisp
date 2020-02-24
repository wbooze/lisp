#+cmu
(defun getenv (name &optional default)
  (let ((x (assoc name ext:*environment-list*
	     :test #'string=)))
    (if x (cdr x) default)))

#+sbcl
(require 'sb-posix)

(defvar symbols nil)

(do-all-symbols (sym)
  (let ((package (symbol-package sym)))
       (cond
         ((not (fboundp sym)))
         ((or (eql #.(find-package :cl) package)
              (eql #.(find-package :sb-ext) package)
              (eql #.(find-package :cl-user) package))
          (pushnew (symbol-name sym) symbols))
         ((eql #.(find-package :keyword) package)
          (pushnew (concatenate 'string ":" (symbol-name sym)) symbols))
         (package
           (pushnew (concatenate 'string
                                 (package-name package)
                                 ":"
                                 (symbol-name sym))
                    symbols)))))

(with-open-file (output #.(concatenate 'string
			    #+sbcl (sb-posix:getenv "HOME")
			    #+cmu  (getenv "HOME")
			    #+sbcl "/.sbcl_completions"
			    #+cmu  "/.cmucl_completions")
		  :direction :output :if-exists :append
		  :if-does-not-exist :create)
  (format output "~{~(~A~)~%~}" (sort symbols #'string<)))
