;;; #! /u/b/r/breakds/usr/local/bin/sbcl --script
;;; Simple Semi Supervised Semantic Labeling Wrapper

;;; Author: BreakDS
;;; Maintainer: BreakDS
;;; Date: Thu May 24 16:37:23 CDT 2012


(use-package :sb-thread)

;;; Global Settings
(defparameter *path-num* 4)
(defparameter *testing* nil)
(defparameter *training* nil)
(defparameter *shared-folder* "shared")
(defparameter *template* "template.conf")
(defparameter *pwd* (namestring (truename ".")))


;;; Utilities
(defmacro compose-file-name (&rest parts)
  "compose a filename under current *pwd*"
  `(concatenate 'string *pwd* 
		,@(mapcar (lambda (x) `(format nil "/~a" ,x))
			  parts)))

(defun run-command (command &optional args)
  "run a shell comamnd and reflect the stdout on screen."
  (let* ((process (sb-ext:run-program command args
                                     :output :stream
                                     :wait nil))
         (output (sb-ext:process-output process)))
    (loop for line = (read-line output nil)
       while line do (format t "~a~%" line))))



(setf *testing* '("1" "2" "3" "4"))
(setf *training* '("5" "6" "7" "8"))


(defun gen-conf (path-id target labeled)
  "Prepare the configuration file"
  (format t "[~a]: ~a~%" path-id target)
  (let ((current-dir (compose-file-name path-id)))
    (run-command "/bin/cp" (list "-f" (compose-file-name *shared-folder* *template*)
                                 (format nil "~a/Prediction.conf" current-dir)))
    (with-open-file (*standard-output* (format nil "~a/Prediction.conf" current-dir)
    				       :direction :output
    				       :if-exists :append)
      (format t "--estimate ~a~%" path-id))))


(defun first-iteration ()
  (loop for i below 20
       do (gen-conf (thread-name *current-thread*) (format nil "~a" i) (list "123" "456"))))


;;; main
(defun main ()
  (let ((child-threads (loop for i below *path-num*
                          collect (make-thread 
				   (lambda () (first-iteration))
				   :name (format nil "~a" i)))))
    (loop for th in child-threads
       do (join-thread th))))


(main)
