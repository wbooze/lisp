(defvar *unicode-test-file* "faithtest-out.txt")

(defun generate-256 (&key (filename *unicode-test-file*)
			  #+CLISP (charset 'charset:iso-8859-1)
                          external-format)
  (let ((e (or external-format
	       #+CLISP (ext:make-encoding :charset charset :line-terminator :unix))))
    (describe e)
    (with-open-file (f filename :direction :output :if-exists :supersede
		     :external-format e)
      (write-sequence
        (loop with s = (make-string 256)
	      for i from 0 to 255
	      do (setf (char s i) (code-char i))
	      finally (return s))
       f)
      (file-position f))))

;(generate-256 :external-format :default)
;#+CLISP (generate-256 :external-format :unix)
;#+CLISP (generate-256 :external-format 'charset:ascii)
;(generate-256)

(defun check-256 (&optional (filename *unicode-test-file*))
  (with-open-file (f filename :direction :input
		     :element-type '(unsigned-byte 8))
    (loop for i from 0
	  for c = (read-byte f nil nil)
	  while c
	  unless (= c i)
	  do (format t "~&Position ~D found ~D(#x~X)." i c c)
	  when (and (= i 33) (= c 32))
	  do (let ((c (read-byte f)))
	       (format t "~&Resync back 1 byte ~D(#x~X) - cause CRLF?." c c) ))
    (file-length f)))

#| CLISP
(check-256 *unicode-test-file*)
(progn (generate-256 :external-format :unix) (check-256))
; uses UTF-8 -> 385 bytes

(progn (generate-256 :charset 'charset:iso-8859-1) (check-256))

(progn (generate-256 :external-format :default) (check-256))
; uses UTF-8 + CRLF(on MS-Windows) -> 387 bytes

(progn (generate-256 :external-format
  (ext:make-encoding :charset 'charset:iso-8859-1 :line-terminator :mac)) (check-256))
(progn (generate-256 :external-format
  (ext:make-encoding :charset 'charset:iso-8859-1 :line-terminator :dos)) (check-256))
|#