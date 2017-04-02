(defun backslash-reader (stream char)
   (declare (ignore char))
   `(,(loop for char = (read-preserving-whitespace stream t nil t)
                   then (progn (read-char stream t nil t)
                               (read-preserving-whitespace stream t nil t))
                   collect char
                   while (eql (peek-char nil stream nil nil t) #\\))))
 (set-macro-character #\\ #'backslash-reader)
