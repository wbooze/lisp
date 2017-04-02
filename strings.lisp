(defun string-to-number (str &optional (base *read-base*))
  (read-from-string str base))
      
(defun number-to-string (num &optional (base *read-base*))
  (write-to-string num :base base))

(defun read-char-by-name (stream)
  "blabla"
  (let* ((input stream))
    (cond
      ((equal "^[0-9a-fA-F]+s" input)
       (string-to-number input 16))
      ((equal "^#" input)
       (read input))
      (t (read stream)))))

(defun clim-read-char-by-name (stream)
  "blabla"
  (let* ((input stream))
    (cond
      ((equal "^[0-9a-fA-F]+s" input)
       (string-to-number input 16))
      ((equal "^#" input)
       (read input))
      (t (read stream)))))

(defun insert (arg)
  arg)

(defun ucs-insert (&optional character (count 1))
  "given a character returns the unicode symbol or reads input and then returns the symbol"
  (let ((character (or character (read-char-by-name *standard-input*)))
        (count (or count 1))
        (result (insert (or (string (code-char character)) (code-char character)))))
    (progn
      (dotimes (i count)
        (princ result)) result)))


(defun clim-ucs-insert (&optional character (count 1))
  "given a character returns the unicode symbol or reads input and then returns the symbol"
  (let ((character (or character (read-char-by-name *standard-input*)))
        (count (or count 1))
        (result (insert (or (string (code-char character)) (code-char character)))))
    (progn
      (dotimes (i count)
        (princ result)) result)))