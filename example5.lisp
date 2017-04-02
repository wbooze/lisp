(defun ucs-insert (&optional character (count 1))
  "given a character returns the unicode symbol or reads input and then returns the symbol"

  (if character
    (let ((result (or (string (code-char character)) (string character))))
      (progn
        (dotimes (i count)
         (format t "~s" result))))

    (progn
      (let* (
             (character (read-char))
             (result (or (string character) (string (code-char character)))))
      (dotimes (i count)
       (format t "~s" result))))))

(defun ascii-table ()
  (let ((i -1))
    (format t "~&ASCII characters 32 thru 127.~&~%")
    (format t "   Dec  Hex  Char         |   Dec  Hex   Char         |   Dec  Hex   Char         |   Dec  Hex   Char~%")
    (loop while (< i 31) do
      (princ (format nil "~4d ~4x    ~12s | ~4d ~4x    ~12s | ~4d ~4x    ~12s | ~4d ~4x    ~12s~%"
	       (setq i (+ 33  i)) i (code-char i)
	       (setq i (+ 32 i)) i (code-char i)
	       (setq i (+ 32 i)) i (code-char i)
	       (setq i (+ 1 i)) i (code-char i)))
      (setq i (- i 95)))) (values))

(defun ascii-table-s ()
  (let ((i -1))
    (format t "~&ASCII characters 32 thru 127.~&~%")
    (format t "   Dec  Hex  Char         |   Dec  Hex   Char         |   Dec  Hex   Char         |   Dec  Hex   Char~%")
    (loop while (< i 31) do
      (princ (format nil "~4d ~4x    ~12s | ~4d ~4x    ~12s | ~4d ~4x    ~12s | ~4d ~4x    ~12s~%"
	       (setq i (+ 33  i)) i (string (code-char i))
	       (setq i (+ 32 i)) i (string (code-char i))
	       (setq i (+ 32 i)) i (string (code-char i))
	       (setq i (+ 1 i)) i (string (code-char i))))
      (setq i (- i 95)))) (values))

(defun extended-table ()
  (let ((i 128))
    (format t "~&extended ASCII characters (unicode) 128 thru 256.~&~%")
    (format t " Dec   Hex   Char  |  Dec   Hex   Char~%")
    (loop while (< i 256) do
      (princ (format nil "~4d ~4x ~50s  |  ~4d ~4x ~50s~%"
	       i i (code-char i)
	       (incf i) i (code-char i)))
      (incf i))) (values))

(defun extended-table-s ()
  (let ((i 128))
    (format t "~&extended ascii characters (unicode) 128 thru 256.~&~%")
    (format t " dec   hex   char  |  dec   hex   char~%")
    (loop while (< i 256)
          do (princ
              (format nil "~4d ~4x ~50s  |  ~4d ~4x ~50s~%" 
											i i (string (code-char i)) 
											(incf i) i (string (code-char i))))
					(incf i))) (values))

(defun ucs-codes-g (start row col &key (stream *standard-output*) y-spacing x-spacing) ;; graphics version
  (with-drawing-options (stream :ink +wheat4+ :text-size 22)
    (let ((x start))
      (formatting-table (stream :y-spacing y-spacing :x-spacing x-spacing)
        (do ((i 1 (1+ i)))
            ((> i row))
          (formatting-row (stream)
            (do ((j 1 (1+ j)))
                ((> j col))
              (formatting-cell (stream)
                (ucs-insert x)
                (incf x)))))))))


(defun ucs-codes-l (start row col &key (stream *standard-output*)) ;; listener-list version
	(with-drawing-options (stream :ink +wheat4+ :text-size 22)
												(let ((x start))
													(do ((i 1 (1+ i)))
														((> i row))
														(terpri)
														(do ((j 1 (1+ j)))
															((> j col))
															(format stream "~s " (string (code-char x)))
															(incf x))))))

(defun ucs-codes-ll (start row col &key (stream *standard-output*)) ;; listener-list version
	(with-drawing-options (stream :ink +wheat4+ :text-size 22)
												(let ((x start) (somechars nil))
													(do ((i 1 (1+ i)))
														((> i row))
														(do ((j 1 (1+ j)))
															((> j col))
															(setq somechars (append somechars (list (string (code-char x)))))
															(incf x))) somechars)))

(defun ucs-codes-t (start row col) ;; terminal version
	(let ((x start) (somechars nil))
		(do ((i 1 (1+ i)))
			((> i row))
			(terpri)
			(do ((j 1 (1+ j)))
				((> j col))
				(format t "~s " (string (code-char x)))
				(incf x)))))


(defun ucs-codes-tl (start row col) ;; terminal-list version
	(let ((x start) (somechars nil))
		(do ((i 1 (1+ i)))
			((> i row))
			(do ((j 1 (1+ j)))
				((> j col))
				(setq somechars (append somechars (list (string (code-char x)))))
				(incf x))) somechars))
