(defun example2 (&key (stream *standard-output*) y-spacing x-spacing)
  (formatting-table (stream :y-spacing y-spacing :x-spacing x-spacing)
    (dotimes (i 3)
      (formatting-row (stream)
	(dotimes (j 3)
	  (formatting-cell (stream)
	    (draw-rectangle* stream 10 10 50 50)))))))

(example2 :stream *standard-output* :y-spacing 5)
