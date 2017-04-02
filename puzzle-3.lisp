;;; things to fix - replace encoded position
;;; - auto-size window , get line-height, char-width

(define-application-frame fifteen-puzzle-3 ()
  ((pieces :initform (make-array '(4 4) :initial-contents '((1 2 3 4)
							     (5 6 7 8)
							     (9 10 11 12)
							     (13 14 15 0))))
    (presentations :initform (make-array '(4 4)))
    (char-width :initform 12)
    (line-height :initform 30))
  (:panes
    (display :application
      :text-style '(:fix :bold :very-large)
      :display-function 'draw-the-display
      :display-after-commands t
      :incremental-redisplay nil
      :scroll-bars nil
      :initial-cursor-visibility nil))
  (:layouts
    (main
      (vertically () display))))

(define-presentation-type puzzle-piece ())

(defmethod draw-piece ((application fifteen-puzzle-3)
			piece position-y position-x stream)
  (with-slots (char-width line-height presentations) application
    (stream-set-cursor-position stream (* position-x 3 char-width)
      (* position-y line-height))
    (when (aref presentations position-y position-x)
      (erase-output-record (aref presentations position-y position-x) stream))
    (setf (aref presentations position-y position-x)
      (let ((position (+ (* position-y 4) position-x)))
	(write-string " " stream)
	(with-output-as-presentation (stream position 'puzzle-piece)
	  (if (zerop piece)
	    (format stream "   ")
	    (format stream "~2D" piece)))))))

(defmethod draw-the-display ((application fifteen-puzzle-3) stream &key &allow-other-keys)
  (with-slots (pieces) application
    (dotimes (y 4)
      (dotimes (x 4)
	(draw-piece application (aref pieces y x) y x stream)))))

(defun which-way-to-move (yp xp pieces)
  (macrolet ((is-space (y x) `(zerop (aref pieces ,y ,x))))
    (loop for x from (+ xp 1) to 3 do
      (when (is-space yp x)
	(return-from which-way-to-move (values yp x 0 -1 (- x xp)))))
    (loop for x from (- xp 1) downto 0 do
      (when (is-space yp x)
	(return-from which-way-to-move (values yp x 0 1 (- xp x)))))
    (loop for y from (+ yp 1) to 3 do
      (when (is-space y xp)
	(return-from which-way-to-move (values y xp -1 0 (- y yp)))))
    (loop for y from (- yp 1) downto 0 do
      (when (is-space y xp)
	(return-from which-way-to-move (values y xp 1 0 (- yp y)))))))

(define-fifteen-puzzle-3-command (move) ((yp 'integer) (xp 'integer))
  (with-slots (pieces) *application-frame*
    (let ((display-pane (get-frame-pane *application-frame* 'display)))
      (flet ((update (y x new-piece)
	       (setf (aref pieces y x) new-piece)
	       (draw-piece *application-frame* new-piece y x display-pane)))
	(multiple-value-bind (start-y start-x dy dx n-moves)
	  (which-way-to-move yp xp pieces)
	  (when dx
	    (loop repeat n-moves
	      for x1 = start-x then x2
	      for x2 = (+ x1 dx) then (+ x2 dx)
	      for y1 = start-y then y2
	      for y2 = (+ y1 dy) then (+ y2 dy)
	      do (update y1 x1 (aref pieces y2 x2))
	      finally (update yp xp 0))))))))


(define-presentation-to-command-translator move-a-piece
  (puzzle-piece move fifteen-puzzle-3)
  (object)
  (multiple-value-bind (yp xp) (floor object 4)
    (list yp xp)))

(define-fifteen-puzzle-3-command (reset :menu t) ()
  (with-slots (pieces presentations) *application-frame*
    (loop for y from 0 to 3 do
      (loop with 4y+1 = (+ (* 4 y) 1)
	for x from 0 to 3 do
	(setf (aref pieces y x) (mod (+ 4y+1 x) 16))))
    (let ((display-pane (get-frame-pane *application-frame* 'display)))
      (window-clear display-pane)
      (dotimes (y 4)
	(dotimes (x 4)
	  (setf (aref presentations y x) nil)))
      (draw-the-display *application-frame* display-pane))))

(define-fifteen-puzzle-3-command (exit :menu t) ()
  (frame-exit *application-frame*))

#||
()
(setq fp3 (make-application-frame 'fifteen-puzzle-3
	    :left 400 :right 600 :top 150 :bottom 350))
(run-frame-top-level fp3)
||#
