(define-application-frame draw-frame ()
  ((lines :accessor lines :initform nil)
    (strings :accessor strings :initform nil))
  (:panes (draw-pane (make-pane 'draw-pane :min-height 600 :min-width 1200))
    (interactor :interactor :min-height 200 :min-width 1200))
  (:layouts (default-default (vertically ()
			       draw-pane
			       interactor)))
  (:menu-bar t)
  (:command-definer t)
  (:top-level (default-frame-top-level)))

(defclass draw-pane (standard-extended-input-stream basic-pane permanent-medium-sheet-output-mixin) ())

(defmethod handle-repaint ((pane draw-pane) region)
  (with-application-frame (frame)
    (call-next-method)
    (dolist (line (lines frame))
      (draw-line pane (car line) (cdr line)))
    (dolist (pair (strings frame))
      draw-text pane (cdr pair) (car pair))))

(define-draw-frame-command (com-draw-add-string :menu t :name t)
  ((string 'string) (x 'integer) (y 'integer))
  (with-slots (strings) *application-frame*
    (push (cons (make-point x y) string) strings) (update-draw-pane)))

(define-draw-frame-command (com-draw-add-line :menu t :name t)
  ((x1 'integer) (y1 'integer) (x2 'integer) (y2 'integer))
  (with-slots (lines) *application-frame*
    (push (cons (make-point x1 y1) (make-point x2 y2))
      lines)) (update-draw-pane))

(define-draw-frame-command (com-draw-clear :menu t :name t) ()
  (with-slots (lines strings) *application-frame*
    (setf lines nil strings nil)) (update-draw-pane))

(defun update-draw-pane ()
  (let ((frame *application-frame*))
    (with-application-frame (frame)
      (repaint-sheet (find-pane-named *application-frame* 'draw-pane) +everywhere+))))

(defmethod show-package-info (stream)
  (dolist (package (list-all-packages))
    (write-string (package-name package) stream)
    (write-string " " stream)
    (format stream "~D" (count-package-symbols package))
    (terpri stream)))


(defun track-line-drawing (pane startx starty)
  (let ((lastx startx)
	 (lasty starty))
    (with-drawing-options (pane :ink +black+)
      (draw-line* pane startx starty lastx lasty)
      (tracking-pointer (pane)
	(:pointer-motion (&key window x y)
	  (draw-line* pane startx starty lastx lasty) ;delete old
	  (draw-line* pane startx starty x y) ;draw-new
	  (setq lastx x lasty y))
	(:pointer-button-release (&key event x y)
	  (draw-line* pane startx starty lastx lasty)
	  (execute-frame-command *application-frame* `(com-draw-add-line ,startx ,starty ,x ,y))
	  (return-from track-line-drawing nil))))))

(defmethod handle-event ((pane draw-pane) (event pointer-button-press-event))
  ;;start line tracking when left pointer button is pressed
  (when (eql (pointer-event-button event) +pointer-left-button+)
    (track-line-drawing pane
      (pointer-event-x event)
      (pointer-event-y event))))

(defun track-text-drawing (pane current-string current-x current-y)
  (with-drawing-options (pane :ink +black+)
    (draw-text* pane current-string current-x current-y)
    (tracking-pointer (pane)
      (:pointer-motion (&key window x y)
	;; we can't use flipping ink for text, hence redraw
	(handle-repaint pane +everywhere+)
	(setq current-x x current-y y)
	(draw-text* pane current-string current-x current-y))
      (:keyboard (&key gesture)
	(when (and (typep gesture 'key-release-event)
		(keyboard-event-character gesture))
	  (setf current-string (concatenate 'string current-string (string (keyboard-event-character gesture))))
	  (draw-text* pane current-string current-x current-y)))
      (:pointer-button-release (&key event x y)
	(draw-text* pane current-string current-x current-y)
	(when (eql (pointer-event-button event) +pointer-left-button+)
	  (execute-frame-command *application-frame* `(com-draw-add-string ,current-string ,x ,y))
	  (return-from track-text-drawing nil))))))


(defmethod handle-event ((pane draw-pane) (event pointer-button-press-event))
  ;; start with empty stirng, as a key release event will be received anyway
  (when (eql (pointer-event-button event) +pointer-left-button+)
    (multiple-value-bind (x y) (values (pointer-event-x event) (pointer-event-y event))
      (track-text-drawing pane "" x y))) (update-draw-pane))


(run-frame-top-level (make-application-frame 'draw-frame))
