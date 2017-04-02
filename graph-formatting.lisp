(define-application-frame graph-it ()
  ((root-node :initform (find-class 'clim:design) :initarg :root-node :accessor
     root-node)
    (app-stream :initform () :accessor app-stream))
  (:panes
    (display :application :display-function 'draw-display :display-after-commands
      :no-clear :scroll-bars :both 
      :background +black+ :foreground +white+))
  (:layouts
    (:defaults
      (horizontally ()
	display))))

(define-presentation-type node ())

(defun make-random-color ()
  (make-rgb-color (/ (+ 30 (random (- 255 30))) 255)
    (/ (+ 30 (random (- 255 30))) 255)
    (/ (+ 30 (random (- 255 30))) 255)))

(defun draw-node (object stream)
  (with-drawing-options (stream :ink (make-random-color))
    (with-output-as-presentation (stream object 'node)
      (surrounding-output-with-border (stream :shape :rectangle)
	(format stream "~a" (class-name object))))))

(defun graph-it (&optional (root-node (find-class (quote basic-sheet))) (port (find-port)))
  (if (atom root-node)
    (setf root-node (list root-node)))
  (let ((graph-it
	  (make-application-frame 'graph-it :frame-manager (find-frame-manager :port port) :width 1000 :height
	    800 :root-node root-node)))
    (run-frame-top-level graph-it)))

(defmethod draw-display ((frame graph-it) stream)
  (format-graph-from-roots (root-node frame) #'draw-node #'sb-mop:class-direct-subclasses
    :stream stream :arc-drawer
    (lambda (stream from-object to-object x1 y1 x2 y2 &rest drawing-options)
      (declare (dynamic-extent drawing-options))
      (declare (ignore from-object to-object))
      (apply #'draw-arrow* stream x1 y1 x2 y2 drawing-options)))
;;    :merge-duplicates t)
  (setf (app-stream frame) stream))

(defun border-test (stream)
  (fresh-line stream)
  (surrounding-output-with-border (stream :shape :rectangle)
    (format stream "this is some output with a rectangular border"))
  (terpri stream)
  (terpri stream)
  (surrounding-output-with-border (stream :shape :drop-shadow)
    (format stream "this has a drop-shadow under it"))
  (terpri stream)
  (terpri stream)
  (surrounding-output-with-border (stream :shape :underline)
    (format stream "and this output is underlined")))

(define-graph-it-command (com-exit :name "quit" :keystroke (#\q :control)) 
  () (frame-exit *application-frame*))

(define-application-frame graph-test ()
  ()
  (:command-definer define-graph-test-command)
  (:panes
    (interactor :interactor 
      :width 1000 :height 800 
      :max-width +fill+ :max-height +fill+ 
      :scroll-bars :both
      :background +black+ :foreground +white+ :line-thickness 2))
  (:layouts
    (default
      interactor)))

(define-graph-test-command (com-exit :name "exit" :keystroke (#\q :control)) 
  () (frame-exit *application-frame*))

(define-graph-test-command (com-foo :name "foofoo" :keystroke (#\f :control))
  ()
  (with-drawing-options (*standard-output* :text-size 20 :line-thickness 2)
    (let ((*print-case* :downcase))
      (format-graph-from-roots
	(list `(define-graph-test-command test ()
		 (let ((stream *standard-output*)
			(orientation :vertical))
		   (fresh-line stream)
		   (macrolet ((make-node (&key name children)
				`(list* ,name ,children)))
		     (flet ((node-name (node)
			      (car node))
			     (node-children (node)
			       (cdr node)))
		       (let* ((2a (make-node :name "2A"))
			       (2b (make-node :name "2B"))
			       (2c (make-node :name "2C"))
			       (1a (make-node :name "1A" :children (list 2a 2b)))
			       (1b (make-node :name "1B" :children (list 2b 2c)))
			       (root (make-node :name "0" :children (list 1a 1b))))
			 (format-graph-from-roots
			   (list root)
			   (lambda (node s)
			     (write-string (node-name node) s))
			   #'node-children
			   :orientation orientation
			   :stream stream)))))))
	(lambda (x s) 
	  (with-drawing-options (t :ink (make-random-color))
	    (with-output-as-presentation (s x 'command)
	      (let ((*print-level* 1))
		(princ (if (consp x) (car x) x) s)))))
	(lambda (x) (and (consp x) (cdr x)))
	:arc-drawer #'climi::arrow-arc-drawer
	:arc-drawing-options (list :ink +red+ :line-thickness 2)
	:stream *standard-output*
	:orientation :vertical))))

(define-graph-test-command (com-foo :name "foo" :keystroke (#\F :control))
  ()
  (with-drawing-options (*standard-output* :text-size 20 :line-thickness 2)
    (let ((*print-case* :downcase))
      (with-room-for-graphics (t)
	(with-translation (t -3500 0)
	  (format-graph-from-roots
	    (list `(define-graph-test-command test ()
		     (let ((stream *standard-output*)
			    (orientation :horizontal))
		       (fresh-line stream)
		       (macrolet ((make-node (&key name children)
				    `(list* ,name ,children)))
			 (flet ((node-name (node)
				  (car node))
				 (node-children (node)
				   (cdr node)))
			   (let* ((2a (make-node :name "2A"))
				   (2b (make-node :name "2B"))
				   (2c (make-node :name "2C"))
				   (1a (make-node :name "1A" :children (list 2a 2b)))
				   (1b (make-node :name "1B" :children (list 2b 2c)))
				   (root (make-node :name "0" :children (list 1a 1b))))
			     (format-graph-from-roots
			       (list root)
			       (lambda (node s)
				 (write-string (node-name node) s))
			       #'node-children
			       :orientation orientation
			       :stream stream)))))))
	    (lambda (x s) (with-output-as-presentation (s x 'command)
			    (with-drawing-options (t :ink (make-random-color))
			      (let ((*print-level* 1))
				(princ (if (consp x) (car x) x) s)))))
	    (lambda (x) (and (consp x) (cdr x)))
	    :stream *standard-output*
	    :orientation :horizontal
	    :generation-separation '(1 :character)
	    :within-generation-separation '(1 :character)
	    :arc-drawer #'climi::arrow-arc-drawer
	    :arc-drawing-options (list :ink +red+ :line-thickness 2)))))))

(defun external-symbol-p (sym)
  ;; *cough* *cough*
  (< (count #\: (let ((*package* (find-package :keyword)))
                  (prin1-to-string sym)))
    2))

(define-graph-test-command (com-bar :name "bar" :keystroke (#\B :control))
  ()
  (with-room-for-graphics (t)
    (with-translation (t -400 0)
      (with-drawing-options (*standard-output* :text-size 12)
	(let ((*print-case* :downcase))
	  (format-graph-from-roots
	    (list (find-class 'climi::basic-output-record))
	    (lambda (x s)
	      (progn (surrounding-output-with-border (s :shape :oval)
		       (with-drawing-options (t :ink (and +black+ (make-random-color)))
			 (with-text-style (s (make-text-style nil
					       (if (external-symbol-p (class-name x))
						 :bold
						 nil)
					       16))
			   (prin1 (class-name x) s))))))
	    (lambda (x)
	      (clim-mop:class-direct-subclasses x))
	    :generation-separation '(1 :character)
	    :within-generation-separation '(1 :character)
	    :merge-duplicates 't
	    :graph-type :tree
	    :stream *standard-output*
	    :orientation :horizontal
	    :arc-drawer #'climi::arrow-arc-drawer
	    :arc-drawing-options (list :ink +red+ :line-thickness 2)))))))

(define-graph-test-command (com-baz :name "baz" :keystroke (#\b :control))
  ()
  (with-drawing-options (*standard-output* :text-size 20)
    (let ((*print-case* :downcase))
      (format-graph-from-roots
	(list (find-class 'climi::standard-graph-output-record)
	  (find-class 'climi::basic-output-record)
	  (find-class 'climi::graph-output-record)
	  
	  )
	(lambda (x s)
	  (with-drawing-options (t :ink (make-random-color))
	    (with-text-style (s (make-text-style nil
				  (if (external-symbol-p (class-name x))
				    :bold
				    nil)
				  20))
	      (prin1 (class-name x) s))))
	(lambda (x)
	  (reverse (clim-mop:class-direct-superclasses x)))
	;; :duplicate-key #'(lambda (x) 't)
	:merge-duplicates t
	:graph-type :tree
	:arc-drawer #'climi::arrow-arc-drawer
	:arc-drawing-options (list :ink +red+ :line-thickness 2)
	:stream *standard-output*
	:orientation :vertical))))

(define-graph-test-command (com-foobar :name "foobar" :keystroke (#\r :control))
  ()
  (with-drawing-options (*standard-output* :text-size 20)
    (format-graph-from-roots
      (list '(:foo
	       (:bar)
	       (:baaaaaaaaaaaaaaz
		 (:a)
		 (:b))
	       (:q
		 (:x) (:y)))
	)
      (lambda (x s)
	(with-drawing-options (t :ink (make-random-color))
	  (prin1 (first x) s)))
      (lambda (x)
	(cdr x))
      :generation-separation '(4 :line)
      :within-generation-separation '(2 :character)
      :stream *standard-output*
      :orientation :vertical
      :arc-drawer #'climi::arrow-arc-drawer
      :arc-drawing-options (list :ink +red+ :line-thickness 2))))

(define-graph-test-command (com-test :name "test" :keystroke (#\p :control))
  ()
  (let ((stream *standard-output*)
	 (orientation :vertical))
    (with-drawing-options (stream :text-size 20)
      (fresh-line stream)
      (macrolet ((make-node (&key name children)
		   `(list* ,name ,children)))
	(flet ((node-name (node)
		 (car node))
		(node-children (node)
		  (cdr node)))
	  (let* ((2a (make-node :name "2A"))
		  (2b (make-node :name "2B"))
		  (2c (make-node :name "2C"))
		  (1a (make-node :name "1A" :children (list 2a 2b)))
		  (1b (make-node :name "1B" :children (list 2b 2c)))
		  (root (make-node :name "0" :children (list 1a 1b))))
	    (format-graph-from-roots
	      (list root)
	      (lambda (node s)
		(with-drawing-options (t :ink (make-random-color))
		  (write-string (node-name node) s)))
	      #'node-children
	      :arc-drawer #'climi::arrow-arc-drawer
	      :arc-drawing-options (list :ink +red+ :line-thickness 2)
	      :orientation orientation
	      :stream stream)))))))

(defun make-circ-list (list)
  (nconc list list))

(define-graph-test-command (com-test2 :name "test2" :keystroke (#\t :control))
  ()
  (let ((stream *standard-output*)
	 (orientation :vertical))
    (with-drawing-options (stream :text-size 20)
      (fresh-line stream)
      (format-graph-from-roots
	(list '(defun dcons (x) (cons x x))
	  (make-circ-list (list 1 '(2 . 4) 3)))
	(lambda (node s)
	  (with-drawing-options (t :ink (make-random-color))
	    (if (consp node)
	      (progn
		(draw-circle* s 5 5 5 :filled nil))
	      (princ node s))))
	(lambda (x) (if (consp x) (list (car x) (cdr x))))
	:cutoff-depth nil
	:graph-type :tree
	:merge-duplicates t
	:arc-drawer #'climi::arrow-arc-drawer
	:arc-drawing-options (list :ink +red+ :line-thickness 2)
	:orientation orientation
	:stream stream))))

(defun run-graph-test ()
  (run-frame-top-level	(make-application-frame 'graph-test)))

;; this one you can use like (run-graph-it 'clim:dialog) or
;; like (run-graph-it 'drei:view)...etc.
(defun run-graph-it (root-node)
  (graph-it (find-class root-node)))
