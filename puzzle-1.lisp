(define-application-frame fifteen-puzzle-1 ()
                          ((pieces :initform (make-array '(4 4) 
                                                         :initial-contents '((1 2 3 4)
                                                                             (5 6 7 8)
                                                                             (9 10 11 12)
                                                                             (13 14 15 0)))))
                          (:menu-bar  nil)
                          (:panes
                           (display :application
                                    :text-style '(:fix :bold :very-large)
                                    :display-function 'draw-the-display
                                    :scroll-bars nil)
                           (menu :command-menu))
                          (:layouts
                           (main
                            (vertically () display menu))))

;;; this draws the entire display

(defmethod draw-the-display ((application fifteen-puzzle-1) stream
                             &key &allow-other-keys)
  (with-slots (pieces) application
    (dotimes (y 4)
      (dotimes (x 4)
        (let ((piece (aref pieces y x)))
          (if (zerop piece)
              (format stream "    ")
              (format stream "~2D " piece))))
      (terpri stream))))

;;; useful macrology - the body will be run with x and y bound to
;;; the coordinates of the empty cell

(defmacro find-empty-piece-and-do ((y x) &body body)
  `(block find-empty-piece
     (dotimes (,y 4)
       (dotimes (,x 4)
         (when (zerop (aref pieces ,y ,x))
           ,@body
           (return-from find-empty-piece))))))

(define-fifteen-puzzle-1-command (down :menu t) ()
                                 (with-slots (pieces) *application-frame*
                                   (find-empty-piece-and-do (y x)
                                                            (if (not (zerop y))
                                                                (rotatef (aref pieces y x) (aref pieces (- y 1) x))))))

(define-fifteen-puzzle-1-command (up :menu t) ()
                                 (with-slots (pieces) *application-frame*
                                   (find-empty-piece-and-do (y x)
                                                            (if (not (= y 3))
                                                                (rotatef (aref pieces y x) (aref pieces (+ y 1) x))))))

(define-fifteen-puzzle-1-command (left :menu t) ()
  (with-slots (pieces) *application-frame*
    (find-empty-piece-and-do (y x)
      (if (not (= x 3))
          (rotatef (aref pieces y x) (aref pieces y (+ x 1)))))))

(define-fifteen-puzzle-1-command (right :menu t) ()
                                 (with-slots (pieces) *application-frame*
                                   (find-empty-piece-and-do (y x)
                                                            (if (not (zerop x))
                                                                (rotatef (aref pieces y x) (aref pieces y (- x 1)))))))

#||
()
(setq fp1 (make-application-frame 'fifteen-puzzle-1
:left 200 :right 400 :top 150 :bottom 350))
(run-frame-top-level fp1)
||#
