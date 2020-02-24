(in-package :common-lisp-user)

(defpackage :app
  (:use :clim :clim-lisp)
  (:export run-app))

(in-package :app)

(define-application-frame superapp () 
                          ((current-number :initform nil
                                           :accessor current-number))
                          (:pointer-documentation t)
                          (:panes
                           (app :application
                                :height 400
                                :width 600
                                :display-function 'display-app)
                           (int :interactor
                                :height 200
                                :width 600))
                          (:layouts
                           (default
                            (vertically ()
                                        app
                                        int))))

(defun display-app (frame pane)
  (let ((number (current-number frame)))
  (format pane "~a is ~a" number
          (cond ((null number) "not a number")
                ((oddp number) "odd")
                (t "even")))))


(define-superapp-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-superapp-command (com-parity :name t) ((number 'integer))
  (setf (current-number *application-frame*) number))


(defun run-app ()
  (run-frame-top-level (make-application-frame 'superapp)))
