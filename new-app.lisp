(in-package :common-lisp-user)

(defpackage :app
  (:use :clim :clim-lisp)
  (:export run-app))

(in-package :app)

(define-application-frame superapp () 
                          ()
                          (:pointer-documentation t)
                          (:panes
                           (app :application
                                :display-time nil
                                :height 400
                                :width 600)
                           (int :interactor
                                :height 200
                                :width 600))
                          (:layouts
                           (default
                            (vertically ()
                                        app
                                        int))))

(define-superapp-command (com-parity :name t) ((number 'integer))
                         (format t "~a is ~a~%" number
                                 (if (oddp number)
                                     "odd"
                                     "even")))


(defun run-app ()
  (run-frame-top-level (make-application-frame 'superapp)))