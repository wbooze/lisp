(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :vecto))

(defpackage :sb-logo
  (:use :cl :vecto))

(in-package :sb-logo)

(defvar *green* '(0 224/255 0))
(defvar *yellow* '(221/255 221/255 34/255))
(defvar *gray* '(85/255 85/255 85/255))
(defvar *read* '(170/255 0 0))

(defun set-color (color)
  (apply #'set-rgb-fill (mapcar #'float color)))

(defun make-sbcl-log (file)
  (with-canvas (:width 64 :height 64)
    (rectangle 1/2 1/2 63 63)
    (set-rgb-fill 1 1 1)
    (fill-and-stroke)
    (rectangle 1 1 30 30)
    (set-color *gray*)
    (fill-path)
    (rectangle 1 33 30 30)
    (set-color *green*)
    (fill-path)
    (rectangle 33 33 30 30)
    (set-color *yellow*)
    (fill-path)
    (rectangle 33 1 30 30)
    (set-color *read*)
    (fill-path)
    (save-png file)))
