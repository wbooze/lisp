(defpackage :cube-modeller (:use :clim-lisp :LED-cube))

(in-package :cube-modeller)

(defvar *animation-pathname* nil)

(defmacro make-button (name label function)
  `(make-pane 'push-button-pane
              :name ',(read-from-string (format nil "~s-button" name))
              :label ,label
              :activate-callback (lambda (&rest rest)
                                   (declare (ignore rest))
                                   ,function)))

(defmacro make-move-button (name &optional key)
  `(make-button ,name
                ;; ,(string-downcase (symbol-name name))
                ,(format nil "~a~@[ (~(~s~))~]" (command-name-from-symbol name) key)
                (,(read-from-string (format nil "move-~s" name)))))

;; (defmacro make-color-button (color)
;;   `(make-button ,(read-from-string (format nil "set-intensity-to-~s" color))
;;                 ,(format nil "~s" color)
;;                 (set-current-intensity ,color :update-color-selector nil))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun button-name-for-color (color)
    (let ((*package* #.*package*))
      (read-from-string (format nil "set-intensity-to-~a-button" color)))))

(defmacro make-color-button (color)
  `(make-pane 'toggle-button
              :name ',(button-name-for-color color)
              :label ,(format nil "~s" color)
              :indicator-type :one-of
              :value-changed-callback (lambda (&rest rest)
                                        (declare (ignore rest))
                                        (set-current-intensity ,color :update-color-selector nil))))

(defmacro make-dark-mode-button (dark-mode)
  `(make-pane 'toggle-button
              :label ,(if dark-mode "Night mode  " "Day mode")
              :indicator-type :one-of
              :value-changed-callback (lambda (&rest rest)
                                        (declare (ignore rest))
                                        (setf *dark-mode* ,dark-mode))))

(defmacro make-goto-cube-button (name &optional key)
  `(make-button ,name
                ;; ,(string-downcase (symbol-name name))
                ,(format nil "~a~@[ (~(~s~))~]" (command-name-from-symbol name) key)
                (goto-cube ,(read-from-string (format nil ":~s" name)))))

(defmacro make-command-button (name command &optional key)
  `(make-button ,name
                ,(format nil "~a~@[ (~(~s~))~]" (command-name-from-symbol name) key)
                (throw-command ,command)))


;;; COMMAND-TABLES

(define-command-table file-command-table)
(define-command-table configuration-command-table)
(define-command-table help-command-table)

;;; APPLICATION-FRAME

(define-application-frame toolbox ()
  ()
  (:command-table (toolbox
                   :inherit-from (file-command-table configuration-command-table help-command-table)
                   :menu (("File" :menu file-command-table)
                          ("Configuration" :menu configuration-command-table)
                          ("Help" :menu  help-command-table))))
  (:menu-bar t #+nil (("About" :command com-about)
                      ("Configure Networking" :command com-configure-networking)
                      ("Quit" :command com-exit)))
  (:panes
   (cube-slider :slider
                :id 'cube-slider-gadget
                :min-value 0
                :max-value 0
                :value 0
                ;; :show-value-p t
                :orientation :horizontal
                :current-color +black+)
   (cube-number-pane :application-pane ;; this was just a workaround to make the menus work
                     :min-height 12
                     :background climi::*3d-normal-color*
;;                     :incremental-redisplay t
                     :double-buffering t
                     :display-function
                     (lambda (frame pane)
                       (declare (ignore frame))
                       (write-string-centered (format nil "Frame ~d of ~d" (1+ *current-cube-number*)
                                                                          (length *animation*))
                                              pane)))
   (transformations-pane (make-pane 'option-pane
                                    :name 'transformations-pane
                                    :width 200
                                    :items (reverse *transformations*)
                                    :name-key #'command-name-from-symbol
                                    :value (car (reverse *transformations*)))))
  (:layouts
   (defaults (vertically ()
               (labelling (:label "Show on the real cube via network")
                   (horizontally ()
                     +fill+
                     (vertically ()
                       (horizontally ()
                         (75 (make-command-button go-online 'com-go-online))
                         (75 (make-command-button go-offline 'com-go-offline)))
                       (make-command-button upload-animation 'com-upload-animation))
                     +fill+))
 
;;                (labelling (:label "Files")
;;                  (horizontally ()
;;                    +fill+
;;                    (make-command-button load-animation 'com-load-animation-file #+nil l)
;;                    (make-command-button save-animation 'com-save-animation-file #+nil s)
;;                    +fill+))
                                                                                            
;;                (labelling (:label "Playing")
;;                  (horizontally ()
;;                    +fill+
;;                    (make-button toggle-play-animation "Play or Stop (p)" (toggle-animation-running-state))
;;                    +fill+))
               
               (labelling (:label "General")
                 (vertically ()
                   (horizontally ()
                     +fill+
                     (with-radio-box (:orientation :horizontal)
                       (radio-box-current-selection (make-dark-mode-button t))
                       (make-dark-mode-button nil))
                     +fill+)
                  12
                   (horizontally ()
                     +fill+
                     (make-button undo "Undo (u)" (undo))
                     +fill+
                   (make-button reset-perspective "Reset display (r)" (reset-perspective))
                   +fill+)))

               (labelling (:label "Navigation")
                 (vertically ()
                   cube-number-pane
                   cube-slider
                   (horizontally ()
                     +fill+
                     (248 (make-button toggle-play-animation "Play or Stop (p)" (toggle-animation-running-state)))
                     +fill+)
                   12
                   (horizontally ()
                     +fill+
                     (vertically ()
                       (make-goto-cube-button first home)
                       (make-goto-cube-button last end))
                     +fill+
                     (vertically ()
                       (make-goto-cube-button next pg-up)
                       (make-goto-cube-button previous pg-down))
                     +fill+)))

               (labelling (:label "Insert frame copy...")
                 (horizontally ()
                   +fill+
                   (make-button insert-before "Before current (b)" (insert-cube-copy :position :before))
                   (make-button insert-after "After current (a)" (insert-cube-copy))
                   +fill+))

               (labelling (:label "Delete...")
                 (horizontally ()
                   +fill+
                   (make-button delete-cube "Delete current frame (d)" (delete-cube))
                   (make-button delete-animation "New animation (n)" (delete-animation))
                   +fill+))
               
               (labelling (:label "Select shade of red")
                 (horizontally ()
                   +fill+
                   (with-radio-box (:orientation :horizontal :name 'color-box)
                     (make-color-button 0)
                     (make-color-button 1)
                     (make-color-button 2)
                     (make-color-button 3)
                     (make-color-button 4)
                     (make-color-button 5))
                   +fill+))

               (labelling (:label "Change all LEDs of the current frame")
                 (horizontally ()
                     +fill+
                     (make-button delete-cube "Set to current color (f)" (fill-cube))
                     (make-button delete-cube "Switch them off (e)" (empty-cube))
                     +fill+))

               (labelling (:label "Move the current frame")
                 (horizontally ()
                   +fill+
                   (vertically ()
                     +fill+
                     (make-move-button up s)
                     (make-move-button down x)
                     +fill+)
                   +fill+
                   (vertically ()
                     (make-move-button forward up)
                     (horizontally ()
                       (60 (make-move-button left))
                       (60 (make-move-button right)))
                     (make-move-button backward down))
                   +fill+))
               
               (labelling (:label "Effects")
                 (horizontally ()
                   +fill+
                   (vertically ()
                     (horizontally ()
                       transformations-pane
                       (make-button 'transformate "Apply" (apply-user-transformation)))
                     (make-command-button load-user-effects 'com-load-user-transformations))
                   +fill+))
       
               ))))

(defmethod run-frame-top-level :before ((frame toolbox) &key)
  (setf *toolbox-frame* frame)
  (setf *dark-mode* t)
  (adjust-cube-slider-for-animation)
  (update-color-selector)
  (ignore-errors
    (com-load-user-transformations :ask-for-filename-p nil
                                   :show-load-messages-p nil)))

(defmethod run-frame-top-level :after ((frame toolbox) &key)
  (declare (ignore frame))
  (setf *toolbox-frame* nil))

(define-application-starter toolbox
    :function-name start-toolbox
    :default-process-name "cube-modeller toolbox"
    :complain-about-existing-process t)


;;; COMMANDS

(define-command (com-new-animation :name t :menu t
                                   :command-table file-command-table
                                   :keystroke (#\n :meta))
    ()
  (delete-animation))
  
(define-command (com-load-animation-file :name t :menu t
                                         :command-table file-command-table
                                         :keystroke (#\l :meta))
    ()
  (load-animation-file
   (setf *animation-pathname*
         (file-selector:select-file :own-window t
                                    :pathname (or *animation-pathname*
                                                  *default-pathname*))))
  (adjust-cube-slider-for-animation))

(define-command (com-save-animation-file :name t :menu t
                                         :command-table file-command-table
                                         :keystroke (#\s :meta))
    ()
  (save-animation-file
   (setf *animation-pathname*
         (file-selector:select-file :own-window t
                                    :pathname (or *animation-pathname*
                                                  *default-pathname*)))
   :if-exists :overwrite))

(define-command (com-exit :name "Quit" #+nil "Quit (Alt-q)" :menu t
                          :command-table file-command-table
                          :keystroke (#\q :meta))
    ()
  (frame-exit *application-frame*))

(define-toolbox-command (com-go-online :name t)
    ()
  (start-networking))

(define-toolbox-command (com-go-offline :name t)
    ()
  (stop-networking))

(define-toolbox-command (com-upload-animation :name t)
    ()
  (let ((author "Anonymous")
        (title "unnamed"))
    (accepting-values (stream :own-window t); :align-prompts t)
      (scroll-clim-stream stream :y-delta 20 :x-delta 20)
      (with-text-size (stream 15)
        (format stream "Upload an animation to the real cube"))
      (scroll-clim-stream stream :y-delta 25)
      (fresh-line stream)
      (setf author (clim:accept 'string :default author :prompt "Author" :stream stream))
      (fresh-line stream)
      (setf title (clim:accept 'string :default title :prompt "Title" :stream stream)))
  (network-upload-animation :author author :title title)))

;; (define-toolbox-command (com-set-server-address :name t :menu t)
;;     ((name-or-ip 'string :prompt "name or ip" :default "localhost")
;;      (port 'integer :prompt "port" :default *default-server-port*))
;;   (set-server-address name-or-ip port))

(define-command (com-configure-networking :name t :menu t
                                          :command-table configuration-command-table)
    ()
  (let ((name-or-ip (or (get-server-name-or-ip) *default-server-name*))
        (port (or (get-server-port) *default-server-port*)))
    (accepting-values (stream :own-window t); :align-prompts t)
      (scroll-clim-stream stream :y-delta 20 :x-delta 20)
      (with-text-size (stream 15)
        (format stream "Configure Networking"))
      (scroll-clim-stream stream :y-delta 25)
      (fresh-line stream)
      (setf name-or-ip (clim:accept 'string :default name-or-ip :prompt "Name or IP" :stream stream))
      (fresh-line stream)
      (setf port (clim:accept 'integer :default port :prompt "Port" :stream stream)))
    (set-server-address name-or-ip port)))

(define-command (com-configure-serial-device :name t :menu t
                                             :command-table configuration-command-table)
    ()
  (let ((device-file (namestring (or *device-file* ""))))
    (accepting-values (stream :own-window t); :align-prompts t)
      (scroll-clim-stream stream :y-delta 20 :x-delta 20)
      (with-text-size (stream 15)
        (format stream "Configure Serial Device"))
      (scroll-clim-stream stream :y-delta 25)
      (fresh-line stream)
      (setf device-file (clim:accept 'string :default device-file :prompt "file path" :stream stream)))
    (when (string= device-file "")
      (setf device-file nil))
    (when device-file
      (setf device-file (pathname device-file)))
    (set-device-file device-file)))

(define-toolbox-command (com-load-user-transformations :name t)
    (&key
     (ask-for-filename-p 'boolean :default t)
     (show-load-messages-p 'boolean :default t))
  (let* ((directory (pathname-directory (or *animation-pathname*
                                            *default-pathname*)))
         (pathname (make-pathname :name "user-effects"
                                  :type "lisp"
                                  :directory directory)))
    (flet ((do-load (pathname stream)
             (if (probe-file pathname)
                 (progn
                   (format stream "Loading file \"~a\"...~%~%" pathname)
                   (let ((*package* (find-package 'LED-cube)))
                     (load pathname)
                     (terpri stream))
                   (update-transformations-pane))
                 (format stream "There is no file \"~a\".~%~%" pathname))))

      (when ask-for-filename-p
        (setf pathname (file-selector:select-file :own-window t
                                                  :pathname pathname)))

      (if show-load-messages-p
          (accepting-values (stream :own-window t :label "Messages for Load User Effects")
            (let ((*error-output* stream)
                  (*query-io* stream)
                  (*trace-output* stream))
              (do-load pathname stream)))
          (do-load pathname *standard-output*)))))

(defun apply-user-transformation ()
  (funcall (gadget-value (find-pane-named *application-frame*
                                          'transformations-pane)))
  (adjust-cube-slider-for-animation)
  (write-cube-to-device))

;;; CALLBACKS

(defmethod value-changed-callback ((gadget slider-pane) (client toolbox) (gadget-id (eql 'cube-slider-gadget)) value)
  (declare (ignore gadget client gadget-id))
  (goto-cube (round value))
  ;; NEEDS-REDISPLAY was not enough
  ;; (setf (pane-needs-redisplay (find-pane-named *application-frame* 'cube-number-pane)) t)
  ;; (redisplay-frame-pane *application-frame* (find-pane-named *application-frame* 'cube-number-pane))
  (queue-toolbox-event *application-frame*
                       (make-instance 'redisplay-event 
                                      :sheet *application-frame*
                                      :pane (find-pane-named *application-frame* 'cube-number-pane))))


(defmethod drag-callback ((pane slider-pane) (client toolbox) (gadget-id (eql 'cube-slider-gadget)) value)
  (value-changed-callback pane client gadget-id value))


;;; setters for the cube-slider

(defun set-cube-slider-value (&optional (value *current-cube-number*))
  (setf (gadget-value (find-pane-named *application-frame* 'cube-slider)) ;; :invoke-callback t)
        value)
  ;; The redisplay code has to be copied here; we cannot use :invoke-callback t anymore, as
  ;; then there is a infinitive loop (GOTO-CUBE calls SET-CUBE-SLIDER, this triggers the
  ;; VALUE-CHANGED-CALLBACK that invokes GOTO-CUBE...).
    
  ;; NEEDS-REDISPLAY was not enough
  ;; (setf (pane-needs-redisplay (find-pane-named *application-frame* 'cube-number-pane)) t)
  ;; (redisplay-frame-pane *application-frame* (find-pane-named *application-frame* 'cube-number-pane))
  (queue-toolbox-event *application-frame*
                       (make-instance 'redisplay-event 
                                      :sheet *application-frame*
                                      :pane (find-pane-named *application-frame* 'cube-number-pane)))
  ;; the next is simply too much
  #+nil (queue-toolbox-event *application-frame*
                             (make-instance 'redisplay-event 
                                            :sheet *application-frame*
                                            :pane (find-pane-named *application-frame* 'cube-slider))))

(defun adjust-cube-slider-for-animation (&key (animation *animation*))
  (let ((value (length animation)))
    (setf (gadget-max-value (find-pane-named *application-frame* 'cube-slider))
          (1- value)
          
          (slot-value (find-pane-named *application-frame* 'cube-slider) 'climi::number-of-quanta)
          value))
  (set-cube-slider-value *current-cube-number*))

(defun update-color-selector (&optional (intensity *current-intensity*))
  (let ((pane (find-pane-named *application-frame* (button-name-for-color intensity))))
    (setf (gadget-value pane :invoke-callback t) t))
  (queue-toolbox-event *application-frame*
                       (make-instance 'redisplay-event
                                      :sheet *application-frame*
                                      :pane (find-pane-named *application-frame* 'color-box))))

;; update transformations-pane

(defun update-transformations-pane ()
  (let ((pane (find-pane-named *application-frame* 'transformations-pane)))
    (setf (slot-value pane 'climi::items) (reverse *transformations*))
  (queue-toolbox-event *application-frame*
                       (make-instance 'redisplay-event
                                      :sheet *application-frame*
                                      :pane pane))))


;;; event handling methods

(defmethod handle-event ((frame toolbox) (event redisplay-event))
  (let ((next-event (and (processes-supported-p)
                         (event-peek (frame-top-level-sheet frame))))
        (pane (event-pane event)))
    (unless (typep next-event 'redisplay-event)
      (setf (pane-needs-redisplay pane)
            t)
      (redisplay-frame-pane frame pane)
      ;; might be the case that even the following is necessary:
      ;; (redisplay-frame-panes frame)
      (medium-force-output (sheet-medium pane)))))
