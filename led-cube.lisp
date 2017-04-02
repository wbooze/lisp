(defpackage :LED-cube (:use :clim-lisp))
(in-package :LED-cube)

(defvar *device-file* nil #+nil #p"/dev/ttyUSB0")
(defvar *device-lock* (clim-sys:make-lock "LED-cube device lock"))
(defvar *device-stream* nil)
(defvar *cube-dimension* 5)

(defvar *default-server-name* "cube.entropia.de")
(defvar *default-server-port* 5282)
(defvar *server-address* nil) ;;  (list #(127 0 0 1) *default-server-port* *default-server-name*))
(defvar *socket* nil)

(defvar *current-cube-number* 0)
(defvar *current-intensity* #\5)
(defvar *default-idle-loop-delay* 0.000001)
(defvar *default-frame-duration* 0.08)
(defvar *frame-duration* *default-frame-duration*)

(defvar *undo-cache* nil)
(defvar *undo-depth* 30)
(defvar *save-undo-enabled* t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; intensity functions

(defun ensure-intensity-to-be-of-character (intensity)
  (if (characterp intensity)
      intensity
      (code-char (+ intensity (char-code #\0)))))

(defun ensure-intensity-to-be-of-number (intensity)
  (if (numberp intensity)
      intensity
      (- (char-code intensity) (char-code #\0))))

(defun set-current-intensity (intensity)
  (setf *current-intensity*
        (ensure-intensity-to-be-of-character intensity)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tools

(defun make-cube (&key (intensity #\0))
  (make-array (list *cube-dimension* *cube-dimension* *cube-dimension*)
          :element-type 'character
          :initial-element (ensure-intensity-to-be-of-character intensity)))

(defvar *animation* (list (make-cube)))
;;; We do perhaps need a lock for accesses on the *animation*, as the animation
;;; might be played by the idle-loop of the viewer while it gets modified inside
;;; the toolbox thread. (mgr, 20060102)
;;; We do indeed, and here it is: (mgr, 20061010)
(defvar *animation-lock* (clim-sys:make-lock "LED-cube animation lock"))
;;; If I don't screw it up completely, this is, among others, a problem between
;;; GOTO-CUBE and DELETE-CUBE as well as DELETE-ANIMATION. (NEXT-FRAME goes
;;; to the next frame using GOTO-CUBE.) Do we need to protect more functions?
;;; Yes. User-transformations, undo, and more...

(defmacro current-cube ()
  '(nth *current-cube-number* *animation*))

(defmacro do-cube-times ((x y z &optional result) &rest body)
  `(dotimes (,z *cube-dimension* ,result)
      (dotimes (,y *cube-dimension*)
	(dotimes (,x *cube-dimension*)
	    ,@body))))

(defun LED-number-to-coord (number)
  (let* ((z (truncate (/ number (expt 5 2))))
	   (number (- number (* z (expt 5 2))))
	   (y (truncate (/ number 5)))
	   (number (- number (* y 5)))
	   (x number))
    (list x y z)))

(defun sleep-for-delay (&optional (delay *default-idle-loop-delay*))
  (unless (zerop delay)
    (sleep delay)))

(defun goto-cube (target)
  (clim-sys:with-recursive-lock-held (*animation-lock*)
    (cond
      ((or (eq :next target)
           (eq :after target))
       (when (> (1- (length *animation*))  *current-cube-number*)
         (incf *current-cube-number*)))
      ((or (eq :previous target)
           (eq :before target))
       (when (< 0  *current-cube-number*)
         (decf *current-cube-number*)))
      ((eq :last target)
       (setf *current-cube-number* (1- (length *animation*))))
      ((eq :first target)
       (setf *current-cube-number* 0))
      ((typep target 'number)
       (setf *current-cube-number* target)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo

(defmacro with-disabled-save-undo (&rest body)
  `(let ((*save-undo-enabled* nil))
     ,@body))

(defun save-undo ()
  (when *save-undo-enabled*
    (clim-sys:with-recursive-lock-held (*animation-lock*)
    (let (animation)
      (dolist (actual-cube *animation*)
	(pushnew (copy-cube actual-cube) animation))
      (pushnew (cons (nreverse animation) *current-cube-number*)
	       *undo-cache*))
    (when (> (length *undo-cache*) ;; limit *undo-cache to *undo-depth*
	         *undo-depth*)
      (setf *undo-cache* (subseq *undo-cache* 0 *undo-depth*))))))

(defun undo ()
  (when *undo-cache*
    (clim-sys:with-recursive-lock-held (*animation-lock*)
      (let ((actual-undo (pop *undo-cache*)))
        (setf *animation* (car actual-undo)
              *current-cube-number* (cdr actual-undo))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fundamental cube and animation alteration functions

(defun set-LED (x y z &optional (intensity *current-intensity*))
  (save-undo)
  (setf (aref (current-cube) x y z) (ensure-intensity-to-be-of-character intensity)))

(defun get-LED (x y z)
  (aref (current-cube) x y z))

(defun copy-cube (&optional (cube-to-copy (current-cube)))
  (let ((new-cube (make-cube)))
    (do-cube-times (x y z)
      (setf (aref new-cube x y z)
	      (aref cube-to-copy x y z)))
    new-cube))

(defun insert-cube-copy (&key (cube-to-copy (current-cube))
			        (position :after)
			        (move-to-new-cube t))
  (save-undo)
  (let (new-animation-list)
    (dolist (cube (reverse *animation*))
      (when (eq position :before)
	(pushnew cube new-animation-list))
      (when (eq cube (current-cube))
	(pushnew (copy-cube cube-to-copy) new-animation-list))
      (when (eq position :after)
	(pushnew cube new-animation-list)))
    (setf *animation* new-animation-list))
;  (setf *animation* (append *animation* (list (copy-cube))))
  (when move-to-new-cube
    (goto-cube position)))

(defmacro fill-cube (&key (intensity '*current-intensity*)
                          (cube-to-empty '(current-cube)))
  `(progn
     (save-undo)
     (setf ,cube-to-empty (make-cube :intensity ,intensity))))

(defmacro empty-cube (&optional (cube-to-empty '(current-cube)))
  `(fill-cube :cube-to-empty ,cube-to-empty
              :intensity #\0))

(defun delete-cube (&optional (number *current-cube-number*))
  (clim-sys:with-recursive-lock-held (*animation-lock*)
    (save-undo)
    (when (and (> (length *animation*) 1)
               (> (length *animation*) number)
               (<= 0 number))
      (multiple-value-prog1
            (values (setf *animation* (delete (nth number *animation*) *animation*))
                    t)
        (when (<= number *current-cube-number*)
          (goto-cube :previous))))))

(defun delete-animation ()
  (clim-sys:with-recursive-lock-held (*animation-lock*)
    (save-undo)
    (setf *animation* (list (make-cube)))
    (goto-cube :first)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; additional cube and animation alteration functions
;; (This section shall be moved to a separate file.)

(defun move-cube (&key (cube-to-copy (current-cube)) (x 0) (y 0) (z 0))
  "Move the cube in the specified direction (via rotated shift).
   Direction specified in OpenGL coordinate system."
  (save-undo)
  (destructuring-bind (x y z)
      (opengl->cube x y z)
    (let ((new-cube (make-cube)))
      (do-cube-times (x-m y-m z-m)
        (setf (aref new-cube (mod (+ x x-m) 5)
                    (mod (+ y y-m) 5)
                    (mod (+ z z-m) 5))
              (aref cube-to-copy x-m y-m z-m)))
;;      new-cube))
      (setf (current-cube) new-cube))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; networking code

(defun set-server-address (name-or-ip &optional (port *default-server-port*))
  (setf *server-address* (list ;;(sb-bsd-sockets:make-inet-address ip)
                               (sb-bsd-sockets:host-ent-address
                                (sb-bsd-sockets:get-host-by-name
                                 (or name-or-ip *default-server-name*)))
                               (or port *default-server-port*) ;; in case of nil
                               name-or-ip)))

(defun get-server-name-or-ip ()
  (third *server-address*))

(defun get-server-port ()
  (second *server-address*))

(defun networking-started-p ()
  *socket*)

(defun start-networking (&key (send-go-online t));;&key (address *server-address* address-p))
;;   (when address-p
;;     (set-server-address address))
  (stop-networking :send-go-offline nil)
  (setf *socket* (make-instance 'sb-bsd-sockets:inet-socket :type :datagram :protocol :udp))
  (when send-go-online
    (network-send :go_online #+nil "I'M_NOT_LISTENING")
    (write-cube-to-device :device-file nil)))

(defun stop-networking (&key (send-go-offline t))
  (when (networking-started-p)
    (when send-go-offline
      (network-send :go_offline))
    (sb-bsd-sockets:socket-close *socket*)
    (setf *socket* nil)))

(defun network-send (type &optional string)
  (when (networking-started-p)
    (unless *server-address*
      (set-server-address *default-server-name* *default-server-port*))
    (let ((net-string (with-output-to-string (stream)
                        (let ((*standard-output* stream))
                          (princ "CUBE_MODELLER_MSG")
                          (princ #\Space)
                          (princ type)
                          (when string
                            (princ #\newline)
                            (princ string))
                          (princ #\Newline)))))
      (sb-bsd-sockets:socket-send *socket* net-string nil :address *server-address*))))

(defun network-upload-animation (&key (animation *animation*)
                                      (author "Anonymous")
                                      (title "unnamed"))
  (let ((have-been-online (if *socket* t nil)))
    (unless have-been-online
      (start-networking :send-go-online nil))
    (let ((string (with-output-to-string (*standard-output*)
                    (princ author)
                    (princ #\Newline)
                    (princ title)
                    (princ #\Newline)
                    (let ((length (length animation)))
                      (dotimes (i length)
                        (do-cube-times (x y z)
                          (princ (aref (nth i animation) x y z)))
                          (unless (= i (1- length))
                            (princ #\Newline)))))))
      (network-send :animation string))
    (unless have-been-online
      (stop-networking :send-go-offline nil))))

;; variant: in any case,go offline after animation upload
;; (defun network-upload-animation (&optional (animation *animation*))
;;   (when *server-address*
;;     (let ((have-been-online (if *socket* t nil)))
;;       (if have-been-online
;;           (network-send :go_offline) ;; let the server play the animation after upload
;;           (start-networking :send-go-online nil))
;;       (let ((string (with-output-to-string (*standard-output*)
;;                       (dolist (cube animation)
;;                         (do-cube-times (x y z)
;;                           (princ (aref cube x y z)))
;;                         (princ #\Newline)))))
;;         (network-send :animation string))
;;       (stop-networking :send-go-offline nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; input and output functions

(defun set-device-file (device-file)
  (close-cube-device)
  (setf *device-file* device-file))

(defun set-terminal-speed (&key (device-file *device-file*) (speed sb-posix:B57600))
  (with-open-file (stream device-file :direction :io :if-exists :append)
    (let ((termios (sb-posix:tcgetattr stream)))
      (sb-posix:cfsetispeed speed termios)
      (sb-posix:cfsetospeed speed termios)
      (sb-posix:tcsetattr stream sb-posix:tcsadrain termios))))

(defun open-cube-device (&optional (device-file *device-file*))
  (close-cube-device)
  ;; configure device
  (set-terminal-speed :device-file device-file)
  ;; and open it for output
  (setf *device-stream*
        (open device-file :direction :output :if-exists :append)))

(defun close-cube-device ()
  (when (and (streamp *device-stream*)
             (open-stream-p *device-stream*))
    (close *device-stream*))
  (setf *device-stream* nil))

(defun write-cube-to-device (&key (cube (current-cube))
			                (device-file *device-file*))
  (when device-file
    (clim-sys:with-recursive-lock-held (*animation-lock*) ;; this has occurred (mgr, 20061010)
      (clim-sys:with-lock-held (*device-lock*)
        (unless (and (streamp *device-stream*)
                     (open-stream-p *device-stream*))
          (open-cube-device device-file))
        (let ((serial *device-stream*))
;;        (with-open-file (serial device-file :direction :output :if-exists :append
;;                                ) ;; :element-type 'character)
          ;; hackhackhack
;;          (princ "0000" serial)
          (do-cube-times (x y z)
            ;; (princ (aref cube x y z))
            (princ (aref cube x y z) serial))
          ;; (princ #\Newline)
          (princ #\Newline serial)
          (force-output serial))
        cube)))
  
  (when (networking-started-p)
    (let ((string (with-output-to-string (stream)
                    (do-cube-times (x y z)
                      (princ (aref cube x y z) stream))
                    #+nil (princ #\Newline stream))))
      (network-send :frame string)))
  nil)

(defun load-animation-file (pathname)
  (let ((type (pathname-type pathname)))
    (unless (find type '("data" "raw") :test #'string-equal)
      (error (format nil "Don't know how to load a file of the type \"~a\"." type)))

    (delete-animation)
    (with-disabled-save-undo
        (clim-sys:with-recursive-lock-held (*animation-lock*)
          (with-open-file (file (merge-pathnames pathname))
            (cond ((string-equal type "data")
                   (setf *animation* (read file)))
                  
                  ((string-equal type "raw")
                   (do ((line (read-line file) (read-line file nil 'eof)))
                       ((eq line 'eof))
                     ;; (print line)
                     (dotimes (led-number 125)
                       (destructuring-bind (x y z)
                           (led-number-to-coord led-number)
                         ;; (format t "-> ~d ~d ~d: ~d <-~%" x y z (elt line led-number)) (force-output)
                         (setf (aref (current-cube) x y z) (elt line led-number))))
                     (insert-cube-copy))))))
      (goto-cube :first))))

(defun save-animation-file (pathname &key (if-exists :error))
  (let ((type (pathname-type pathname)))

    (unless type ;; assume a "data"-file if type is not specified
      (setf type "data"
            pathname (make-pathname :type type
                                    :defaults pathname)))

    (unless (find type '("data" "raw" "c") :test #'string-equal) ;; complain if type is unknown
      (error (format nil "Don't know how to write a file of the type ~a." type)))

    (with-open-file (file (merge-pathnames pathname)
                          :direction :output :if-does-not-exist :create :if-exists if-exists)
      (cond ((string-equal type "data")
             (write *animation* :stream file))
            
            ((string-equal type "raw")
             (dolist (cube *animation*)
               (do-cube-times (x y z)
                 (princ (aref cube x y z) file))
               (princ #\Newline file)))
            
            ((string-equal type "c")
             (princ "{" file)
             (dolist (cube *animation*)
               (do-cube-times (x y z)
                 (princ (aref cube x y z) file)
                 (princ "," file))
               (princ #\Newline file))
             (princ "}" file))))))
