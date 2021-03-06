(in-package "COMMON-LISP-USER") 
;;                                                         rm - february 1993
;;                                                         email: "rm@cast.uni-linz.ac.at"
;;                                                         newest version of this file available
;;                                                         from "ftp.cast.uni-linz.ac.at"

;;                                         changes:
;;                                         93/03/15 rm for 1.1 CLIM/MCL: 
;;                                                  don't use the mac menubar
;;                                         93/03/17 rm for 1.1 CLIM/MCL: 
;;                                                  work-around for read-gesture-bug in run-simulation

(export '(open-kytron))
(proclaim '(optimize (speed 3) (space 0) (safety 1)))

;;; Simulation of KYTRONs on the Moon. 

;;; ABSTRACT:
;; KYTRONs are small autonomous vehicles which try to find a bright light source (e.g. the sun)
;; to be able to recharge their batteries from their solar panels. 
;; They live on a moon-like surface with obstacles. The (randomly computed) landscape
;; consists of craters and mountains which the KYTRONs cannot climb.

;; KYTRONs are real - they were built by rm from 1974 to 1980 (KYTRON 1, KYTRON 2, 
;; m-KYTRON 3, c-KYTRON 4, and KYTRON 5). c-KYTRON 4 has a MC14500B micro-controller and 
;; 256 words of program memory. The other KYTRONs are controlled by pure digital hardware
;;  - no software, only SSI CMOS circuits.

;; This CLIM program is a toy simulation application written to be completely
;; portable under CLIM 1.1, developed in MCL 2.0b + CLIM 1.0 beta.
;; It was tested in MCL 2.0p1 + CLIM 1.1, 
;; and Lucid 4.0.1 + CLIM 1.1 on a Sun IPX and a Sun ELC.
;; (Some color-names used in this version are not supported in 1.0 beta, namely the
;; gray-colors.)
;; Only the open-root-window call has to be adapted to other platforms...

;;; GOAL:
;; The primary goal was to learn the most important CLIM concepts:
;;         - application-frame
;;         - pane
;;         - command
;;         - presentation-translator
;;         - drawing
;;         - incremental redisplay
;; Only the upper-layer functions of CLIM are used as far as it was possible. No low-level 
;; programming. The powerful CLIM default-mechanisms for presentation-types, 
;; presentation-translators, command-tables etc. are used extensively, which keeps the
;; GUI-part rather short.

;;; HOW TO USE:
;;  After loading the file, start the simulation application by evaluating (open-kytron).
;;  Then you may add some KYTRONs with "Add KYTRON ..." and modify the 
;;  landscape by adding new (randomly placed) elements or deleting some of the existing. If 
;;  some KYTRON is placed accidently on an obstacle, please move it to some free place using
;;  "Move Object".
;;  Then you start the simulation with "Run Simulation". Stop it anytime with a mouse-click
;;  in the main pane. After quitting, you may resume with the last state
;;  of the environment by evaluating (common-lisp-user:open-kytron). Use a "KYTRON 2" for
;;  best results. It handles rather complex situations.

;;;====================================================================

;;; CONTENTS OF THIS FILE:
;;; (1)----- basic classes --------------------------------------------
;;; (2)----- misc. ----------------------------------------------------
;;; (3)----- discrete simulation of an analogue Monoflop --------------
;;; (4)----- the MOON surface -----------------------------------------
;;; (5)----- basic KYTRON functionality -------------------------------
;;; (6)----- KYTRON simulation  ---------------------------------------
;;; (7)----- light searching ------------------------------------------
;;; (8)----- geometry -------------------------------------------------
;;; -------------------------------------------------------------------
;;; (9)----- the CLIM GUI ---------------------------------------------
;;;(10)----- presentation by drawing ----------------------------------
;;;(11)----- basic geometric drawing ----------------------------------
;;;(12)----- CLIM commands --------------------------------------------
;;;(13)----- hacks for obstacle detection -----------------------------
;;;(14)----- initialization -------------------------------------------

;;;====================================================================

;;; -------------------------------------------------------------------
;;; (1)----- basic classes --------------------------------------------

(defclass movable ()
  ((output-record :accessor output-record :initform nil 
                  :documentation "CLIM's internal record of drawing actions"))
  (:documentation "mixin for any object movable with the pointer"))

(defclass kytron (movable)
  ((direction :accessor direction :initarg :direction :documentation 
              "orientation vector of length 1")
   (location :accessor location :initarg :location :documentation "translation vector")
   (border :accessor border :initarg :border :documentation "bounding rectangle")
   (name :accessor name :initarg :name :documentation 
         "the default is the name of the original KYTRON")
   (speed :accessor speed :initarg :speed :documentation "actual speed")
   (default-speed :reader default-speed :initform 5 :allocation :class :documentation "max speed")
   (timer :reader timer :initform (make-timer 20) :documentation "for darkness action")
   (bumpers :accessor bumpers :initarg :bumpers :documentation "mechanicle obstacle sensors")
   (graphics :accessor graphics :initarg :graphics :documentation "drawing data"))
  (:documentation "abstract class for common features of all KYTRONs")
  (:default-initargs :name "KYTRON Lisp" :speed 5 
    :location (make-coord (round (rand 20 200)) (round (rand 20 200)))
    :direction (make-coord (sqrt 2) (sqrt 2))
    :border (make-box -12 -8 8 8)
    :bumpers (list (list :left (make-coord 8 -8))
                   (list :right (make-coord 8 8))
                   (list :left-rear (make-coord -12 -8))
                   (list :right-rear (make-coord -12 8)))
    :graphics (list (make-filled-box -12 4 8 8 'blue)
                    (make-filled-box -12 -8 8 -4 'blue)
                    (make-filled-box -11 -4 7 4 'cyan)
                    (make-filled-circle 3 (make-coord -1 0) 'red))))

(defclass kytron3 (kytron)
  ((default-speed :reader default-speed :initform 5 :allocation :class)
   (timer :reader timer :initform (make-timer-chain 40 12))
   (timer-rev-left :reader timer-rev-left :initform (make-timer-chain 3 8))
   (timer-rev-right :reader timer-rev-right :initform (make-timer-chain 3 8)))
  (:documentation "the m-KYTRON 3 was built 1976 - it is 11cm long and has 8 solar
    cells which on bright sunny days deliver more energy than needed for driving -
    runs off rechargable batteries in the dark when TIMER is triggered")
  (:default-initargs  :name "m-KYTRON 3" :speed 5 
    :location (make-coord (round (rand 20 200)) (round (rand 20 200)))
    :direction (make-coord 1 0)
    :border (make-box -12 -8 8 8)
    :bumpers (list (list :left (make-coord 8 -8))
                   (list :right (make-coord 8 8))
                   (list :left-rear (make-coord -12 -8))
                   (list :right-rear (make-coord -12 8)))
    :graphics (list (make-filled-box -12 4 8 8  'blue)
                    (make-filled-box -12 -8 8 -4 'blue)
                    (make-filled-box -11 -4 7 4 'cyan)
                    (make-filled-circle 3 (make-coord -1 0) 'red))))

(defclass kytron5 (kytron)
  ((default-speed :reader default-speed :initform 3 :allocation :class)
   (timer-rev-back :reader timer-rev-back :initform (make-timer 5))
   (timer-rev-left :reader timer-rev-left :initform (make-timer 3))
   (timer-rev-right :reader timer-rev-right :initform (make-timer 3)))
  (:documentation "the KYTRON 5 was built 1978 - it is only 5.5cm long and has 8 solar
    cells to recharge its batteries - drives in the dark when TIMER is triggered")
  (:default-initargs  :name "KYTRON 5" :speed 3 
    :location (make-coord (round (rand 20 200)) (round (rand 20 200)))
    :direction (make-coord 1 0)
    :border (make-box -6 -4 4 4)
    :bumpers (list (list :left (make-coord 4 -4))
                   (list :right (make-coord 4 4))
                   (list :left-rear (make-coord -6 -4))
                   (list :right-rear (make-coord -6 4)))
    :graphics (list (make-filled-box -6 2 4 4 'blue)
                    (make-filled-box -6 -4 4 -2 'blue)
                    (make-filled-box -5 -2 3 2 'orange)
                    (make-filled-box -4 -3 2 3 'cyan)
                    (make-box -4 -3 2 3))))

(defclass kytron4 (kytron3)
  ((default-speed :reader default-speed :initform 8 :allocation :class))
  (:documentation "the KYTRON 4 was built 1977 and in 1980 it got new electronics
    with the 4500 microcontroller - it is 20cm long and has 16 solar
    cells to recharge its batteries - drives in the dark when TIMER is triggered")
  (:default-initargs :name "c-KYTRON 4" :speed 8 
    :location (make-coord (round (rand 20 200)) (round (rand 20 200)))
    :direction (make-coord 1 0)
    :border (make-box -18 -12 16 12)
    :bumpers (list (list :left (make-coord 12 -12))
                   (list :right (make-coord 12 12))
                   (list :left-rear (make-coord -18 -12))
                   (list :right-rear (make-coord -18 12)))
    :graphics (list (make-filled-box -18 7 12 12 'red)
                    (make-filled-box -18 -12 12 -7 'red)
                    (make-filled-box -16 -7 10 7 'orange)
                    (make-filled-circle 4 (make-coord -1 0) 'red))))

(defclass kytron2 (kytron3)
  ((default-speed :reader default-speed :initform 6 :allocation :class)
   (b-memory :accessor b-memory :initform nil :documentation 
             "should the next obstacle method be :left or :right?")
   (b-timer :reader b-timer :initform (make-timer 15) :documentation
            "the obstacle method is taken from b-memory when the timer runs -
             from the bumper else"))
  (:documentation "the KYTRON 2 was built 1975  - it is 18cm long - 
    this KYTRON behaves most intelligently of all
    because its complex obstacle methods implemented in then-new CMOS logic")
  (:default-initargs :name "KYTRON 2" :speed 3 
    :location (make-coord (round (rand 20 200)) (round (rand 20 200)))
    :direction (make-coord 1 0)
    :border (make-box -18 -13 16 13)
    :bumpers (list (list :left (make-coord 16 -13))
                   (list :right (make-coord 16 13))
                   (list :left-rear (make-coord -18 -13))
                   (list :right-rear (make-coord -18 13)))
    :graphics (list (make-filled-box -18 7 16 13 'cyan)
                    (make-filled-box -18 -13 16 -7 'cyan)
                    ;(make-filled-box -16 -7 14 7 'orange)
                    (make-filled-polygon* 
                     '(-18 -4 -15 -12 13 -12 16 -9 16 9 13 12 -15 12 -18 4)
                     'orange)
                    (make-box -13 -4 -5 4)
                    (make-filled-circle 4 (make-coord 2 0) 'red))))

(defun make-kytron (n &rest options)
  (case n
    (2 (apply #'make-instance 'kytron2 options))
    (3 (apply #'make-instance 'kytron3 options))
    (4 (apply #'make-instance 'kytron4 options))
    (5 (apply #'make-instance 'kytron5 options))
    (t (warn "unhandled KYTRON type number ~s" n)
       (apply #'make-instance 'kytron options))))

;;; bumpers (the mechanical obstacle sensors) are implemented as lists

(defun bumper-name (bumber-descr)
  (first bumber-descr))

(defun geometry (bumber-descr)
  (second bumber-descr))

;;; -------------------------------------------------------
;;; (2)----- misc. ----------------------------

(defun degree-to-rad (degree)
  ;; for my convenience only
  (* (/ degree 180) pi))

(defun square (n) (* n n))

(defun boolean-exor (x y)
  (or (and x (not y)) (and (not x) y)))

;;; -------------------------------------------------------
;;; (3)----- discrete simulation of an analogue Monoflop --

(defclass monoflop ()
  ((timer :accessor timer :initform 0 :documentation "integer count down")
   (lasttimer :accessor lasttimer :initform 0 :documentation "for edge triggering")
   (init-val :reader init-val :initarg :init-val :documentation "delay units"))
  (:documentation "discrete simultion of an analogue Monoflop")
  (:default-initargs :init-val 2))

(defmethod trigger ((m monoflop) &optional (index 0))
  "start the timer"
  (declare (ignore index))
  ;; assume retriggerable monoflops
  (setf (timer m) (init-val m)))

(defmethod reset-timer ((m monoflop))
  "reset with suppressed output edges (!)"
  (setf (lasttimer m) 0)
  (setf (timer m) 0))

(defmethod one-step ((m monoflop))
  "simulate one timestep"
  (setf (lasttimer m) (timer m))
  (setf (timer m) (update-timer (timer m))))

(proclaim '(inline update-timer))

(defun update-timer (n)
  (let ((r (1- n)))
    (if (< r 0) 0 r)))

(defmethod output-level ((m monoflop))
  "logic output"
  (if (> (timer m) 0) t nil))

(defmethod output-edge-p ((m monoflop))
  "negative output edge predicate"
  (if (and (= (timer m) 0) (= (lasttimer m) 1)) t nil))

(defclass monoflop-chain ()
  ((timers :reader timers :initarg :timers :documentation "ordered set of chained monoflops"))
  (:documentation "discrete simultion of chained Monoflops")
  (:default-initargs :timers nil))

(defmethod trigger ((mc monoflop-chain) &optional (index 0))
  "start the timer(index) of the timer chain"
  (trigger (nth index (timers mc))))

(defmethod output-level ((mc monoflop-chain))
  (output-level (first (last (timers mc)))))

(defmethod output-levels ((mc monoflop-chain))
  (mapcar #'output-level (timers mc)))

(defmethod reset-timer ((mc monoflop-chain))
  "total reset of whole chain"
  (dolist (timer (timers mc))
    (reset-timer timer)))

(defmethod one-step ((mc monoflop-chain))
  (labels ((one-step-recursive (timer-chain)
             (if (endp timer-chain)
               nil
               (progn 
                 (one-step (first timer-chain))
                 (if (output-edge-p (first timer-chain))
                   (unless (endp (rest timer-chain))
                     (trigger (second timer-chain))
                     (one-step-recursive (rest (rest timer-chain))))
                   (one-step-recursive (rest timer-chain)))))))
    (one-step-recursive (timers mc))))

(defmethod k-describe ((mc monoflop-chain))
  (format t "~&Contents of Timer Chain: ~s" (mapcar #'timer (timers mc))))

(defun make-timer (delay)
  (make-instance 'monoflop :init-val delay))

(defun make-timer-chain (&rest delay-list)
  (make-instance 'monoflop-chain :timers 
                 (mapcar #'make-timer delay-list)))

#|
************ testing the timers
(setq *t* (make-timer-chain '(3 5 3)))
(k-describe *t*)
(trigger *t*)
(k-describe *t*)
(dotimes (i 12)
  (one-step *t*)
  (k-describe *t*))
(trigger *t* 1)
(k-describe *t*)
(one-step *t*)
(k-describe *t*)
(reset-timer *t*)
(k-describe *t*)
  
|#

;;; -------------------------------------------------------------
;;; (4)----- the MOON surface --------------------------------

(defclass moon ()
  ((surface-elements :accessor elements :initarg :elements))
  (:documentation "Simulation Environment class"))

(defclass mount (movable)
  ((location :accessor location :initarg :location)
   (outer :reader outer :initarg :outer :documentation "a geometric polygon"))
  (:documentation "a mountain, the basic obstacle class"))

(defclass crater (mount)
  ((inner :reader inner :initarg :inner :documentation "a geometric object"))
  (:documentation "a mountain with a hole..."))

(defclass light-source (movable)
  ((centre :accessor centre :accessor location :initarg :centre)
   (radius :accessor radius :accessor intensity :initarg :radius :documentation 
           "distance, in which the brightness is one unit"))
  (:documentation "a light source attracts mosquitos and KYTRONs")
  (:default-initargs :centre (make-coord 400 400) :radius 600))

(defun make-light (x y radius)
  "constructor function"
  (make-instance 'light-source :centre (make-coord x y) :radius radius))

(defun rand (n m)
  "return a random float uniformly distributed between n and m"
  (+ n (* (random 1.0) (- m n))))

(defun random-select-from (list)
  "select a randomly chosen elt of list"
  (nth (random (length list)) list))

(defvar *north* #+:mcl 0 #-:mcl -1000)

(defvar *west* #+:mcl 0 #-:mcl -1000)

(defvar *east* 1400)

(defvar *south* #+:mcl 1000 #-:mcl 1200)

(defvar *number-of-mounts* #+:mcl 24 #-:mcl 80)

(defun make-random-mount ()
  "random constructor function"
  (let* ((location (make-coord (round (rand *west* *east*)) (round (rand *north* *south*))))
         (p (make-coord 0 (rand 10 50)))
         (n (round (rand 8 32)))
         (as (loop for i from 1 to n collect (rand (/ (* 1 pi) n) (/  (* 3 pi) n))))
         (vs (loop for a in as collect 
                   (nround-coord (scale (setq p (rotate p a)) (rand 0.8 3.0))))))
    (make-instance 'mount :location location
                   :outer (make-filled-polygon vs (random-select-from '(gray dark-gray gray dark-gray green))))))

(defun make-random-crater ()
  (let* ((location (make-coord (round (rand *west* *east*)) (round (rand *north* *south*))))
         (p (make-coord 0 (rand 10 25)))
         (n (rand 8 32))
         (as (loop for i from 1 to n collect (rand (/ (* 1 pi) n) (/  (* 3 pi) n))))
         (vs (loop for a in as collect 
                   (nround-coord (scale (setq p (rotate p a)) (rand 0.9 2))))))
    (make-instance 'crater :location location
                   :outer (make-filled-polygon vs (random-select-from '(gray dark-gray))) 
                   :inner (make-filled-circle (* (y p) 0.8) (make-coord 0 0) (random-select-from '(white light-gray))))))

(defun make-random-moon-surface (&optional (no-of-obstacles *number-of-mounts*))
  (make-instance 'moon 
                 :elements (let ((gl nil))
                             (dotimes (i no-of-obstacles gl)
                               (push (if (< 1 (random 3))
                                       (make-random-crater)
                                       (make-random-mount))
                                     gl)))
                 ))

(defmethod region ((m mount))
  (region (outer m)))

(defmethod regions ((moon moon))
  (mapcar #'region (elements moon)))

 
;;; --------------------------------------------------------------
;;; (5)----- basic KYTRON functionality --------------------------

(defmethod step-forward ((k kytron))
  (setf (location k) (add-coords (scale (direction k) (speed k))
                                 (location k))))

(defmethod turn-left ((k kytron) &optional (angle (* 2 pi 1/24)))
  (setf (direction k) (nrotate (direction k) (- angle))))
                      
(defmethod turn-right ((k kytron) &optional (angle (* 2 pi 1/24)))
  (setf (direction k) (nrotate (direction k) angle)))

;;; --------------------------------------------------------------
;;; (6)----- KYTRON simulation  ----------------------------------


(defmethod one-step ((k kytron))
  "one timestep simulation - default"
  ;; in this method k is regarded as a FSM
  (if (check-environment k)  
    (clim:beep)
    (step-forward k)))

(defmethod one-step ((k kytron5))
  "one timestep simulation - KYTRON 5"
  (let ((touches (check-environment k)))
    (when (and (or (output-level (timer-rev-left k))
                   (output-level (timer-rev-right k)))
               (or (member :left-rear touches)
                    (member :right-rear touches)))
      (trigger (timer-rev-back k))
      (turn-left k (- (degree-to-rad (random 21)) (degree-to-rad 11))))
    (when (member :left-rear touches)
      (trigger (timer-rev-left k)))
    (when (member :right-rear touches)
      (trigger (timer-rev-right k)))
    (cond ((and (output-level (timer-rev-left k))
                (output-level (timer-rev-right k)))
           ;; KYTRON is in reversing operation
           ;;straight backwards
           (setf (speed k) (- (default-speed k)))
           (step-forward k))
          ((output-level (timer-rev-left k))
           ;; backwards turn left
           (setf (speed k) (- (default-speed k)))
           (turn-left k (degree-to-rad 45)))
          ((output-level (timer-rev-right k))
           ;; backwards turn right
           (setf (speed k) (- (default-speed k)))
           (turn-right k (degree-to-rad 45)))
          ((output-level (timer-rev-back k))
           ;; forwards straight
           (setf (speed k) (default-speed k))
           (step-forward k))
          (t ;; KYTRON is in normal light finding or timer operation
            (drive-normal k)))
    (update-timers k)))

(defmethod one-step ((k kytron3))
  "one timestep simulation - KYTRON 3"
  (let ((touches (check-environment k)))
    (update-timers k)
    (cond ((or (output-level (timer-rev-left k))
               (output-level (timer-rev-right k)))
           ;; KYTRON is in reversing operation
           (if (or (member :left-rear touches)
                    (member :right-rear touches))
             ;; touching another obstacle in the back
             ;; KYTRON 3: simply shut off timers, restore speed
             (progn (reset-reversing k) )
             ;; else continue reversing
             (drive-reversing k)))
           (t ;; KYTRON is in normal operation
            (if (or (member :left touches)
                    (member :right touches))
             ;; touching an obstacle
             ;; KYTRON 3: start timers, reverse speed
             (progn (set-reversing k touches) 
                    (drive-reversing k))
             ;; no obstacles, should search for light or work on timer
             (drive-normal k))))
    ))

(defmethod one-step ((k kytron2))
  "one timestep simulation - KYTRON 2"
  (let ((touches (check-environment k)))
    (update-timers k)
    (cond ((or (output-level (timer-rev-left k))
               (output-level (timer-rev-right k)))
           ;; KYTRON is in reversing operation
           (if (or (member :left-rear touches)
                    (member :right-rear touches))
             ;; touching another obstacle in the back
             ;; KYTRON 2: shut off timers, restore speed, ...
             (progn ;; memorize presence of obstacles
                    (trigger (b-timer k))
                    ;; if in first or third phase, switch obstacle-avoidance method
                    (if (boolean-exor (output-level (timer-rev-left k))
                                      (output-level (timer-rev-right k)))
                      (switch-avoid-method k))
                    (reset-reversing k))
             ;; else continue reversing
             (drive-reversing k)))
           (t ;; KYTRON is in normal operation
            (if (or (member :left touches)
                    (member :right touches))
             ;; touching an obstacle
             ;; KYTRON 2: start timers, reverse speed, ...
             (progn (set-reversing k touches) 
                    ;; memorize presence of obstacles
                    (trigger (b-timer k))
                    (drive-reversing k))
             ;; no obstacles, should search for light or work on timer
             (drive-normal k))))
    ))

(defmethod drive-normal ((k kytron))
  "search for light or drive according to the timer"
  ;; default, appropriate for kytron2 and kytron3
  (let ((cmds (append (compute-search-light-drive-commands k) (timer-commands k))))
    (cond ((and (member :left cmds) (member :right cmds))
           (setf (speed k) (default-speed k)))
          ((member :left cmds)
           (turn-left k)
           (setf (speed k) (default-speed k)))
          ((member :right cmds)
           (turn-right k)
           (setf (speed k) (default-speed k)))
          (t (setf (speed k) 0))))
  (step-forward k))

(defmethod drive-normal ((k kytron5))
  ;; may drive backwards
  (let ((cmds (append (compute-search-light-drive-commands k) (timer-commands k))))
    (cond ((and (member :left cmds) (member :right cmds))
           (setf (speed k) (default-speed k)))
          ((and (member :left-rear cmds) (member :right-rear cmds))
           (setf (speed k) (- (default-speed k))))
          ((and (member :left cmds) (member :right-rear cmds))
           (turn-left k (degree-to-rad 45))
           (setf (speed k) 0))
          ((and (member :right cmds) (member :left-rear cmds))
           (turn-right k (degree-to-rad 45))
           (setf (speed k) 0))
          ((member :left cmds)
           (turn-left k)
           (setf (speed k) (default-speed k)))
          ((member :right cmds)
           (turn-right k)
           (setf (speed k) (default-speed k)))
          ((member :left-rear cmds)
           (turn-right k)
           (setf (speed k) (- (default-speed k))))
          ((member :right-rear cmds)
           (turn-left k)
           (setf (speed k) (- (default-speed k))))
          (t (setf (speed k) 0))))
  (step-forward k))

(defmethod switch-avoid-method ((k kytron2))
  "switch the obstacle  avoidance method"
  (if (eql :left (b-memory k))
    (setf (b-memory k) :right)
    (setf (b-memory k) :left)))

(defmethod drive-reversing ((k kytron3))
  (cond ((and (output-level (timer-rev-left k))
              (output-level (timer-rev-right k)))
         ;;straight backwards
         (setf (speed k) (- (default-speed k)))
         (step-forward k))
        ((output-level (timer-rev-left k))
         ;; backwards turn left
         (setf (speed k) (- (default-speed k)))
         (turn-left k (degree-to-rad 30))
         (step-forward k))
        ((output-level (timer-rev-right k))
         ;; backwards turn right
         (setf (speed k) (- (default-speed k)))
         (turn-right k (degree-to-rad 30))
         (step-forward k))
        (t (error "KYTRON ~s not reversing" k))))

(defmethod set-reversing ((k kytron3) touches)
  ;; KYTRON 3: dealing left and right independently
  (when (member :right touches)
    (trigger (timer-rev-left k) 0)        ;first of left MFF chain
    (trigger (timer-rev-right k) 1))      ;second of right MFF chain
  (when (member :left touches)
    (trigger (timer-rev-left k) 1)        ;second of left MFF chain
    (trigger (timer-rev-right k) 0))      ;first of right MFF chain
  (setf (speed k) (- (default-speed k))))

(defmethod set-reversing ((k kytron2) touches)
  ;; KYTRON 2: either left exor right wins, never both
  (if (and (member :right touches) (member :left touches))
    (if (= 0 (random 2))
      (progn
        (trigger (timer-rev-left k) 0)        ;first of left MFF chain
        (trigger (timer-rev-right k) 1))      ;second of right MFF chain
      (progn
        (trigger (timer-rev-left k) 1)        ;second of left MFF chain
        (trigger (timer-rev-right k) 0))))    ;first of right MFF chain
  (progn
    (when (member :right touches)
      (trigger (timer-rev-left k) 0)          ;first of left MFF chain
      (trigger (timer-rev-right k) 1))        ;second of right MFF chain
    (when (member :left touches)
      (trigger (timer-rev-left k) 1)          ;second of left MFF chain
      (trigger (timer-rev-right k) 0))  )     ;first of right MFF chain
  (setf (speed k) (- (default-speed k))))

(defmethod set-reversing :around ((k kytron2) touches)
  "use touches if b-timer is off, b-memory else"
  ;; this is the place where the "intelligence" of KYTRON 2 comes from!
  (let ((corrected-touch 
         (if (output-level (b-timer k))
           (b-memory k)
           (progn
             ;; update b-memory
             (if (and (member :right touches) (member :left touches))
               (if (= 0 (random 2)) (setf (b-memory k) :left) (setf (b-memory k) :right))
               (if (member :left touches) 
                 (setf (b-memory k) :left)
                 (setf (b-memory k) :right)))
             (b-memory k)))))
    (call-next-method k (list corrected-touch))))

(defmethod update-timers ((k kytron5))
  ;; not too elegant...
  (one-step (timer k))
  (one-step (timer-rev-back k))
  (one-step (timer-rev-left k))
  (one-step (timer-rev-right k)))

(defmethod update-timers ((k kytron3))
  ;; not too elegant...
  (one-step (timer k))
  (one-step (timer-rev-left k))
  (one-step (timer-rev-right k)))

(defmethod reset-reversing ((k kytron3))
  "stop the reversing timers, restore speed"
  (reset-timer (timer-rev-left k))
  (reset-timer (timer-rev-right k))
  (setf (speed k) (default-speed k)))

;;;---------------------------------------------------------------------
;;; (7)----- light searching ------------------------------------------

;; 7.1 geometric calculations common to all KYTRONs:

(defmethod light-source-angle ((k kytron) (ls light-source))
  "returns the angle and the brightness under which KYTRON k sees light ls"
  ;; should work for all KYTRONs
  (let ((d2 (+ (square (- (x (location k)) (x (location ls))))
               (square (- (y (location k)) (y (location ls)))))) ; square of distance k->ls
        (gamma-k (atan (y (direction k)) (x (direction k)))) ; angle of kytron absolute
        (gamma-ls                                  ;angle of light relative to k location
         (atan (- (y (location ls)) (y (location k))) 
               (- (x (location ls)) (x (location k)))))) 
    ;; why isn't it (- gamma-ls gamma-k) here? Math is hard...
    (list (- gamma-k gamma-ls) (/ (square (intensity ls)) d2))))

(defun collect-light-values (angle-brightness-pairs from-angle to-angle)
  "add up intensities of sources between from-angle and to-angle"
  (let ((intensity-sum 0))
    (dolist (angle-brightness angle-brightness-pairs intensity-sum)
      (when (angle-between-p (first angle-brightness) from-angle to-angle)
        (incf intensity-sum (second angle-brightness))))))
  
(defun angle-between-p (angle from-angle to-angle)
  ;; assumes that (< from-angle to-angle)
  (unless  (<= (- pi) angle) (setq angle (+ (* 2 pi) angle)))
  (unless  (<= angle pi) (setq angle (- angle (* 2 pi))))
  (unless  (< from-angle to-angle) (error "Not (< from-angle to-angle)"))
  (cond ((< from-angle (- pi))
         (or (angle-between-p angle (- pi) to-angle)
             (angle-between-p angle (+ (* 2 pi) from-angle) pi)))
        ((> to-angle pi)
         (or (angle-between-p angle from-angle pi)
             (angle-between-p angle (- pi) (- to-angle (* 2 pi)))))
        (t (angle-between-simple-p angle from-angle to-angle))))

(defun angle-between-simple-p (angle from-angle to-angle)
  ;; assumes that all angles between -pi and pi
  (<= from-angle angle to-angle))

;; 7.2 the following methods are KYTRON specific:

(defmethod compute-search-light-drive-commands ((k kytron))
  "simulation of light sensors"
  (let ((lights (mapcar #'(lambda (x) (light-source-angle k x)) 
                        (light-sources clim:*application-frame*))))
    (let ((right-front (collect-light-values lights (degree-to-rad -90) 0))
          (left-front (collect-light-values lights 0 (degree-to-rad 90)))
          (result nil))
      (when (> left-front 1) (push :left result))
      (when (> right-front 1) (push :right result))
      ;; example (list :left :right) 
      result)))

(defmethod compute-search-light-drive-commands ((k kytron2))
  "simulation of light sensors"
  ;; fixed threshold value (= 1) in this KYTRON
  (let ((lights (mapcar #'(lambda (x) (light-source-angle k x))
                        (light-sources clim:*application-frame*))))
    (let ((right-front (collect-light-values lights (degree-to-rad -181) (degree-to-rad 5)))
          (left-front (collect-light-values lights (degree-to-rad -5) (degree-to-rad 181)))
          (result nil))
      (when (> left-front 1) (push :left result))
      (when (> right-front 1) (push :right result))
      ;; example (list :left :right) 
      result)))

(defmethod compute-search-light-drive-commands ((k kytron3))
  "simulation of light sensors"
  ;; automatic gain control (agc) ignored in this version of the simulator
  (let ((agc 1)
        (lights (mapcar #'(lambda (x) (light-source-angle k x))
                        (light-sources clim:*application-frame*))))
    (let ((right-front (collect-light-values lights (degree-to-rad -60) (degree-to-rad 10)))
          (left-front (collect-light-values lights (degree-to-rad -10) (degree-to-rad 60)))
          (result nil))
      (when (> left-front agc) (push :left result))
      (when (> right-front agc) (push :right result))
      ;; example (list :left :right) 
      result)))

(defmethod compute-search-light-drive-commands ((k kytron5))
  "simulation of light sensors"
  ;; automatic gain control (agc) ignored in this version of the simulator
  (let ((agc 1)
        (lights (mapcar #'(lambda (x) (light-source-angle k x)) 
                        (light-sources clim:*application-frame*))))
    (let ((right-front (collect-light-values lights (degree-to-rad -90) (degree-to-rad 10)))
          (left-front (collect-light-values lights (degree-to-rad -10) (degree-to-rad 90)))
          (right-rear (collect-light-values lights (degree-to-rad -190) (degree-to-rad -90)))
          (left-rear (collect-light-values lights (degree-to-rad 90) (degree-to-rad 190)))
          (result nil))
      (when (> left-front agc) (push :left result))
      (when (> right-front agc) (push :right result))
      (when (> left-rear agc) (push :left-rear result))
      (when (> right-rear agc) (push :right-rear result))
      ;; example result: (list :left :right) 
      result)))

(defmethod timer-commands ((k kytron))
  "interpret the main timer"
  ;; default, adequat for kytron5
  (if (output-level (timer k)) (list :left :right) nil))

(defmethod timer-commands ((k kytron3))
  "interpret the main timer"
  (let ((timer-contents (output-levels (timer k))))
    (cond ((first timer-contents) (list :left :right))
          ((second timer-contents) (list :right))
          (t nil))))



;;; -----------------------------------------------------
;;; (8)----- geometry --------------------------------


(defclass geometric-object ()
  ((region :accessor region))
  (:documentation "geometric object for building 2-dim constructions"))
   
(defclass filled-object ()
  ((color :accessor color :initarg :color))
  (:default-initargs :color 'white)
  (:documentation "geometric mixin for an object filled with a color"))

(defclass coord (geometric-object)
  ((x :accessor x :initarg :x) 
   (y :accessor y :initarg :y))
  (:default-initargs :x 0 :y 0)
  (:documentation "a single point"))

(defmethod initialize-instance :after ((go coord) &rest keys)
  (declare (ignore keys))
  (update-geometric-object go))

(defmethod update-geometric-object ((go coord))
  (setf (region go) (clim:make-point (x go) (y go))))

(defmethod add-coords ((p1 coord) (p2 coord))
  (make-coord (+ (x p1) (x p2)) (+ (y p1) (y p2))))

(defmethod rotate ((2-vector coord) angle)
  (make-coord
   (- (* (x 2-vector) (cos angle)) (* (y 2-vector) (sin angle)))
   (+ (* (x 2-vector) (sin angle)) (* (y 2-vector) (cos angle)))))

(defmethod nrotate ((2-vector coord) angle)
  (let ((x (x 2-vector))
        (y (y 2-vector)))
  (setf (x 2-vector) (- (* x (cos angle)) (* y (sin angle))))
  (setf (y 2-vector) (+ (* x (sin angle)) (* y (cos angle))))
  (update-geometric-object 2-vector)
  2-vector))

(defmethod scale ((p coord) factor)
  (make-coord
   (* (x p) factor)
   (* (y p) factor)))

(defmethod nscale ((p coord) factor)
  (setf (x p) (* (x p) factor))
  (setf (y p) (* (y p) factor))
  (update-geometric-object p)
  p)

(defmethod translate ((p coord) x y)
  (make-coord (+ (x p) x) (+ (y p) y)))

(defmethod nround-coord ((p coord))
  (setf (x p) (round (x p)))
  (setf (y p) (round (y p)))
  (update-geometric-object p)
  p)
;;; ----------
   
(defclass box (geometric-object)
  ((left :reader left :initarg :left)
   (top :reader top :initarg :top)
   (right :reader right :initarg :right)
   (bottom :reader bottom :initarg :bottom))
  (:default-initargs :left 0 :top 0 :right 0 :bottom 0))

(defmethod initialize-instance :after ((go box) &rest keys)
  (declare (ignore keys))
  (setf (region go) (clim:make-rectangle* 
                     (left go)
                     (top go)
                     (right go)
                     (bottom go))))

(defclass filled-box (box filled-object)
  ()
  (:default-initargs :color 'white))

(defclass circle (geometric-object)
  ((radius :reader radius :initarg :radius)
   (centre :reader centre :initarg :centre))
  (:default-initargs :radius 1 :centre (make-coord 0 0)))

(defmethod initialize-instance :after ((go circle) &rest keys)
  (declare (ignore keys))
  (setf (region go) (clim:make-ellipse*  
                     (x (centre go))
                     (y (centre go))
                     (radius go)
                     (radius go)
                     (radius go)
                     (radius go))))

(defclass filled-circle (circle filled-object)
  ()
  (:default-initargs :color 'white))

(defclass polygon (geometric-object)
  ((points :reader points :initarg :points))
  (:default-initargs :points (list 0 0 0 1 1 1 1 0)))

(defmethod initialize-instance :after ((go polygon) &rest keys)
  (declare (ignore keys))
  (setf (region go) (clim:make-polygon*  
                     (points go))))

(defclass filled-polygon (polygon filled-object)
  ()
  (:default-initargs :color 'green))

;;; constructors: -------------------

(defun make-coord (x y)
  (make-instance 'coord :x x :y y))

(defun make-box (left top right bottom)
  (make-instance 'box :left left :top top 
                 :right right :bottom bottom))

(defun make-filled-box (left top right bottom c)
  (make-instance 'filled-box :left left :top top 
                 :right right :bottom bottom :color c))

(defun make-circle (radius centre)
  (make-instance 'circle :radius radius :centre centre))

(defun make-filled-circle (radius centre c)
  (make-instance 'filled-circle :radius radius :centre centre :color c))

(defun make-polygon (points)
  (make-instance 'polygon 
                 :points (mapcan #'(lambda (p) (list (x p) (y p))) points)))

(defun make-filled-polygon (points color)
  (make-instance 'filled-polygon 
                 :color color
                 :points (mapcan #'(lambda (p) (list (x p) (y p))) points)))

(defun make-polygon* (points)
  (make-instance 'polygon 
                 :points points))

(defun make-filled-polygon* (points color)
  (make-instance 'filled-polygon 
                 :color color
                 :points points))

;;; -------------------------------------------------------
;;; (9)----- the CLIM GUI ---------------------------------
;;; -------------------------------------------------------

(clim:define-application-frame kytron-on-the-moon ()
      ((moon :initarg :moon :accessor moon)
       (moon-presentation :initform nil :accessor moon-presentation)
       (light-sources :accessor light-sources :initarg :light-sources)
       (kytrons :initarg :kytrons :accessor kytrons)
       (scale-factor :initarg :scale-factor :accessor scale-factor))
   (:panes (simulation :application 
                   :default-text-style 
                   (clim:make-text-style :sans-serif :roman :very-small)
                   :display-function 'draw-simulation-pane
                   :incremental-redisplay t
                   ;;:scroll-bars :both
                   ;;:end-of-line-action :allow
                   ;;:end-of-page-action :allow
                   )
            ;(interactor :interactor)
            (documentation :pointer-documentation :default-text-style 
                   (clim:make-text-style :sans-serif :italic :very-small))
            (menu :command-menu
                   :default-text-style '(:sans-serif :bold :small)))
   (:layouts ((default (:column 1
                               (:row :rest
                                     (:column :rest
                                              (simulation :rest)
                                              (documentation :compute))
                                     ;(interactor 0.2)
                                     )
                               (menu :compute)))))
   (:default-initargs :moon nil :kytrons nil :scale-factor 1)
   (:documentation "KYTRON simulation"))

(setq clim::*commands-in-menubar* nil) ;; we don't use the Mac menu bar here

#+:mcl ;;a patch to get the scroll bars active: (still buggy! dragging-feedback drawn incorrect in MCL-CLIM 1.1 when scrolled, + "extent" incorrect)
(defmethod clim:redisplay-frame-pane :after ((f kytron-on-the-moon) pane &rest keys)
  (declare (ignore pane keys))
  (clim::redisplay-decorations (clim:get-frame-pane f 'simulation)))

(defvar *cast-root-window* nil)

(defvar *tty* *standard-output*)

(defvar *moon* (make-random-moon-surface))

(defvar *kytron-on-the-moon* nil)

(defun open-kytron (&optional (reinit nil)  (new-landscape nil))
  (when new-landscape (setq *moon* nil))
  (if (or reinit new-landscape (not (typep *kytron-on-the-moon* 'clim:application-frame)))
    (setq *kytron-on-the-moon* (init-kytron))
    (clim:run-frame-top-level *kytron-on-the-moon*)))

(defun init-kytron ()
  (unless (and (streamp *cast-root-window*)
               ;(open-stream-p *cast-root-window*)
               )
    (setq *cast-root-window*
          (clim:find-port #+:lucid :clx #+:mcl :mcl #+:genera :sheet #+:allegro-v4.1 :clx
                                 #-(or :lucid :mcl :allegro-v4.1 :genera)
                                 (error "Unhandled CLIM-variant: 
Please edit the function init-kytron's call to clim:open-root-window"))))
  (setq *tty* *terminal-io*)
  (unless (typep *moon* 'moon)
    (setq *moon* (make-random-moon-surface))) 
  (let ((frame 
         (clim:make-application-frame 'kytron-on-the-moon
                                      :pretty-name "KYTRONs on the Moon"
                                      :frame-class 'kytron-on-the-moon
                                      :parent *cast-root-window*
                                      :height 360
                                      :width 600
                                      :kytrons (list (make-kytron 2))
                                      :moon *moon*
                                      :light-sources (list (make-light 400 400 1000)))))
    (clim:run-frame-top-level frame)
    frame))

;;; ----------------------------------------------------------------
;;;(10)----- presentation by drawing -------------------------------

(defclass iconic-view (clim:view)
  ()
  (:documentation "CLIM view for graphical output"))

(defconstant +iconic-view+ (make-instance 'iconic-view))


;; using incremental redisplay via updating-output:

(defmethod draw-simulation-pane ((frame kytron-on-the-moon) stream)
  (clim:with-scaling (stream (scale-factor frame) )
    (setf (moon-presentation frame)
          (draw-moon (moon frame) stream))
    (draw-light-sources (light-sources frame) stream)
    (clim:updating-output 
     (stream :unique-id 'kytrons ; any KYTRON to be redrawn?
             :cache-value (loop for kytron in (kytrons frame) collect
                                (list (x (direction kytron)) 
                                      (y (direction kytron))
                                      (x (location kytron)) 
                                      (y (location kytron))))
             :cache-test #'equal)
     (dolist (kytron (kytrons frame))
       (clim:updating-output 
        (stream :unique-id kytron ; which KYTRON to be redrawn?
                :cache-test #'equal
                :cache-value ; direction+location define whether a KYTRON changed
                (list (x (direction kytron)) 
                      (y (direction kytron))
                      (x (location kytron)) 
                      (y (location kytron))))
        (draw-kytron kytron stream))))))

(defmethod draw-kytron ((k t) stream)
  (declare (ignore stream))
  )

(defmethod draw-kytron ((k kytron) stream)
 (setf (output-record k)
  (clim:with-translation (stream (x (location k)) (y (location k)))
    (clim:with-rotation (stream (atan (y (direction k)) (x (direction k))))
      (clim:present k 'kytron :stream stream 
                    :view +iconic-view+
                    :single-box :highlighting)))))

(defmethod make-kytron-transformation ((k kytron))
  (clim:compose-rotation-with-transformation 
   (clim:make-translation-transformation (x (location k)) (y (location k)))
   (atan (y (direction k)) (x (direction k)))))

(clim:define-presentation-method clim:present 
  (k (type kytron) stream
     (view iconic-view) &key)
  (simple-draw-kytron k stream))

(defmethod simple-draw-kytron ((k kytron) stream)
  (dolist (g (graphics k))
    (draw-geometric g stream)))

(clim:define-presentation-method clim:present 
  (k (type kytron) stream
     view &key)
  (declare (ignore view))
  (format stream "~s:~s" k (name k)))

(clim:define-presentation-method clim:present 
  (k (type kytron) stream
     (view clim:textual-view) &key)
  (declare (ignore view))
  (format stream "~s" (name k)))

(defmethod draw-moon ((m t) stream)
  (declare (ignore stream))
  (error "fatal rm error"))

(defmethod draw-moon ((m moon) stream)
  (clim:updating-output 
   (stream :unique-id m 
           :cache-value (length (elements m)) ;changes for ADD and DELETE commands
           :cache-test #'=)                   ; for MOVE no redisplay needed here
   (map nil #'(lambda (x) 
                (setf (output-record x)
                      (clim:updating-output 
                       (stream :unique-id x
                               :cache-value t)
                       (clim:present x (class-name (class-of x))
                                     :stream stream 
                                     :view +iconic-view+))))
        (elements m))))

(defun draw-light-sources (light-sources stream)
  (clim:updating-output 
   (stream :unique-id 'light-sources 
           :cache-value (length light-sources) ;changes for ADD and DELETE commands
           :cache-test #'=)                    ; for MOVE no redisplay needed here
   (map nil #'(lambda (x) 
                (setf (output-record x)
                      (clim:updating-output 
                       (stream :unique-id x
                               :cache-value t)
                       (clim:present x (class-name (class-of x))
                                     :stream stream 
                                     :view +iconic-view+))))
        light-sources)))

(clim:define-presentation-method clim:present 
  (m (type mount) stream
     (view iconic-view) &key)
  (draw-obstacle m stream))

(clim:define-presentation-method clim:present 
  (m (type mount) stream
     view &key)
  (declare (ignore view))
  (format stream "~s" m))

(defmethod draw-obstacle ((m mount) stream)
  (clim:with-translation (stream (x (location m)) (y (location m)))
    (draw-geometric (outer m) stream)))

(defmethod draw-obstacle :after ((m crater) stream)
  (clim:with-translation (stream (x (location m)) (y (location m)))
    (draw-geometric (inner m) stream)))

(clim:define-presentation-method clim:present 
  (m (type light-source) stream
     (view iconic-view) &key)
  (draw-obstacle m stream))

(clim:define-presentation-method clim:present 
  (m (type light-source) stream
     view &key)
  (declare (ignore view))
  (format stream "~s" m))

(defmethod draw-obstacle ((object light-source) stream)
  (clim:draw-circle* stream (x (centre object)) (y (centre object))
                     10 :filled t :ink (translate-color 'yellow))
  (clim:draw-circle* stream (x (centre object)) (y (centre object))
                     14 :filled nil :ink (translate-color 'yellow))
  (clim:draw-circle* stream (x (centre object)) (y (centre object))
                     20 :filled nil :ink (translate-color 'yellow))
  ;; this makes it slow but informative:
  (clim:draw-circle* stream (x (centre object)) (y (centre object))
                     (radius object) :filled nil :ink (translate-color 'yellow)))

;;;  ------------------------------------------------------------
;;;(11)----- basic geometric drawing ----------------------------

;; this whole chapter could be replaced by two calls to clim:draw-design, 
;;  one for filled and one for non-filled objects, if it only worked in CLIM 1.0 beta
;;  --> to do in 1.1

(defmethod draw-geometric (object stream)
  (declare (ignore object))
  (declare (ignore stream))
  nil)

(defmethod draw-geometric ((object box) stream)
  (clim:draw-rectangle* stream
                          (left object)
                          (top object)
                          (right object)
                          (bottom object)
                          :filled nil
                          :ink clim:+foreground-ink+))

(defmethod draw-geometric ((object filled-box) stream)
  (clim:draw-rectangle* stream
                          (left object)
                          (top object)
                          (right object)
                          (bottom object)
                          :filled t
                          :ink (translate-color (color object))))

(defmethod draw-geometric ((object circle) stream)
  (clim:draw-circle* stream
                          (x (centre object))
                          (y (centre object))
                          (radius object)
                          :filled nil
                          :ink clim:+foreground-ink+))

(defmethod draw-geometric ((object filled-circle) stream)
  (clim:draw-circle* stream
                          (x (centre object))
                          (y (centre object))
                          (radius object)
                          :filled t
                          :ink (translate-color (color object))))

(defmethod draw-geometric ((object polygon) stream)
  (clim:draw-polygon* stream (points object) :closed t))

(defmethod draw-geometric ((object filled-polygon) stream)
  (clim:draw-polygon* stream (points object) :closed t 
                      :ink (translate-color (color object))))

;; this function allows easy experimentation with how colors look like
;; (one single place to edit)
(defun translate-color (color-name)
  (case color-name
    (cyan clim:+CYAN+)
    (magenta clim:+MAGENTA+)
    (yellow clim:+YELLOW+)
    (blue clim:+BLUE+)
    (red clim:+RED+)
    (green clim:+GREEN+)
    (orange clim:+ORANGE+)
    (dark-orange clim:+DARK-ORANGE+)
    (black clim:+BLACK+)
    (white clim:+WHITE+)
    (dark-gray clim:+DIM-GRAY+)
    (middle-gray clim:+GRAY+)
    (light-gray clim:+LIGHT-GRAY+)
    (gray clim:+GRAY+)
    (t (warn "color-name ~s not translated." color-name) color-name)))

;;; ------------------------------------------------------------
;;;(12)----- CLIM commands -------------------------------------

(clim:define-gesture-name :shift-click :pointer-button (:left :shift))

(clim:define-gesture-name :option-click :pointer-button (:left :meta))

(clim:define-gesture-name :control-click :pointer-button (:left :control))

;;; did anyone of the CLIM gurus notice that not every computer has META, HYPER, SUPER, MIDDLE, RIGHT buttons?
;;;  - afterall, is this a portable definition if you use such exotics ???

(define-kytron-on-the-moon-command (com-quit-kytron-on-the-moon :menu t) ()
  (clim:frame-exit clim:*application-frame*))

(define-kytron-on-the-moon-command (com-step-forward :menu t :name "Step Forward") 
                                   ((k 'kytron))
  (step-forward k))

(define-kytron-on-the-moon-command (com-trigger-timer :menu t :name "Trigger Timer") 
                                   ((k 'kytron))
  (trigger (timer k)))

(define-kytron-on-the-moon-command (com-step :menu t :name "Step") ((k 'kytron))
  (one-step k))

;; the only explicitly defined translator (only to have tried it):
(clim:define-presentation-to-command-translator kytron-active 
  (kytron com-step kytron-on-the-moon :echo nil
          :pointer-documentation "Execute one simulation step")
  (object)
  (list object))

;; here is an implicit translator definition:
(define-kytron-on-the-moon-command (com-turn-left :menu t :name "Turn Left") 
                                   ((k 'kytron :gesture :shift-click))
  (turn-left k))

#|
(clim:define-presentation-to-command-translator kytron-left 
  (kytron com-turn-left kytron-on-the-moon :echo nil
          :gesture :shift-click
          :pointer-documentation "Turn the KYTRON left")
  (object)
  (list object))
|#

;; here is another implicit translator definition:
(define-kytron-on-the-moon-command (com-turn-right :menu t :name "Turn Right") 
                                   ((k 'kytron :gesture :option-click))
  (turn-right k))

#|
(clim:define-presentation-to-command-translator kytron-right 
  (kytron com-turn-right kytron-on-the-moon :echo nil
          
          :pointer-documentation "Turn the KYTRON right")
  (object)
  (list object))
|#

(define-kytron-on-the-moon-command (com-add-crater :menu t :name "Add Crater") ()
  (push (make-random-crater) (elements (moon clim:*application-frame*)))
  (clim:redisplay-frame-pane clim:*application-frame* 
                           'simulation :force-p nil))

(define-kytron-on-the-moon-command (com-add-mountain :menu t :name "Add Mountain") ()
  (push (make-random-mount) (elements (moon clim:*application-frame*)))
  (clim:redisplay-frame-pane clim:*application-frame* 
                           'simulation :force-p nil))

(define-kytron-on-the-moon-command (com-add-light :menu t :name "Add Light Source") ()
  (push (make-light (random 400) (random 400) (+ 300 (random 500)))
        (light-sources clim:*application-frame*))
  (clim:redisplay-frame-pane clim:*application-frame* 
                           'simulation :force-p nil))

(define-kytron-on-the-moon-command (com-add-kytron-2 :name "Add KYTRON 2" :menu t) ()
  (push (make-kytron 2) (kytrons clim:*application-frame*))
  (clim:redisplay-frame-pane clim:*application-frame* 
                           'simulation :force-p nil))

(define-kytron-on-the-moon-command (com-add-kytron-3 :name "Add KYTRON 3" :menu t) ()
  (push (make-kytron 3) (kytrons clim:*application-frame*))
  (clim:redisplay-frame-pane clim:*application-frame* 
                           'simulation :force-p nil))

(define-kytron-on-the-moon-command (com-add-kytron-4 :name "Add KYTRON 4" :menu t) ()
  (push (make-kytron 4) (kytrons clim:*application-frame*))
  (clim:redisplay-frame-pane clim:*application-frame* 
                           'simulation :force-p nil))

(define-kytron-on-the-moon-command (com-add-kytron-5 :name "Add KYTRON 5" :menu t) ()
  (push (make-kytron 5) (kytrons clim:*application-frame*))
  (clim:redisplay-frame-pane clim:*application-frame* 
                           'simulation :force-p nil))

;; here is an implicit translator definition:
(define-kytron-on-the-moon-command (com-delete-mountain :menu t :name "Delete Mountain") 
                                   ((c 'mount :gesture :control-click))
  (setf (elements (moon clim:*application-frame*))
        (delete c
         (elements (moon clim:*application-frame*))))
  (clim:redisplay-frame-pane clim:*application-frame* 
                           'simulation :force-p nil))

;; here is another implicit translator definition:
(define-kytron-on-the-moon-command (com-delete-light :menu t :name "Delete Light Source") 
                                   ((c 'light-source :gesture :control-click))
  (setf (light-sources clim:*application-frame*)
        (delete c (light-sources clim:*application-frame*)))
  (clim:redisplay-frame-pane clim:*application-frame* 
                           'simulation :force-p nil))

;; here is another implicit translator definition:
(define-kytron-on-the-moon-command (com-delete-kytron :menu t :name "Delete KYTRON") 
                                   ((k 'kytron :gesture :control-click :documentation
                "Delete this KYTRON"))
  (setf (kytrons clim:*application-frame*)
        (delete k (kytrons clim:*application-frame*)))
  (clim:redisplay-frame-pane clim:*application-frame* 
                           'simulation :force-p nil))

;; moving of objects using their stored output-records
(define-kytron-on-the-moon-command (com-move :menu t :name "Move Object") 
                                   ((m 'movable))
  (let ((stream (clim:get-frame-pane clim:*application-frame* 'simulation)))
    (multiple-value-bind (x y)
	(clim:drag-output-record stream 
                                     (output-record m) 
                                     :repaint t :finish-on-release t)
      (setf (location m) (scale (make-coord x y) ;not quite correct - should have used 
                                                 ; the diff of old (x y) and new (x y) to move the
                                                 ; location - to be fixed someday...
                            (/ 1 (scale-factor clim:*application-frame*)))))))

(define-kytron-on-the-moon-command (com-new-landscape :menu t) ()
  (setf (moon clim:*application-frame*)
        (setf *moon* (make-random-moon-surface)))
  (clim:redisplay-frame-pane clim:*application-frame* 
                           'simulation :force-p t))

(define-kytron-on-the-moon-command (com-zoom-in :menu t) ()
  (setf (scale-factor clim:*application-frame*)
        (* 2 (scale-factor clim:*application-frame*)))
  (clim:redisplay-frame-pane clim:*application-frame* 
                           'simulation :force-p t))

(define-kytron-on-the-moon-command (com-zoom-out :menu t) ()
  (setf (scale-factor clim:*application-frame*)
        (/ (scale-factor clim:*application-frame*) 2))
  (clim:redisplay-frame-pane clim:*application-frame* 
                           'simulation :force-p t))


(define-kytron-on-the-moon-command (com-run-simulation :menu t) ()
  ;; the main simulation command
  (block loops
    (dotimes (i 100000) ;; should it be an infinite loop?
      ;; One simu step for all KYTRONs:
      (dolist (k (kytrons clim:*application-frame*))
        (one-step k))
      (clim:redisplay-frame-pane clim:*application-frame* 
                                 'simulation :force-p nil)
      ;; abort when we see an event in the application pane
      (when (let* ((p (clim:get-frame-pane clim:*application-frame* 'simulation))
                   (e (clim:read-gesture :stream p :peek-p nil :timeout 0)))
              ;; lengthy work-around for a MCL-CLIM 1.1 bug of read-gesture
              #+:mcl
              (and e (eq (clim:event-window e) p)) ;;shouldn't happen that we get an event from a different pane here!
              #-:mcl
              e)
        (return-from loops nil)))
    (clim:beep)))




;;; ----------------------------------------------------------------
;;;(13)----- hacks for obstacle detection --------------------------

;; to do: update for using the stored output-record in any "movable"

(defvar *touched-records* nil)  ; for debugging only

(defmethod check-environment ((k kytron) &optional (f clim:*application-frame*))
  (let ((candidates (check-env k (moon-presentation f))))
    (loop for c in candidates 
        ;;  when (clim:region-intersects-region-p ;not in CLIM 1.1 !!!!!!!!!!!!!
        ;;        (third c)  ;now mounts are movable, this won't work anyway...
        ;;        (region (first (find-objects-from-output-record (second c)))))
          collect (first c))))

(defmethod check-env ((k kytron) (moon-representation clim:output-record))
  ;; fast check using CLIM's output-recording and bounding-rectangles
  (let ((other-kytrons-records 
         (loop for ky in (kytrons clim:*application-frame*) when (not (eql k ky))
               collect (output-record ky)))
        (touched-records nil) 
        (transformation 
         (clim:compose-transformations 
          (clim:make-scaling-transformation (scale-factor clim:*application-frame*)
                                            (scale-factor clim:*application-frame*))
          (make-kytron-transformation k))))
    (dolist (bumper (bumpers k))
      (let ((bumper-region 
             (clim:transform-region transformation (region (geometry bumper)))))
        (dolist (ky-rec other-kytrons-records)
          (when (clim:region-intersects-region-p (clim:bounding-rectangle bumper-region)
                                                 (clim:bounding-rectangle ky-rec))
            (push (list (bumper-name bumper) ky-rec bumper-region) touched-records)))
        (clim:map-over-output-records-overlapping-region
         moon-representation 
         bumper-region
         #'(lambda (record) 
             (push (list (bumper-name bumper) record bumper-region) touched-records)))))
    ;(setq *touched-records* touched-records)
    ;(format *tty* "~%Touched: ~s" touched-records)
    (values touched-records)))

(defmethod check-env ((k kytron) (moon moon))
  ;; sometimes draw-moon returns the moon object itself instead of its output-record
  ;; this is the hack to handle this exception
  (warn "Incorrect arg of type moon instead of output-record to CHECK-ENV.")
  (let ((output-record 
         (find-output-record-of-object  
          (clim:stream-output-history  
           (clim:get-frame-pane clim:*application-frame* 'simulation)) moon)))
    (unless (typep output-record 'clim:output-record)
      (error "~s not output-record" output-record))
    (check-env k output-record)))

(defun find-output-record-of-object (root object)
  (clim:map-over-output-records
   root 
   #'(lambda (x) 
       (cond ((and (typep x 'clim:presentation)
                   (eql (clim:presentation-object x) object))
              (return-from find-output-record-of-object x))
             ((typep x 'clim:output-record)
              (find-output-record-of-object x object))
             (t nil)))))

(defmethod find-objects-from-output-record ((record clim:output-record))
  (let ((result nil))
    (clim:map-over-output-records
     record 
     #'(lambda (x) 
         (cond ((typep x 'clim:presentation)
                (push (clim:presentation-object x) result))
               ((typep x 'clim:output-record)
                (setf result (append (find-objects-from-output-record x) result)))
               (t nil))))
    result))

(defmethod find-objects-from-output-record ((record clim:presentation))
  ;; handle the case that record is already a presentation
  (list (clim:presentation-object record)))

;;; --------------------------------------------------------
;;;(14)----- initialization --------------------------------


;evaluate (open-kytron) to start the simulation application

"rudolf mittelmann        A-4040 Linz Austria Europe          2 - 1993"
;;; -------------- end-of-file ----------------------------------------
