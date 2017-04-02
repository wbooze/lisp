(require "clim")
(in-package :clim-user)

;;; tilt-mazes, a Lisp implementation of the game at
;;; http://www.clickmazes.com/newtilt/ixtilt2d.htm
;;;
;;; Copyright Frank Buss
;;; TODO: solve-maze for Multi-Goal mazes

;;; start text mode game with (clim-user::play-maze (number))
;;; and CLIM game with (clim-user::play-maze-clim)

;; the boards
(defconstant *mazes*
  '((("###########"
      "#o    #   #"
      "# ### #   #"
      "#         #"
      "# #     ###"
      "# #       #"
      "# # ###   #"
      "#         #"
      "# ### # ###"
      "#     #  T#"
      "###########")
     ("Single-Goal Maze S5A (5x5)" "layout by Andrea - Aug 99"))
    (("#############"
      "# #       # #"
      "# # ###   # #"
      "#           #"
      "# # # # ### #"
      "# # #o#     #"
      "# # ##### ###"
      "#     #T#   #"
      "###   # #   #"
      "#           #"
      "#   # #   ###"
      "#   # #     #"
      "#############")
     ("Single-Goal Maze S6A (6x6)" "layout by Andrea - Aug 99"))
    (("#############"
      "#     #     #"
      "#   ###   ###"
      "#           #"
      "### ###   # #"
      "#         #T#"
      "#     #   ###"
      "#o    #     #"
      "# #   ###   #"
      "# #         #"
      "### #   # # #"
      "#   #   # # #"
      "#############")
     ("Single-Goal Maze S6B (6x6)" "layout by Andrea - Sept 99"))
    (("###############"
      "#         #   #"
      "#   # ### #   #"
      "#   #         #"
      "### # # ### ###"
      "#o    #       #"
      "###   #   #   #"
      "#         #   #"
      "#     # ### ###"
      "#     #      T#"
      "# ### #     ###"
      "#   #         #"
      "### # #   ### #"
      "#     #   #   #"
      "###############")
     ("Single-Goal Maze S7A (7x7)" "layout by Andrea - Aug 99"))
    (("###############"
      "# #     #     #"
      "# # ### #   ###"
      "#        o    #"
      "#     #     # #"
      "#     #     # #"
      "# ### # # # # #"
      "#       # #   #"
      "#     ### # ###"
      "#             #"
      "### # # ###   #"
      "#   #T#       #"
      "#   ### #   # #"
      "#       #   # #"
      "###############")
     ("Single-Goal Maze S7B (7x7)" "layout by Andrea - Sept 99"))
    (("###############"
      "#o#   #     # #"
      "# ### #   # # #"
      "#         #   #"
      "###     ###   #"
      "#             #"
      "# ### # # ### #"
      "#     # #     #"
      "### # # #     #"
      "#   #         #"
      "#   ### #   ###"
      "#       #     #"
      "# #   ##### # #"
      "# #         #T#"
      "###############")
     ("Single-Goal Maze S7C (7x7)" "layout by Andrea - Sept 99"))
    (("#################"
      "#   #         # #"
      "#   ### # ### # #"
      "#       #       #"
      "###   ### # # ###"
      "#o        # #   #"
      "###   #   # ### #"
      "#     #         #"
      "#     #       # #"
      "#             # #"
      "# ###     #   ###"
      "#         #    T#"
      "# # ### ###     #"
      "# #             #"
      "# # #   ###   ###"
      "#   #     #     #"
      "#################")
     ("Single-Goal Maze S8A (8x8)" "layout by Andrea - Sept 99"))
    (("###################"
      "#     #       #   #"
      "### # #   ### # ###"
      "#   #           # #"
      "# ###   ### # # # #"
      "#           # #   #"
      "#   #     # # #   #"
      "#   #     #o      #"
      "### # ### ### # ###"
      "#             #   #"
      "#     ### ### #   #"
      "#      T# #       #"
      "# ###   # # ### ###"
      "# #               #"
      "# # ### ###       #"
      "#         #       #"
      "###   ### #   ### #"
      "#       #       # #"
      "###################")
     ("Single-Goal Maze S9A (9x9)" "layout by Andrea - Sept 99"))
    (("###################"
      "# #   #       #   #"
      "# #   ###   # # ###"
      "#           #     #"
      "# ### ##### ### # #"
      "#   #   #       # #"
      "### #   #   #   # #"
      "#           #     #"
      "#   ### # ### ### #"
      "#     # #         #"
      "###   # ###     # #"
      "#       #       # #"
      "# # #   # ###   ###"
      "# # #             #"
      "# # # ### #   #   #"
      "#         #   #   #"
      "# ###     # # ### #"
      "#  o#       #  T# #"
      "###################")
     ("Single-Goal Maze S9B (9x9)" "layout by Andrea - Sept 99"))
    (("#####################"
      "#o#           #   # #"
      "# # #     ### #   # #"
      "#   #               #"
      "#   #   #     #   ###"
      "#       #     #     #"
      "###   ###   #####   #"
      "#     #       #     #"
      "#   ### #     #   # #"
      "#       #         # #"
      "### ### # # ### ### #"
      "#         #       # #"
      "# ### #   #   ### # #"
      "#     #       #     #"
      "#   #####   ###   ###"
      "#     #     #       #"
      "###   #   ### #   ###"
      "#             #     #"
      "#   ###   #   ### # #"
      "#   #     #       #T#"
      "#####################")
     ("Single-Goal Maze S10A (10x10)" "layout by Andrea - Feb 98"))
    (("#####################"
      "#o#     #     #     #"
      "# ###   #     #   ###"
      "#   #             # #"
      "# # # #   # ### ### #"
      "# #   #   #         #"
      "# # ###   #   # ### #"
      "#     #       #     #"
      "#     #   #   #   ###"
      "#         #       # #"
      "### #   #####     # #"
      "# # #     #         #"
      "# # # ### #     ### #"
      "#                   #"
      "# ###   # #   ### ###"
      "#       # #         #"
      "# ##### # ### # #   #"
      "# #           # #   #"
      "### #   # ### # ### #"
      "#   #   #         #T#"
      "#####################")
     ("Single-Goal Maze S10B (10x10)" "layout by Andrea - Feb 98"))
    (("#####################"
      "# #o      #       # #"
      "# ### #   #   ### # #"
      "#     #             #"
      "#     #       #     #"
      "#             #     #"
      "# #     ### ### ### #"
      "# #               # #"
      "# # ### ### #     # #"
      "#         # #       #"
      "### #     # #   ### #"
      "# # #               #"
      "# # #     ### ### # #"
      "#                 # #"
      "# #   ### ###     # #"
      "# #     #           #"
      "# #     ###     # ###"
      "#         #     #   #"
      "###   #   # ### ### #"
      "#     #     #    T# #"
      "#####################")
     ("Single-Goal Maze S10C (10x10)" "layout by Andrea - Sept 99"))
    (("#####################"
      "#o# #     #   #     #"
      "# # #     #   ###   #"
      "# #                 #"
      "# # ### # #     #   #"
      "#       # #     #   #"
      "#   #   # #     # ###"
      "#   #               #"
      "#   #     # ### #   #"
      "#         #     #   #"
      "### ### ##### ### ###"
      "#         #         #"
      "#   ###   # # ### # #"
      "#           #     # #"
      "### ###   # ###   # #"
      "#   #     # #     # #"
      "# # # ### # #   # # #"
      "# #             #   #"
      "# #     # # #   # # #"
      "#       # # #     #T#"
      "#####################")
     ("Single-Goal Maze S10D (10x10)" "layout by Robert Abbott - Sept 99"))
    (("#####################"
      "#   #       #     # #"
      "#   #       # ### # #"
      "#  o#         #     #"
      "# ##### ###   # #   #"
      "# #             #   #"
      "# #     # ###   #   #"
      "#       #           #"
      "#   ### #   ###     #"
      "#           #       #"
      "###   # ### # # ### #"
      "#     #   #   #     #"
      "#   # ### #   #   ###"
      "#   #               #"
      "# ### ###   ###   # #"
      "#       #         # #"
      "###     #       ### #"
      "#               #T  #"
      "# #   ### ### # #   #"
      "# #           # #   #"
      "#####################")
     ("Single-Goal Maze S10E (10x10)" "layout by Andrea - Sept 99"))
    (("###########"
      "#T  #    T#"
      "### # ### #"
      "#       # #"
      "# # #   # #"
      "# # #T    #"
      "# # ###   #"
      "#    o    #"
      "# #   # ###"
      "#T#   #  T#"
      "###########")
     ("Multi-Goal Maze M5A (5x5)" "layout by Andrea - Aug 99"))
    (("###########"
      "#  T   T# #"
      "### ### # #"
      "#T    #  T#"
      "# #   #   #"
      "# #  o    #"
      "# # ### ###"
      "#T  #    T#"
      "### # #####"
      "#  T   T  #"
      "###########")
     ("Multi-Goal Maze M5B (5x5)" "layout by Andrea - Aug 99"))
    (("###########"
      "#T  #    T#"
      "### # #####"
      "# #T   T  #"
      "# #   ### #"
      "#    o    #"
      "# ### # ###"
      "#  T# #T  #"
      "# # # ### #"
      "#T#      T#"
      "###########")
     ("Multi-Goal Maze M5C (5x5)" "layout by Andrea - Aug 99"))
    (("#############"
      "# #   #T    #"
      "# #   # #   #"
      "#T      #   #"
      "### ### # ###"
      "#o#         #"
      "# #   #   # #"
      "#     #   #T#"
      "###   ### ###"
      "#T         T#"
      "# # ### #   #"
      "# #    T#   #"
      "#############")
     ("Multi-Goal Maze M6A (6x6)" "layout by Andrea - Aug 99"))
    (("#############"
      "#T  #      T#"
      "### # # ### #"
      "#     #  o# #"
      "# # ###   # #"
      "#T# #       #"
      "### #     ###"
      "#      T    #"
      "# #   ##### #"
      "# #T       T#"
      "# ###   ### #"
      "#T  #  T#   #"
      "#############")
     ("Multi-Goal Maze M6B (6x6)" "layout by Andrea - Aug 99"))
    (("#############"
      "#T#  T#    T#"
      "# #   # # ###"
      "#  o    # #T#"
      "# ### ### # #"
      "#  T#       #"
      "### #   # ###"
      "#      T#   #"
      "# # ### # # #"
      "#T# #T    # #"
      "### ### ### #"
      "#T    #    T#"
      "#############")
     ("Multi-Goal Maze M6C (6x6)" "layout by Andrea - Aug 99"))
    (("###############"
      "#  T#  T#  T  #"
      "#   #   # ### #"
      "#             #"
      "###   ###     #"
      "#       #     #"
      "# ### # #   ###"
      "#T    #T     T#"
      "#   ### ### ###"
      "#         #  o#"
      "###       #   #"
      "# #           #"
      "# #   #   ### #"
      "#  T  #T  #T  #"
      "###############")
     ("Multi-Goal Maze M7A (7x7)" "layout by Andrea - Aug 99"))
    (("###############"
      "#T  #  T  #   #"
      "# # ###   # # #"
      "# #    o   T# #"
      "# #   # #   # #"
      "#    T# #    T#"
      "### ### # ### #"
      "#       #     #"
      "# # #   ### ###"
      "# #T#    T    #"
      "# # # ### # ###"
      "#T        #  T#"
      "# ### #   #   #"
      "# #  T#       #"
      "###############")
     ("Multi-Goal Maze M7B (7x7)" "layout by Andrea - Sept 99"))
    (("#################"
      "#      T    #T  #"
      "# ##### ### #   #"
      "#T      #T      #"
      "#   #   #   ### #"
      "#   #        T# #"
      "#   # ### #   # #"
      "#      T  #T    #"
      "#   ###   ###   #"
      "#   #          T#"
      "# # #   ###   ###"
      "#T#  T   o      #"
      "###   ###   ### #"
      "#          T#T  #"
      "#     #   ### # #"
      "#  T  #T      #T#"
      "#################")
     ("Multi-Goal Maze M8A (8x8)" "layout by Andrea - Aug 99"))
    (("#################"
      "#        T#     #"
      "# ### #   ###   #"
      "#  T  #        T#"
      "#   ###   # ### #"
      "#         #  T  #"
      "###   #   #     #"
      "#T    #T       T#"
      "#     ###   # # #"
      "#          T# # #"
      "#   #       # ###"
      "#   #  o        #"
      "# # ###       # #"
      "# #  T   T    # #"
      "# ###   # ### # #"
      "#T      #    T  #"
      "#################")
     ("Multi-Goal Maze M8B (8x8)" "layout by Andrea - Aug 99"))
    (("#################"
      "#  T  #    T  #T#"
      "# ### #   # ### #"
      "#      T  #     #"
      "#         #   # #"
      "#            T# #"
      "# # ###     ### #"
      "#T# #T   T      #"
      "### #   #   # ###"
      "#       #o  #  T#"
      "#     ###   #   #"
      "#  T   T        #"
      "#       ###   ###"
      "#        T      #"
      "# # ### ### #   #"
      "#T#  T      #T  #"
      "#################")
     ("Multi-Goal Maze M8C (8x8)" "layout by Andrea - Sept 99"))
    (("#####################"
      "#T       T  #  T#  T#"
      "#   #   ### #   #   #"
      "#   #T              #"
      "### ###   #     ### #"
      "#T#    T  #T   T    #"
      "# #     ###   ###   #"
      "#               #  T#"
      "#   # ### ### # # ###"
      "#  T#        T#     #"
      "# ###     #   #     #"
      "#    T   T#        T#"
      "# # ###   # ### #   #"
      "# #        o   T#   #"
      "# #     #       # ###"
      "#T   T  #           #"
      "# ###   ### #   #   #"
      "#   #       #T  #T  #"
      "### # ###   # # ### #"
      "#T      #T    #    T#"
      "#####################")
     ("Multi-Goal Maze M10A (10x10)" "layout by Andrea - Aug 99"))
    (("#####################"
      "#   #  T#      T#  T#"
      "#   # # # ###   #   #"
      "#  T  #             #"
      "# ### #   ### ### # #"
      "#   #T     T#    T# #"
      "### # #     # # ### #"
      "#T    #T      #T    #"
      "# # ###   #   #   # #"
      "# #      T#o      #T#"
      "# # # # ### # ### # #"
      "#  T# #     #T      #"
      "#   # ###   # ### ###"
      "#      T       T# # #"
      "###   #   ###   # # #"
      "#T    #    T       T#"
      "#     #   # #   ### #"
      "#  T     T# #    T# #"
      "# ###   ### ###   # #"
      "#    T  #    T      #"
      "#####################")
     ("Multi-Goal Maze M10B (10x10)" "layout by Andrea - Sept 99"))))

(defconstant *stone-directions*
  '((#\h -1 0) (#\l 1 0) (#\k 0 -1) (#\j 0 1)))

;; prints a maze
(defun print-maze-array (maze-array)
  (terpri)
  (destructuring-bind (width height) (array-dimensions maze-array)
    (loop for y below height do
          (loop for x below width do
                (princ (case (aref maze-array x y)
                         (stone #\o)
                         (target #\T)
                         (empty #\Space)
                         (otherwise #\#))))
          (terpri))))


;; search a field in the maze and returns (x y) as multiple values
(defun search-maze (maze-array type)
  (destructuring-bind (width height) (array-dimensions maze-array)
    (loop for y from 0 below height do
          (loop for x from 0 below width do
                (when (eql (aref maze-array x y) type)
                  (return-from search-maze (values x y)))))))

;; returns the coordinates of the next wall, starting at (x y), in
;; the direction (dx dy)
(defun search-wall (maze-array x y dx dy)
  (loop until (eql (aref maze-array x y) 'wall) do
        (incf x dx)
        (incf y dy))
  (values (- x dx) (- y dy)))

;; returns the number of targets
(defun count-maze-targets (maze-array)
  (let ((count 0))
    (destructuring-bind (width height) (array-dimensions maze-array)
      (loop for y from 0 below height do
            (loop for x from 0 below width do
                  (when (eql (aref maze-array x y) 'target) (incf count)))))
    count))

;; moves the stone until it tilts to a wall in direction (dx dy)
;; while moving, it removes all targets
(defun move-stone (maze-array dx dy)
  (multiple-value-bind (x y) (search-maze maze-array 'stone)
    (setf (aref maze-array x y) 'empty)
    (multiple-value-bind (x y) (search-wall maze-array x y dx dy)
      (do ((xb x (- xb dx))
           (yb y (- yb dy)))
          ((and (= xb x) (= yb y)))
        (setf (aref maze-array xb yb) 'empty))
      (setf (aref maze-array x y) 'stone))))

;; create a maze array
(defun make-maze-array (number)
  (let* ((maze (car (elt *mazes* number)))
         (width (length (car maze)))
         (height (length maze))
         (maze-array (make-array (list width height)))
         (y 0))
    (dolist (line maze)
      (loop for x from 0 below (length line) do
            (let ((char (elt line x)))
              (setf (aref maze-array x y)
                    (case char
                      (#\o 'stone)
                      (#\T 'target)
                      (#\Space 'empty)
                      (otherwise 'wall)))))
      (incf y))
    maze-array))

;; interactive game play in text mode
(defun play-maze (number)
  (format t "Moving keys are standard VI keys:~%")
  (format t "l: right~%")
  (format t "k: up~%")
  (format t "j: down~%")
  (format t "h: left~%")
  (format t "q: quit~%")
  (do* ((maze-array (make-maze-array number))
        (char nil (read-char))
        (direction nil (cdr (assoc char *stone-directions*))))
       ((eql char #\q) "quit")
    (format t "~A~%" char)
    (if direction 
        (move-stone maze-array (car direction) (cadr direction)))
    (print-maze-array maze-array)
    (when (= (count-maze-targets maze-array) 0) (return "won"))))



;;; CLIM GUI

define-application-frame maze-frame ()
  ((maze-array :initform (make-maze-array 0)
               :accessor maze-array)
   (maze-number :initform 0))
  (:panes
   (display :application
            :display-function 'draw-maze-array
            :scroll-bars nil
            :width 400 :height 400)
   (help text-editor :value
         (format nil
                 (concatenate 'string 
                              "Moving keys are standard VI keys:~%"
                              "l: right~%"
                              "k: up~%"
                              "j: down~%"
                              "h: left~%"
                              "q: quit~%")) :height 150))
  (:layouts
   (default
    (vertically ()
      display 
      help))))

(defun next-maze ()
  (with-slots (maze-array maze-number) *application-frame*
    (incf maze-number)
    (when (= (length *mazes*) maze-number) (setf maze-number 0))
    (setf maze-array (make-maze-array maze-number))))

(define-maze-frame-command (com-exit :keystroke #\q) ()
  (frame-exit *application-frame*))

(defun check-for-win (maze-array)
        (when (= (count-maze-targets maze-array) 0) (next-maze)))

(defmacro define-stone-move-command (name keystroke dx dy)
  `(define-maze-frame-command (,name :keystroke ,keystroke) ()
     (let ((maze-array (maze-array *application-frame*)))
       (move-stone maze-array ,dx ,dy)
       (check-for-win maze-array))))

(define-stone-move-command com-right #\l 1 0)
(define-stone-move-command com-up #\k 0 -1)
(define-stone-move-command com-down #\j 0 1)
(define-stone-move-command com-left #\h -1 0)

(defmethod read-maze-frame-command ((maze maze-frame) &key (stream *standard-input*))
  (let ((abort-chars #+Genera '(#\Abort #\End)
		     #-Genera nil))
    (let ((command (read-command-using-keystrokes
                    (frame-command-table maze) abort-chars
                    :stream stream)))
      (if (characterp command)
	  (frame-exit maze)
        command))))

(defun draw-stone (stream x y cell-width cell-height)
  (let ((half-cell-width (/ cell-width 2))
        (half-cell-height (/ cell-height 2)))
    (draw-ellipse* stream
                   (+ (* x cell-width) half-cell-width)
                   (+ (* y cell-height) half-cell-height)
                   half-cell-width 0
                   0 half-cell-height
                   :ink +red+)))

(defun draw-target (stream x y cell-width cell-height)
  (let ((x0 (+ (* x cell-width) 2))
        (y0 (+ (* y cell-height) 2))
        (x-distance (/ cell-width 5))
        (y-distance (/ cell-height 5)))
    (draw-rectangle* stream 
                     (+ x0 (/ x-distance 2))
                     (+ y0 (/ y-distance 2))
                     (- (+ x0 cell-width) x-distance)
                     (- (+ y0 cell-height) y-distance)
                     :ink +blue+)))

(defun draw-empty (stream x y cell-width cell-height)
  (declare (ignore stream x y cell-width cell-height))
  ())

(defun draw-wall (stream x y cell-width cell-height)
  (let ((x0 (* x cell-width))
        (y0 (* y cell-height)))
    (draw-rectangle* stream x0 y0 (+ x0 cell-width) (+ y0 cell-height))))

(defmethod draw-maze-array ((maze maze-frame) stream &key max-width max-height)
  (declare (ignore max-width max-height))
  (let ((maze-array (maze-array maze)))
    (destructuring-bind (width height) (array-dimensions maze-array)
      (let* ((pane-width (bounding-rectangle-width stream))
             (pane-height (bounding-rectangle-height stream))
             (cell-width (/ pane-width width))
             (cell-height (/ pane-height height)))
        (loop for y below height do
              (loop for x below width do
                    (let ((cell (aref maze-array x y)))
                      (cond
                       ((eql cell 'stone) (draw-stone stream x y cell-width cell-height))
                       ((eql cell 'target) (draw-target stream x y cell-width cell-height))
                       ((eql cell 'empty) (draw-empty stream x y cell-width cell-height))
                       (t (draw-wall stream x y cell-width cell-height))))))))))

(defun play-maze-clim ()
  (let ((frame (make-application-frame 'maze-frame)))
    (run-frame-top-level frame)))



;;; Solving Algorithm
;;; A path looks like this: (x y "hlkj"), where x and y are the current
;;; endpoint and the string are the movements from start.
;;; The algorithm starts with the path (x y ""), where x and y are the
;;; start position of the stone, and adds every iteration all possible
;;; new movements, but only, if the path is shorter. If there are no more
;;; shorter paths for all possible new movements, the search is finished.

;; get all possible movements as new paths
(defun get-possible-new-paths (maze-array path)
  (let ((possible-new-paths nil))
    (destructuring-bind (x y path-string) path
      (dolist (direction *stone-directions*)
        (destructuring-bind (name dx dy) direction
          (multiple-value-bind (x-new y-new) (search-wall maze-array x y dx dy)
            (when (not (and (= x x-new) (= y y-new)))
              (setf possible-new-paths 
                    (cons (list x-new y-new 
                                (concatenate 'string path-string (string name))) 
                          possible-new-paths)))))))
    possible-new-paths))

;; search and return the path for the endposition (x y), or return nil
(defun search-path (paths x y)
  (if (null paths)
      nil
    (let* ((path (car paths))
           (x2 (car path))
           (y2 (cadr path)))
      (if (and (= x x2) (= y y2)) 
          paths
        (search-path (cdr paths) x y)))))

;; creates a new paths list with all possible new movements, which results
;; in shorter paths for the same endpoint
(defun solve-iteration (maze-array paths)
  (let ((new-paths (copy-list paths)))
    (dolist (path paths)
      (dolist (possible-new-path (get-possible-new-paths maze-array path))
        (let* ((existing-path-cons
                (search-path new-paths (car possible-new-path) (cadr possible-new-path)))
               (existing-path (car existing-path-cons)))
          (if existing-path-cons
              (let ((existing-path-string (car (last existing-path)))
                    (possible-path-string (car (last possible-new-path))))
                (when (< (length possible-path-string) (length existing-path-string))
                  (rplaca existing-path-cons possible-new-path)))
            (setf new-paths (cons possible-new-path new-paths))))))
    new-paths))

;; print the solution, if all targets were found
(defun solve-solution (maze-array paths)
  (print-maze-array maze-array)
  (multiple-value-bind (x y) (search-maze maze-array 'target)
    (let ((solution-path (search-path paths x y)))
      (if solution-path
          (format t "solution: ~A" (caddar solution-path))
        (format t "no solution found")))))

;; solve a maze
(defun solve-maze (number)
  (let* ((maze-array (make-maze-array number))
         (paths `((,@(multiple-value-list (search-maze maze-array 'stone)) "")))
         (old-len 0))
    (do ((len 1 (length paths)))
        ((= len old-len) (solve-solution maze-array paths))
      (setf old-len len)
      (setf paths (solve-iteration maze-array paths)))))
