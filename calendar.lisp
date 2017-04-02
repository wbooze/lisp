(in-package :clim-user)

(defvar *day-of-the-week-string* '((0 . "Mon")(1 . "Tue")
																	 (2 . "Wed")(3 . "Thu")
																	 (4 . "Fri")(5 . "Sat")
																	 (6 . "Sun")))

(defun day-of-the-week-string (day-of-week)
	(cdr (assoc day-of-week  *day-of-the-week-string*)))

(defvar *days-in-month* '((1 . 31)(2 . 28) ( 3 . 31)( 4 . 30)
													(5 . 31)(6 . 30) ( 7 . 31)( 8 . 31)
													(9 . 30)(10 . 31)(11 . 30)(12 . 31))
	"alist whose first element is numeric value returned by
decode-universal-time and second is the number of days in that month")

;; In a leap year, the month-length function increments the number of
;; days in February as required 

(defun leap-year-p (year)
	(cond ((and (integerp (/ year 100))
							(integerp (/ year 400)))
				 t)
				
				((and (not (integerp (/ year 100)))
							(integerp (/ year 4)))
				 t)
				
				(t nil)))

(defun month-length (month year)
	(let ((days (cdr (assoc month *days-in-month*))))
		(when (and (eql month 2)
							 (leap-year-p year))
			(incf days))
		days))

(defun calendar-month (month year &key (stream *standard-output*))
	(let ((days-in-month (month-length month year)))
		(multiple-value-bind (sec min hour date month year start-day)
				(decode-universal-time (encode-universal-time 
																0 0 0 1 month year))
			(setq start-day (mod (+ start-day 7) 7))
			
			(clim:formatting-table (stream)
				(clim:formatting-row (stream)
					(dotimes (d 7)
						(clim:formatting-cell (stream :align-x :center)
							(write-string (day-of-the-week-string 
														 (mod (- d 7) 7)) stream))))
				
				(do ((date 1)
						 (first-week t nil))
					((> date days-in-month))
					(clim:formatting-row (stream)
						(dotimes (d 7)
							(clim:formatting-cell (stream :align-x :right)
								(when (and (<= date days-in-month)
													 (or (not first-week) (>= d start-day)))
									(format stream "~D" date)
									(incf date))))))))))

(define-application-frame calendar ()
	()
	(:panes
	 (main :application
				 :width 400 :height 300
				 :display-function 'display-main))
	(:layouts (default main))) 

(define-calendar-command (com-exit-calendar :menu "Exit") ()
	(frame-exit *application-frame*))

(defmethod display-main ((frame calendar) stream &key)
	(multiple-value-bind (sec min hour date month year start-day)
			(decode-universal-time (get-universal-time))
		(calendar-month month year :stream stream)))

(defun run (&optional (stream *standard-output*))
	(run-frame-top-level (make-application-frame 'calendar)))

;; for using in the interactor
;; (calendar-month 3 2014 :stream *standard-output*)
