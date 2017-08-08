(require :stdlib *module-stdlib*)
(provide :date-utils)

(defstruct date-time
  utc
  year
  month
  date
  hour
  minute
  sec
  day-of-week)

(defmethod init-date-time ((dt date-time) &rest args)
  (defun init-utc (dt utc)
    (multiple-value-bind
      (sec minute hour date month year day-of-week daylight-p zone)
      (decode-universal-time utc -9)
      (declare (ignorable daylight-p zone day-of-week))
      (setf (date-time-utc dt) utc
            (date-time-year dt) year
            (date-time-month dt) month
            (date-time-date dt) date
            (date-time-hour dt) hour
            (date-time-minute dt) minute
            (date-time-sec dt) sec
            (date-time-day-of-week dt) (mod (1+ day-of-week) 7)))
    dt)
  (defun init-now (dt) (init-utc dt (get-universal-time)))
  (defun init-year-month-date (dt year month date)
    (init-utc dt (encode-universal-time 0 0 0 date month year -9)))
  (cond ((= (length args) 3)
         (init-year-month-date dt (car args) (cadr args) (caddr args)))
        ((= (length args) 1) (init-utc dt (car args)))
        ((null args) (init-now dt))
        (t (error "init-date-time: Illegal arguments '~A'." args))))

(defmethod year ((dt date-time))
  (date-time-year dt))

(defmethod month ((dt date-time))
  (date-time-month dt))

(defmethod date ((dt date-time))
  (date-time-date dt))

(defmethod hour ((dt date-time))
  (date-time-hour dt))

(defmethod minute ((dt date-time))
  (date-time-minute dt))

(defmethod sec ((dt date-time))
  (date-time-sec dt))

(defmethod day-of-week ((dt date-time))
  "0:sun, 1:mon, 2:tue, 3:wed, 4:thu, 5:fri, 6:sat"
  (date-time-day-of-week dt))

(defmethod leap-year? ((dt date-time))
  (let ((year (year dt)))
    (or
      (and (= (mod year 4) 0)
           (/= (mod year 100) 0))
      (= (mod year 400) 0))))

(defmethod year-day ((dt date-time))
  (if (leap-year? dt) 366 365))

(defmethod month-day ((dt date-time))
  (let ((month (month dt)))
    (cond ((= month 2) (if (leap-year? dt) 29 28))
          ((find month '(4 6 9 11)) 30)
          (t 31))))

(defmethod next-day ((dt date-time))
  (let ((next-day (make-instance 'date-time)))
    (init-date-time next-day (+ (date-time-utc dt) (* 60 60 24)))))
