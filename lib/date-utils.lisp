(require :stdlib *module-stdlib*)

(defstruct date-time
  utc
  year
  month
  date
  hour
  minute
  sec
  day-of-week)

(defun make-date-time (&key (_year 0) (_month 0) (_date 0))
  (multiple-value-bind
    (sec minute hour date month year day-of-week daylight-p zone)
    (decode-universal-time (get-universal-time) -9)
    (declare (ignore daylight-p zone))
    (let ((dt (make-instance 'date-time
                             :utc nil
                             :year (if (= _year 0) year _year)
                             :month (if (= _month 0) month _month)
                             :date (if (= _date 0) date _date)
                             :hour hour
                             :minute minute
                             :sec sec
                             :day-of-week (mod (1+ day-of-week) 7))))
      (setf (date-time-utc dt)
            (encode-universal-time (sec dt)
                                   (minute dt)
                                   (hour dt)
                                   (date dt)
                                   (month dt)
                                   (year dt)
                                   -9))
      dt)))

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
  (or
    (and (= (mod (year dt) 4) 0)
         (/= (mod (year dt) 100) 0))
    (= (mod (year dt) 400) 0)))

(defmethod year-day ((dt date-time))
  (if (leap-year? dt) 366 365))

(defmethod month-day ((dt date-time))
  (cond ((= (month dt) 2) (if (leap-year? dt) 29 28))
        ((find (month dt) '(4 6 9 11)) 30)
        (t 31)))
