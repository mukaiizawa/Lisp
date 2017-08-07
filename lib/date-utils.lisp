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

(defun make-date-time (&optional year month date)
  (let ((default? (every #'null (list year month date))))
    (when (and (not default?) (some #'null (list year month date)))
      (error "make-date-time: illegal arguments."))
    (multiple-value-bind
      (sec minute hour _date _month _year day-of-week daylight-p zone)
      (decode-universal-time (get-universal-time) -9)
      (declare (ignorable daylight-p zone day-of-week))
      (let* ((dt (make-instance 'date-time
                                :year (if default? _year year)
                                :month (if default? _month month)
                                :date (if default? _date date)
                                :hour (if default? hour 0)
                                :minute (if default? minute 0)
                                :sec (if default? sec 0)))
             (utc (encode-universal-time (sec dt)
                                         (minute dt)
                                         (hour dt)
                                         (date dt)
                                         (month dt)
                                         (year dt)
                                         -9))
             (day-of-week (mod (1+ (nth 6
                                        (multiple-value-list
                                          (decode-universal-time utc -9))))
                               7)))
        (setf (date-time-utc dt) utc
              (date-time-day-of-week dt) day-of-week)
        dt))))

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
