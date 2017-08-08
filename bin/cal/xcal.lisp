#|
 | xcalender
 |#

(require :stdlib *module-stdlib*)
(require :date-time *module-date-time*)

(defparameter *calender-cycle* 28)

(defparameter usage
  (usage :title "xcal month date"
         :desc '("Display a xcalendar.")))

(defun month-add (month n)
  (1+ (mod (+ month -1 n) 12)))

(defexe xcal ()
  (when (/= (length args) 2) (funcall usage))
  (let* ((ref-month (parse-int (car args)))
         (ref-day (parse-int (cadr args)))
         (dt (init-date-time (make-instance 'date-time))))
    (format t "~10@A | Su Mo Tu We Th Fr Sa~%" #\Space)
    (format t "-----------+---------------------~%")
    (do* ((ref-date (init-date-time (make-instance 'date-time)
                                    (year dt) ref-month ref-day))
          (dt (before-date (init-date-time dt (year dt) ref-month 1)
                           (date-time-day-of-week dt))
              (after-date dt))
          (i 0 (if (or (plusp i) (date= dt ref-date)) (1+ i) 0)))
      ((= (month-add ref-month 3) (month dt)))
      (when (= (day-of-week dt) 0)
        (aif (car (filter (lambda (x)
                            (let ((ad (after-date dt x)))
                              (when (= (date ad) 1) ad)))
                          (iota 0 6)))
          (format t "~4@A-~2@A " (year it) (month it))
          (format t "~8@A" #\Space))
        (format t "~2@A | " (week-number dt)))
      (format t "~2@A " (if (= (mod i *calender-cycle*) 1) "##" (date dt)))
      (when (= (1+ (day-of-week dt)) *week-day*) (format t "~%")))))
