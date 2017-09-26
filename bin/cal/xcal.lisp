#|
 | xcalender
 |#

(require :stdlib *module-stdlib*)
(require :date-time *module-date-time*)

(defparameter *cycle* 28)

(defparameter usage
  (usage :title "xcal [month date]"
         :desc '("Display a xcalendar.")))

(defmethod add-month ((d date-time) (n number))
  (1+ (mod (+ (month d) -1 n) 12)))

(defexe xcal ()
  (unless (or (null args) (= (length args) 2)) (funcall usage))
  (let* ((simple-cal? (null args))
         (today (init-date-time (make-instance 'date-time)))
         (xday (if simple-cal? today (init-date-time (make-instance 'date-time)
                                                     (year today)
                                                     (parse-int (car args))
                                                     (parse-int (cadr args))))))
    (format t "~10@A | Su Mo Tu We Th Fr Sa~%" #\Space)
    (format t "-----------+---------------------~%")
    (do* ((dt (let ((dt (init-date-time (make-instance 'date-time)
                                        (year xday) (month xday) 1)))
                (before-date dt (day-of-week dt)))
              (after-date dt))
          (i (- (+ (* (- (week-number xday) (week-number dt)) *week-day*)
                   (day-of-week xday)))
             (1+ i)))
      ((and (= (day-of-week dt) 0) (= (month dt) (add-month xday 3))))
      (when (= (day-of-week dt) 0)
        (aif (car (filter (lambda (x)
                            (let ((ad (after-date dt x)))
                              (when (= (date ad) 1) ad)))
                          (iota 0 (1- *week-day*))))
          (format t "~4@A-~2@A " (year it) (month it))
          (format t "~8@A" #\Space))
        (format t "~2@A | " (week-number dt)))
      (format t "~2@A " (cond ((date= dt today) "__")
                              ((and (not simple-cal?)
                                    (= (mod i *cycle*) 0)) "XX")
                              (t (date dt))))
      (when (= (1+ (day-of-week dt)) *week-day*) (format t "~%")))))
