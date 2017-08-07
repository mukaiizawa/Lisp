#|
 | カレンダー
 |#

(require :stdlib *module-stdlib*)
(require :date-utils *module-date-utils*)

(defparameter usage
  (usage :title "cal [[year] month]"
         :desc '("Display a calendar."
                 "Without any arguments, display the current year of month.")))

(defexe cal ()
  (let* ((args (rest (args)))
         (dt (make-date-time))
         (year (year dt))
         (month (month dt))
         (date 1))
    (cond ((= (length args) 1) (setf month (parse-int (car args))))
          ((= (length args) 2) (setf year (parse-int (car args))
                                     month (parse-int (cadr args))))
          ((/= (length args) 0) (funcall usage)))
    (setf dt (make-date-time year month date))
    (echo (year dt) "-" (month dt))
    (echo  "Su Mo Tu We Th Fr Sa")
    (princ (make-string (* (day-of-week dt) 3) :initial-element #\space))
    (dorange (i 1 (month-day dt))
      (format t "~2@A " i)
      (when (= (mod (+ i (day-of-week dt)) 7) 0) (princ #\newline)))))
