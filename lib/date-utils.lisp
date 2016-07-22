
;; date-util
;; day-of-week  {{{

(defun day-of-week (date)
  (first date))

;; }}}
;; year  {{{

(defun year (date)
  (second date))

;; }}}
;; month  {{{

(defun month (date)
  (third date))

;; }}}
;; day  {{{

(defun day (date)
  (fourth date))

;; }}}
;; hour  {{{

(defun hour (date)
  (fifth date))

;; }}}
;; minute  {{{

(defun minute (date)
  (sixth date))

;; }}}
;; sec  {{{

(defun sec (date)
  (sixth date))

;; }}}
;; date  {{{

(defun date ()
  (cddr (reverse (multiple-value-list
                   (decode-universal-time (get-universal-time) -9)))))

;; Examples: {{{
;;
;; (let ((date (date)))
;;   (print (date))
;;   (print (list (day-of-week date)
;;                (year date)
;;                (month date)
;;                (day date)
;;                (hour date)
;;                (minute date)
;;                (sec date))))
;; NOTE:
;; day-of-week
;; => 0:mon, 1:tue, 2:wed, 3:thu, 4:fri, 5:sat, 6:sun

;; }}}


;; }}}

