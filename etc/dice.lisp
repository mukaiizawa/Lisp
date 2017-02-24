(require :stdlib *module-stdlib*)

(defparameter *time-limit* 6)
(defparameter *dice-num* 2)
(defparameter *dice-table* (mkhash :test 'equal))

(defmacro inject-elapsed-time (start)
  `(- (get-universal-time) start))

(defun dice (n)
  (if (= n 0)
    nil
    (cons (1+ (mod (random 6000000 (make-random-state t)) 6))
          (dice (1- n)))))

(defun lookup-dice-table (dice-val)
  (setf (gethash dice-val *dice-table*)
        (aif (gethash dice-val *dice-table*) (1+ it) 1)))

(defun show-dice-table ()
  (dorange (i *dice-num* (* 6 *dice-num*))
    (echo i ":" (aif (gethash i *dice-table*) it 0))))

(let ((timmer)
      (start)
      (elapsed-time))
  (defun kill-timmer ()
    (process-kill timmer)
    (echo "time: " elapsed-time)
    (values elapsed-time))
  (defun run-timmer ()
    (setq start (get-universal-time)
          timmer (process-run-function "timer"
                                       (lambda ()
                                         (while t
                                           (sleep 1)
                                           (setq elapsed-time (inject-elapsed-time start))
                                           (when (or (= (mod elapsed-time 10) 0)
                                                     (>= elapsed-time (- *time-limit* 10)))
                                             (echo "time: " elapsed-time))
                                           (when (>= elapsed-time *time-limit*)
                                             (write-char #\bell))))))))

(defun dice-with-timer ()
  (while t
    (let* ((input)
           (dices (dice *dice-num*))
           (dice-val (apply #'+ dices)))
      (princ "Press enter to use `hatten card'.") (read-line)
      (lookup-dice-table dice-val)
      (surround (echo (make-string 80 :initial-element #\-))
        (echo "dice: " dice-val #\Space dices))
      (princ "Press enter to start timer.") (read-line)
      (run-timmer)
      (princln "Press enter to stop timer.") (setq input (read-line))
      (kill-timmer)
      (when (string= input "quit")
        (show-dice-table)
        (read-line)))))

(dice-with-timer)
