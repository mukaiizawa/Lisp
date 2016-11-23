
(require :stdlib *module-stdlib*)
(provide :math)

(defun % (n divisor)
  (second (multiple-value-list (floor n divisor))))

(defun fact (n)
  (if (or (= n 0) (= n 1))
    1
    (* n (fact (1- n)))))

(defun sigma (from to fn)
  (apply #'+ (mapcar fn (iota from to))))

(defun pi (from to fn)
  (apply #'* (mapcar fn (iota from to))))

(defun round-to (number precision &optional (fn #'round))
  (let1 (div (expt 10 precision))
    (funcall (if (zerop precision) #'parse-int #'float)
             (/ (funcall fn (* number div)) div))))

