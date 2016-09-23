
(require "stdlib" *module-stdlib*)
(provide "math")

(defun fact (n)
  (if (or (= n 0) (= n 1))
    1
    (* n (fact (1- n)))))

(defun sigma (from to fn)
  (apply #'+ (mapcar fn (iota from to))))

