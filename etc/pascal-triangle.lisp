
(require "stdlib" *module-stdlib*)

(defun pascal (i j)
  (when (<= j i )
    (if (or (= j 0) (= i j))
      1
      (+ (pascal (1- i) (1- j))
         (pascal (1- i) j)))))

(defun pascal-line (i)
  (mapcar (lambda (j)
            (pascal i j))
          (iota 0 i)))

(defun pascal-triangle (i)
  (mapcar #'pascal-line (iota 0 i)))

(mapcar #'print (pascal-triangle 10))

