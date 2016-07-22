(load "../lib/stdlib")

(defun pascal (i j)
  (when (<= j i )
    (if (or (= j 0) (= i j))
      1
      (+ (pascal (1- i) (1- j))
         (pascal (1- i) j)))))
; (print (pascal 3 3))

(defun pascal-line (i)
  (mapa-b (lambda (j)
            (pascal i j))
          0 i))
; (print (pascal-line 3))


(defun pascal-triangle (i)
  (mapa-b #'pascal-line 0 i))

(mapcar #'print (pascal-triangle 10))

