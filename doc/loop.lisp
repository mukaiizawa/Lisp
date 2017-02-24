(loop for x in '(1 3 5)
      for y in '(2 4 6 8 10)
      collect (cons x y))
;; => ((1 . 2) (3 . 4) (5 . 6))

(loop for x in '(1 2 3 4 5)
      sum x into y
      collect y)
;; => (1 3 6 10 15)

(loop for i from 10 to 50 by 5 collect i)
;; => (10 15 20 25 30 35 40 45 50)

(loop for i from -1 to 1 collect
      (loop for j from -1 to 1 when (not (= i j 0)) collect
             (list i j)))
;; => (((-1 -1) (-1 0) (-1 1))
;;     (( 0 -1)        ( 0 1))
;;     (( 1 -1) ( 1 0) ( 1 1))) 
