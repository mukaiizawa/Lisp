(load "../lib/stdlib")

(defstruct point ()
  (x 0)
  (y 0)
  (z 0))

(defmethod p-print ((p point))
  (echo "(" (point-x p) "," (point-y p) "," (point-z p) ")"))

(defmethod norm ((p point))
  (sqrt
    (apply #'+ (mapcar (lambda (x)
                         (* x x))
                       (list (point-x p)
                             (point-y p)
                             (point-z p))))))

(defstruct segment ()
  p1
  p2)


(defvar p1 (make-point :x 1 :y 2 :z 3))
(defvar p2 (make-point))
(defvar s1 (make-segment)) 
(p-print p1)

