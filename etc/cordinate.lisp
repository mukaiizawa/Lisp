
(require "stdlib" *module-stdlib*)

(defstruct point
  (x 0 :type number)
  (y 0 :type number)
  (z 0 :type number))

(defmethod point->string ((p point))
  (mkstr "(" (point-x p) "," (point-y p) "," (point-z p) ")"))

(defmethod norm ((p point))
  (sqrt
    (apply #'+ (mapcar (lambda (x)
                         (* x x))
                       (list (point-x p)
                             (point-y p)
                             (point-z p))))))

(defvar p1 (make-point :x 1 :y 2 :z 3))
(defvar p2 (make-point))

;; Examples:
(point->string p1)
;; => (1,2,3)

