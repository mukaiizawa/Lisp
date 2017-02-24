;; symple declare
(defstruct point
  (x 0)
  (y 0))
;; => It is made automatically
;; (make-point ...),
;; (pointp obj),
;; (copy-point point),
;; (point-x point),
;; (point-y point)

;; like a java's constructor
(defparameter p (make-point :x 0 :y 0))
;; => #S(POINT :X 0 :Y 0) 

;; accesser
(point-x p)
;; => 0 
(point-y p)
;; => 0 

;; is point?
(point-p p)
;; <=>
(typep p 'point)
;; => T 

;; substitute
(setf (point-y p) 100)
(point-y p)
;; => 100 


(defstruct (point (:conc-name p)
                  (:print-object point-to-string))
  (x 0)
  (y 0))

;; print-function
(defun point-to-string (p stream)
  (format stream "(~A, ~A)" (px p) (py p)))

(defparameter p (make-point))
;; => (0, 0)

;; point
;; @see class.lisp
(defstruct point ()
  x y z)

(defmethod norm ((p point))
  (sqrt
    (apply #'+ (mapcar (lambda (x)
                         (* x x))
                       (list (point-x p)
                             (point-y p)
                             (point-z p))))))


(defvar *struct* (make-point :x 1 :y 1 :z 1))
(point-x *struct*)
;; =>1 
(point-y *struct*)
;; =>1 
(point-z *struct*)
;; =>1 
(norm *struct*)
;; => 1.7320508 



;; rectangle
;; @see class.lisp

(defstruct rectangle
  height width)

(defstruct circle
  radius)

(defun area (x)
  (cond ((rectangle-p x)
         (* (rectangle-height x) (rectangle-width x)))
        ((circle-p x)
         (* pi (expt (circle-radius x) 2)))))

(let ((r (make-rectangle))
      (c (make-circle)))
  (setf (rectangle-height r) 2
        (rectangle-width r ) 3
        (circle-radius c) 5)
  (area r)
  (area c))
;; => 6 
;; => 78.53981633974483096L0 
