
;; define
(defclass circle()
  (radius center))

;; create instance
(defparameter c (make-instance 'circle))

;; accesser
(setf (slot-value c 'radius) 1)
(setf (slot-value c 'center) '(0 0))

;; define accesser
(defclass circle()
  ((radius :accessor circle-radius)
   (center :accessor circle-center)))
(defparameter c (make-instance 'circle))
(setf (circle-radius c) 4)
(setf (circle-center c) '(4 0))

;; define default slot-value like a constructor
(defclass circle()
  ((radius :accessor circle-radius
           :initarg :radius
           :initform 1)
   (center :accessor circle-center
           :initarg :center
           :initform (cons 0 0))))
(defparameter c (make-instance 'circle :radius 3))

;; define static variable
(defclass circle()
  ((radius :accessor circle-radius
           :initarg :radius
           :initform 1)
   (center :accessor circle-center
           :initarg :center
           :initform (cons 0 0))
   (num :accessor circle-num
        :allocation :class
        :initform 1)))
(defparameter c1 (make-instance 'circle :radius 3))
(incf (circle-num c1))
(defparameter c2 (make-instance 'circle :radius 2))
(print (circle-num c2))
;; => 2








;; point
;; @see structure.lisp
(defclass point ()
  (x y z))

(defmethod norm ((p point))
  (sqrt
    (apply #'+ (mapcar (lambda (x)
                         (* x x))
                       (list (slot-value p 'x)
                             (slot-value p 'y)
                             (slot-value p 'z))))))

(defvar *class* (make-instance 'point))
(setf (slot-value *class* 'x) 1)
(setf (slot-value *class* 'y) 1)
(setf (slot-value *class* 'z) 1)
(slot-value *class* 'x)
;; =>1 
(slot-value *class* 'y)
;; =>1 
(slot-value *class* 'z)
;; =>1 
(norm *class*)
;; => 1.7320508 

;; rectangle
;; @see structure.lisp
(defclass rectangle ()
  (height width))

(defclass circle ()
  (radius))

(defmethod area ((x rectangle))
  (* (slot-value x 'height)
     (slot-value x 'width)))

(defmethod area ((x circle))
  (* pi (expt (slot-value x 'radius) 2)))

(let ((r (make-instance 'rectangle))
      (c (make-instance 'circle)))
  (setf (slot-value r 'height) 2
        (slot-value r 'width) 3
        (slot-value c 'radius) 5)
  (area r)
  (area c))
;; => 6 
;; => 78.53981633974483096L0 




