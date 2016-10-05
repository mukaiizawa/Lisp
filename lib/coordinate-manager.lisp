
(require "stdlib" *module-stdlib*)

(defstruct coordinate
  (x 0 :type number)
  (y 0 :type number)
  (z 0 :type number))

(defmacro with-coordinates ((coordinate &rest coordinates) &body body)
  `(ilet* (,@(do* ((acc)
                   (counter 1 (1+ counter))
                   (coordinates (cons coordinate coordinates) (rest coordinates))
                   (coordinate (first coordinates) (first coordinates)))
               ((null coordinate) (nreverse acc))
               (push `(,(mksym 'r counter) ,coordinate) acc)
               (push `(,(mksym 'x counter) (coordinate-x ,coordinate)) acc)
               (push `(,(mksym 'y counter) (coordinate-y ,coordinate)) acc)
               (push `(,(mksym 'z counter) (coordinate-z ,coordinate)) acc)))
          ,@body))

;; make-vector {{{

(defun make-vector (&optional (x 0) (y 0) (z 0))
  (make-coordinate :x x
                  :y y
                  :z z))

;; }}}
;; make-vector-list {{{

(defmacro make-vector-list (coordinates)
  `(mapcar (lambda (coordinate)
             (list->vector coordinate))
           ,coordinates))

;; }}} ;; vector->list {{{

(defmethod vector->list ((r1 coordinate))
  (with-coordinates (r1)
    (list x1 y1 z1)))

;; }}}
;; list->vector {{{

(defun list->vector (lis)
  (destructuring-bind (&optional (x 0) (y 0) (z 0)) 
    lis
    (make-vector x y z)))

;; }}}
;; vector+ {{{

(defmethod vector+ ((r1 coordinate) (r2 coordinate))
  (with-coordinates (r1 r2)
    (make-coordinate :x (+ x1 x2)
                    :y (+ y1 y2)
                    :z (+ z1 z2))))

;; }}}
;; vector-rotate {{{

(defmethod vector-rotate ((r1 coordinate) (radian number))
  (with-coordinates (r1)
    (make-vector (- (* (cos radian) x1)
                    (* (sin radian) y1))
                 (+ (* (sin radian) x1)
                    (* (cos radian) y1)))))

;; }}}
;; vector-norm {{{

(defmethod vector-norm ((r1 coordinate))
  (sqrt (apply #'+
               (mapcar (lambda (x)
                         (expt x 2))
                       (vector->list r1)))))

;; }}}
;; vector-normalize {{{

(defmethod vector-normalize ((r1 coordinate))
  (with-coordinates (r1)
    (let1 (norm (vector-norm r1))
      (make-vector (/ x1 norm) (/ y1 norm) (/ z1 norm)))))

;; }}}
