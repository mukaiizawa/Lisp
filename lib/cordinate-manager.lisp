
(require "stdlib" *module-stdlib*)

(defstruct cordinate
  (x 0 :type number)
  (y 0 :type number)
  (z 0 :type number))

(defmacro with-cordinates ((cordinate &rest cordinates) &body body)
  `(ilet* (,@(do* ((acc)
                   (counter 1 (1+ counter))
                   (cordinates (cons cordinate cordinates) (rest cordinates))
                   (cordinate (first cordinates) (first cordinates)))
               ((null cordinate) (nreverse acc))
               (push `(,(mksym 'r counter) ,cordinate) acc)
               (push `(,(mksym 'x counter) (cordinate-x ,cordinate)) acc)
               (push `(,(mksym 'y counter) (cordinate-y ,cordinate)) acc)
               (push `(,(mksym 'z counter) (cordinate-z ,cordinate)) acc)))
          ,@body))

;; make-vector {{{

(defun make-vector (x y &optional (z 0))
  (make-cordinate :x x
                  :y y
                  :z z))

;; }}}
;; make-vector-list {{{

(defmacro make-vector-list (&rest cordinates)
  `(mapcar (lambda (cordinate)
             `(make-vector ,@cordinate))
           ',cordinates))

;; }}}
;; vector->list {{{

(defmethod vector->list ((r1 cordinate))
  (with-cordinates (r1)
    (list x1 y1 z1)))

;; }}}
;; list->vector {{{

(defmacro list->vector (lis)
  `(make-vector ,@lis))

;; }}}
;; vector+ {{{

(defmethod vector+ ((r1 cordinate) (r2 cordinate))
  (with-cordinates (r1 r2)
    (make-cordinate :x (+ x1 x2)
                    :y (+ y1 y2)
                    :z (+ z1 z2))))

;; }}}
;; vector-rotate {{{

(defmethod vector-rotate ((r1 cordinate) (radian number))
  (with-cordinates (r1)
    (make-vector (- (* (cos radian) x1)
                    (* (sin radian) y1))
                 (+ (* (sin radian) x1)
                    (* (cos radian) y1)))))

;; }}}
;; vector-norm {{{

(defmethod vector-norm ((r1 cordinate))
  (sqrt (apply #'+
               (mapcar (lambda (x)
                         (expt x 2))
                       (vector->list r1)))))

;; }}}
;; vector-normalize {{{

(defmethod vector-normalize ((r1 cordinate))
  (with-cordinates (r1)
    (let1 (norm (vector-norm r1))
      (make-vector (/ x1 norm) (/ y1 norm) (/ z1 norm)))))

;; }}}

