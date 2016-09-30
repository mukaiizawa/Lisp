
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

;; make-cordinate-list {{{

(defmacro make-cordinate-list (&rest cordinates)
  `(mapcar (lambda (cordinate)
             (let1 (dimension (length cordinate))
               (make-cordinate :x (first cordinate)
                               :y (second cordinate)
                               :z (if (= dimension 3) (third cordinate) 0))))
           ',cordinates))

;; }}}
;; shift {{{

(defmethod shift ((cordinate cordinate) &key (x 0) (y 0) (z 0))
  (make-cordinate :x (+ (cordinate-x cordinate) x)
                  :y (+ (cordinate-y cordinate) y)
                  :z (+ (cordinate-z cordinate) z)))

;; }}}
;; shift! {{{

(defmethod shift! ((cordinate cordinate) &key x y z)
  (awhen x (setf (cordinate-x cordinate) x))
  (awhen y (setf (cordinate-y cordinate) y))
  (awhen z (setf (cordinate-z cordinate) z))
  cordinate)

;; }}}
;; vector+ {{{

(defmethod vector+ ((r1 cordinate) (r2 cordinate))
  (with-cordinates (r1 r2)
    (make-cordinate :x (+ x1 x2)
                    :y (+ y1 y2)
                    :z (+ z1 z2))))

;; }}}
;; norm {{{

(defmethod norm ((cordinate cordinate))
  (sqrt
    (apply #'+ (mapcar (lambda (x)
                         (expt x 2))
                       (list (cordinate-x cordinate)
                             (cordinate-y cordinate)
                             (cordinate-z cordinate))))))

;; }}}

