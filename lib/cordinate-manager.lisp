
(require "stdlib" *module-stdlib*)

(defstruct cordinate
  (x 0 :type number)
  (y 0 :type number)
  (z 0 :type number))

(defmacro with-cordinates ((&rest cordinates) &body body)
  (with-gensyms (counter)
    (let ((counter 0))
      `(let* (,@(do* ((acc)
                      (cordinates cordinates (rest cordinates))
                      (cordinate (first cordinates) (first cordinates)))
                  ((null cordinate) (nreverse acc))
                  (push `(,(mksym 'r  (incf counter)) ,cordinate) acc)
                  (push `(,(mksym 'x counter) (cordinate-x ,cordinate)) acc)
                  (push `(,(mksym 'y counter) (cordinate-y ,cordinate)) acc)
                  (push `(,(mksym 'z counter) (cordinate-z ,cordinate)) acc)))
         ,@body))))

;; shift {{{

(defmethod shift ((cordinate cordinate) &key (x 0) (y 0) (z 0))
  (make-cordinate :x (+ (cordinate-x cordinate) x)
                  :y (+ (cordinate-y cordinate) y)
                  :z (+ (cordinate-z cordinate) z)))

;; }}}
;; shift! {{{

(defmethod shift! ((cordinate cordinate) (x number) (y number) &optional (z 0))
  (setf (cordinate-x cordinate) x
        (cordinate-y cordinate) y
        (cordinate-z cordinate) z)
  cordinate)

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

