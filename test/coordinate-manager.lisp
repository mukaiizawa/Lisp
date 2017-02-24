(require :coordinate-manager *module-coordinate-manager*)
(require :test-utils *module-test-utils*)

;; vector+ {{{

(test-all
  (vector+-01 (vector->list (vector+ (make-vector 2 2)
                                     (make-vector -1 -1)))
              (vector->list (make-vector 1 1)))
  (vector+-02 (vector->list (vector+ (make-vector 1 0 0)
                                     (make-vector 0 1 0)
                                     (make-vector 0 0 1)))
              (vector->list (make-vector 1 1 1))))

;; }}}
;; vector= {{{

(test-all
  (vector=-01 (vector= (make-vector) (make-vector 0 0))
              t)
  (vector=-02 (vector= (make-vector 1 0 0)
                       (make-vector 0 0 1))
              nil))

;; }}}
;; vector-origin? {{{

(test-all
  (vector-origin?-01 (vector-origin? (make-vector)) t)
  (vector-origin?-02 (vector-origin?
                       (vector+ (make-vector 1 0 1)
                                (make-vector -1 0 -1)))
                     t)
  (vector-origin?-03 (vector-origin? (make-vector 1 0 1)) nil))

;; }}}
;; vector-rotate {{{

(test-all
  (vector-rotate-01 (vector->list (vector-rotate (make-vector) (/ pi 2)))
                    (vector->list (make-vector)))
  (vector-rotate-02 (vector->list (vector-rotate (make-vector 1 0) (/ pi 2)))
                    (vector->list (make-vector 0 1))))

;; }}}
;; vector-norm {{{

(test-all
  (vector-norm-01 (vector-norm (make-vector)) 0)
  (vector-norm-02 (vector-norm (make-vector (sqrt 3)
                                            (sqrt 3)
                                            (sqrt 3)))
                  3.0))

;; }}}
