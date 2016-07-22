
(load "../lib/stdlib")

(defun sum (arg1 &rest args)
  (labels ((sum (arg1 arg2)
                (cond ((and (numberp arg1)
                            (numberp arg2))
                       (+ arg1 arg2))
                      ((and (or (not (numberp arg1))
                                (/= arg1 0))
                            (or (not (numberp arg2))
                                (/= arg2 0)))
                       (list '+ arg1 arg2))
                      ((or (not (numberp arg1)) (/= arg1 0)) arg1)
                      ((or (not (numberp arg2)) (/= arg2 0)) arg2)
                      (t 0))))
    (reduce #'sum args :initial-value arg1)))
;; Demo {{{

; (print (sum 1))
; (print (sum 0))
; (print (sum 'x))
; (print (sum 'x 0))
; (print (sum 0 'x))
; (print (sum 1 'x))
; (print (sum 'x 1))
; (print (sum 1 1))
; (print (sum 0 0))
; (print (sum 'x 'x))
; (print (sum 1 1 1))
; (print (sum 1 0 0))
; (print (sum 0 1 0))
; (print (sum 0 0 1))
; (print (sum 1 0 1))
; (print (sum 0 1 1))
; (print (sum 1 1 0))
; (print (sum 0 0 0))
; (print (sum 'x 'x 'x))


;; }}}

(defun product (arg1 arg2)
  (cond ((and (numberp arg1)
              (numberp arg2))
         (* arg1 arg2))
        ((or (and (numberp arg1) (= arg1 0))
             (and (numberp arg2) (= arg2 0)))
         0)
        ((and (numberp arg1) (= arg1 1)) arg2)
        ((and (numberp arg2) (= arg2 1)) arg1)
        (t (cond ((numberp arg1) (mksym arg1 arg2))
                 (t (mksym arg2 arg1))))))
;; Demo {{{

; (print (product 'x 0))
; (print (product 0 'x))
; (print (product 1 'x))
; (print (product 'x 1))
; (print (product 1 1))
; (print (product 0 0))
; (print (product 'x 'x))
; (print (product 'x (product (product 3 'x) 'x)))

;; }}}


(defun d (x expr)
  (cond ((numberp expr) 0)
        ((eq x expr) 1)
        ((atom expr) 0)
        ((eq (first expr) 'sin)
         (product (d x (second expr)) (list 'cos (second expr))))
        ((eq (first expr) 'cos)
         (product (product -1 (d x (second expr))) (list 'sin (second expr))))
        ((eq (first expr) '+)
         (sum (d x (second expr)) (d x (third expr))))
        ((eq (first expr) '*)
         (sum (product (second expr) (d x (third expr)))
              (product (d x (second expr)) (third expr))))))
;; d test{{{

(print (d 'x '(sin (* 3 x))))
(print (d 'x '(* x (+ 4 (* 3 x)))))
(print (d 'x '(cos x)))
(print (d 'x '(sin (* 3 x))))
(print (d 'x '(cos (sin (* 3 x)))))
(print (d 'x '(+ a (* a (+ 4 (* 3 x))))))
(print (d 'x '(* x (* a (+ 4 (* 3 x))))))

;; }}}
