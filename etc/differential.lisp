
(require "stdlib" *module-stdlib*)

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
;; Examples: {{{

(sum 1)    ;; => 1
(sum 0)    ;; => 0
(sum 'x)    ;; => X
(sum 'x 0)    ;; => X
(sum 0 'x)    ;; => X
(sum 1 'x)    ;; => (+ 1 X)
(sum 'x 1)    ;; => (+ X 1)
(sum 1 1)    ;; => 2
(sum 0 0)    ;; => 0
(sum 'x 'x)    ;; => (+ X X)
(sum 1 1 1)    ;; => 3
(sum 1 0 0)    ;; => 1
(sum 0 1 0)    ;; => 1
(sum 0 0 1)    ;; => 1
(sum 1 0 1)    ;; => 2
(sum 0 1 1)    ;; => 2
(sum 1 1 0)    ;; => 2
(sum 0 0 0)    ;; => 1
(sum 'x 'x 'x)    ;; => (+ (+ X X) X)

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
;; Examples: {{{

(product 'x 0)    ;; => 0
(product 0 'x)    ;; => 0
(product 1 'x)    ;; => X
(product 'x 1)    ;; => X
(product 1 1)    ;; => 1
(product 0 0)    ;; => 0
(product 'x 'x)    ;; => XX
(product 'x (product (product 3 'x) 'x))    ;; => X3XX

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
;; Examples: {{{

(d 'x '(sin (* 3 x)))    ;; |3(COS (* 3 X))| 
(d 'x '(* x (+ 4 (* 3 x))))    ;; (+ 3X (+ 4 (* 3 X))) 
(d 'x '(cos x))    ;; |-1(SIN X)| 
(d 'x '(sin (* 3 x)))    ;; |3(COS (* 3 X))| 
(d 'x '(cos (sin (* 3 x))))    ;; |(SIN (SIN (* 3 X)))-13(COS (* 3 X))| 
(d 'x '(+ a (* a (+ 4 (* 3 x)))))    ;; 3A 
(d 'x '(* x (* a (+ 4 (* 3 x)))))    ;; (+ 3AX (* A (+ 4 (* 3 X)))) 

;; }}}

