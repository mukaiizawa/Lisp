(load "./lib/std-lib")


(defmacro getopt (opts &body body)
  `(destructuring-bind ,opts
     (parse-args ',opts (list "-a" "-n" "10" "fname"))
     ,@body))
;; command line args {{{
;; <= "-a -n 10 fname"
;; (args)
;; => ("-a" "-n" "10" "fname") }}}

;; 指定オプションは英小文字のみ有効
(defun parse-args (opts args)
  (mapcar (lambda (opt)
            (let ((opt (string-downcase (mkstr opt))))
              (cond ((not (boyer-moore opt (apply #'mkstr args) :ignore-case t)) nil)
                    ((= (length opt) 1) t)
                    ((before #\+ opt) t)
                    ((before #\? opt) t)
                    (t (echo "illegal option")))))
          opts))

#p(parse-args '(a n+ f+ x) (list "-a" "-n+" "10" "fname"))

; (echo (macroexpand-1
;   '(getopt (a n+)
;            (print n+)
;            (print a))))

; (getopt (a n+)
;           (list a n+))

;; 内部でgetoptが展開されてbindされる
;;  (defapp test(a b? c+)
;;    (print a)
;;    (print b?)
;;    (print c+)))

