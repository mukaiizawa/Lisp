(load "./lib/std-lib")


(defmacro getopt (opts &body body)
  `(destructuring-bind ,opts
     (parse-args ',opts (list "-a" "-n" "10" "fname"))
     ,@body))
;; command line args {{{
;; <= "-a -n 10 fname"
;; (args)
;; => ("-a" "-n" "10" "fname") }}}

(defun parse-args (opts args)
  (mapcar (lambda (opt)
            (BM (mkstr opt) (apply #'mkstr args)))
          opts))


#p(BM (mkstr 'a) (apply #'mkstr (list "-a" "-n" "10" "fname")) :ignore-case t)
#p(apply #'mkstr (list "-a" "-n" "10" "fname"))

; #m(parse-args '(a n+) (list "-a" "-n" "10" "fname"))
; (princln "---")
; #p(parse-args '(a n+) (list "-a" "-n" "10" "fname"))

; (echo (macroexpand-1
;   '(getopt (a n+)
;            (print n+)
;            (print a))))

(getopt (a n+)
          (list a n+))

;; 内部でgetoptが展開されてbindされる
;;  (defapp test(a b? c+)
;;    (print a)
;;    (print b?)
;;    (print c+)))


