(load "./lib/std-lib")


(set-macro-character #\]
  (get-macro-character #\)))

(set-dispatch-macro-character #\# #\[
  (lambda (stream char1 char2)
    (declare (ignore char1 char2))
    (let ((pair (read-delimited-list #\] stream t)))
      (list 'quote (iota (first pair) (last1 pair))))))

(defmacro getopt (opts &body body)
  `(destructuring-bind ,opts
     (parse-arg ',opts (list "-a" "-n" "10" "fname"))
     ,@body))
;; command line args {{{
;; <= "-a -n 10 fname"
;; (args)
;; => ("-a" "-n" "10" "fname") }}}

(defun parse-arg (opts args)
  (echo "args:" args)
  (mapcar (lambda (opt)
            (print opt)
            t)
          opts))


; (echo (macroexpand-1
;   '(getopt (a n+)
;            (print n+)
;            (print a))))

#p(getopt (a n+)
        (print n+)
        (print a))



;; 内部でgetoptが展開されてbindされる
;;  (defapp test(a b? c+)
;;    (print a)
;;    (print b?)
;;    (print c+)))


