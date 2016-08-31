
(require "stdlib" *module-stdlib*)
(require "test-utils" *module-test-utils*)

;; #<< {{{

(test-all
  ('read-macro-#<<-01
#<< END
abc END
   "abc")
  ('read-macro-#<<-02
#<< END
abc
END
   "abc")
  ('read-macro-#<<-03
#<< END
()()()
END
   "()()()")
  ('read-macro-#<<-04
#<< END
(funcall #'+ #b01)
END
   "(funcall #'+ #b01)")
  )

;; }}}
;; mkstr {{{

(test-all
  ('mkstr-01 (mkstr "a") "a")
  ('mkstr-02 (mkstr "1" #\2 3 '4) "1234")
  )

;; }}}
;; mkstr {{{

(test-all
  ('mkkey-01 (mkkey 'a) :a)
  ('mkkey-02 (mkkey "1" #\2 3 '4) :1234)
  ('mkkey-03 (mkkey "a" #\b 'c) :|abC|)
  )

;; }}}
;; canonical-letargs {{{

(test-all
  ('canonical-letargs-01
   (canonical-letargs '(a b c))
   '((a) (b) (c)))
  ('canonical-letargs-02
   (canonical-letargs '((a a)))
   '((a a)))
  ('canonical-letargs-02
   (canonical-letargs '((a a) b))
   '((a a) (b)))
  )

;; }}}
;; alambda {{{

(test-all
  ('alambda-fact
   (funcall (alambda (n)
              (if (= n 1)
                1
                (* n (self (1- n)))))
            5)
   120)
  ('alambda-fib
   (mapcar (alambda (n)
             (if (or (= n 1) (= n 2))
               1
               (+ (self (- n 1))
                  (self (- n 2)))))
           '(1 2 3 4 5 6 7 8 9 10))
   '(1 1 2 3 5 8 13 21 34 55))
  )

;; }}}

