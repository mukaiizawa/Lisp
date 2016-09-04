
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
   "(funcall #'+ #b01)"))

;; }}}
;; mkstr {{{

(test-all
  ('mkstr-01 (mkstr "a") "a")
  ('mkstr-02 (mkstr "1" #\2 3 '4) "1234"))

;; }}}
;; mkkey {{{

(test-all
  ('mkkey-01 (mkkey 'a) :a)
  ('mkkey-02 (mkkey "1" #\2 3 '4) :1234)
  ('mkkey-03 (mkkey "a" #\b 'c) :|abC|))

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
   '((a a) (b))))

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
   '(1 1 2 3 5 8 13 21 34 55)))

;; }}}
;; before {{{

(test-all
  ('before-01 (before #\b "abc") "a")
  ('before-02 (before #\b '(#\a #\b #\c)) '(#\a))
  ('before-03 (before #\b '(#\a #\b #\b #\c) :from-end t) '(#\a #\b)))

;; }}}
;; after {{{

(test-all
  ('after-01 (after #\b "abc") "c")
  ('after-02 (after #\b '(#\a #\b #\c)) '(#\c))
  ('after-03 (after #\b '(#\a #\b #\b #\c) :from-end t) '(#\c)))

;; }}}
;; last1 {{{

(test-all
  ('last1-01 (last1 '(a b c)) 'c))

;; }}}
;; single? {{{

(test-all
  ('single?-01 (single? '(a b c)) nil)
  ('single?-02 (single? '(a)) t)
  ('single?-03 (single? 'a) nil)
  ('single?-04 (single? 1) nil)
  ('single?-05 (single? "a") nil)
  ('single?-06 (single? #\a) nil))

;; }}}
;; alist? {{{

(test-all
  ('alist?-01 (values (alist? '(a b c))) nil)
  ('alist?-02 (values (alist? '(a))) nil)
  ('alist?-03 (values (alist? 'a)) nil)
  ('alist?-04 (values (alist? 1)) nil)
  ('alist?-05 (values (alist? "a")) nil)
  ('alist?-06 (values (alist? #\a)) nil))

;; }}}
;; append1 {{{

(test-all
  ('append1-01 (append1 '(a) 'b) '(a b))
  ('append1-02 (append1 '(a) '(b)) '(a (b))))

;; }}}
;; conc1 {{{

(test-all
  ('conc1-01 (conc1 '(a) 'b) '(a b))
  ('conc1-02 (conc1 '(a) '(b)) '(a (b))))

;; }}}
;; mklist {{{

(test-all
  ('mklist-01 (mklist nil) nil)
  ('mklist-02 (mklist 'a) '(a))
  ('mklist-03 (mklist '(a)) '(a)))

;; }}}
;; mklist {{{

(test-all
  ('mkalist-01 (mkalist nil) nil)
  ('mkalist-02 (mkalist 'a) '((a)))
  ('mkalist-03 (mkalist '(a)) '((a)))
  ('mkalist-04 (mkalist '(a b)) '((a) (b)))
  ('mkalist-05 (mkalist '((a a) b)) '((a a) (b)))
  ('mkalist-06 (mkalist '(a (b b))) '((a) (b b)))
  ('mkalist-07 (mkalist '((a a) (b b))) '((a a) (b b))))

;; }}}

