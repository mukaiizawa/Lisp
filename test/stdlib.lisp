
(require "stdlib" *module-stdlib*)
(require "test-utils" *module-test-utils*)

;; #<< {{{

(test-all
  (read-macro-#<<-01
#<< END
abc END
   "abc")
  (read-macro-#<<-02
#<< END
abc
END
   "abc")
  (read-macro-#<<-03
#<< END
()()()
END
   "()()()")
  (read-macro-#<<-04
#<< END
(funcall #'+ #b01)
END
   "(funcall #'+ #b01)"))

;; }}}
;; mkstr {{{

(test-all
  (mkstr-01 (mkstr "a") "a")
  (mkstr-02 (mkstr "1" #\2 3 '4) "1234"))

;; }}}
;; mkkey {{{

(test-all
  (mkkey-01 (mkkey 'a) :a)
  (mkkey-02 (mkkey "1" #\2 3 '4) :1234)
  (mkkey-03 (mkkey "a" #\b 'c) :|abC|))

;; }}}
;; canonical-letargs {{{

(test-all
  (canonical-letargs-01
    (canonical-letargs '(a b c))
    '((a) (b) (c)))
  (canonical-letargs-02
    (canonical-letargs '((a a)))
    '((a a)))
  (canonical-letargs-02
    (canonical-letargs '((a a) b))
    '((a a) (b))))

;; }}}
;; alambda {{{

(test-all
  (alambda-fact
   (funcall (alambda (n)
              (if (= n 1)
                1
                (* n (self (1- n)))))
            5)
   120)
  (alambda-fib
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
  (before-01 (before #\b "abc") "a")
  (before-02 (before #\b '(#\a #\b #\c)) '(#\a))
  (before-03 (before #\b '(#\a #\b #\b #\c) :from-end t) '(#\a #\b)))

;; }}}
;; after {{{

(test-all
  (after-01 (after #\b "abc") "c")
  (after-02 (after #\b '(#\a #\b #\c)) '(#\c))
  (after-03 (after #\b '(#\a #\b #\b #\c) :from-end t) '(#\c)))

;; }}}
;; last1 {{{

(test-all
  (last1-01 (last1 '(a b c)) 'c))

;; }}}
;; single? {{{

(test-all
  (single?-01 (single? '(a b c)) nil)
  (single?-02 (single? '(a)) t)
  (single?-03 (single? 'a) nil)
  (single?-04 (single? 1) nil)
  (single?-05 (single? "a") nil)
  (single?-06 (single? #\a) nil))

;; }}}
;; alist? {{{

(test-all
  (alist?-01 (values (alist? '(a b c))) nil)
  (alist?-02 (values (alist? '(a))) nil)
  (alist?-03 (values (alist? 'a)) nil)
  (alist?-04 (values (alist? 1)) nil)
  (alist?-05 (values (alist? "a")) nil)
  (alist?-06 (values (alist? #\a)) nil))

;; }}}
;; append1 {{{

(test-all
  (append1-01 (append1 '(a) 'b) '(a b))
  (append1-02 (append1 '(a) '(b)) '(a (b))))

;; }}}
;; conc1 {{{

(test-all
  (conc1-01 (conc1 '(a) 'b) '(a b))
  (conc1-02 (conc1 '(a) '(b)) '(a (b))))

;; }}}
;; mklist {{{

(test-all
  (mklist-01 (mklist nil) nil)
  (mklist-02 (mklist 'a) '(a))
  (mklist-03 (mklist '(a)) '(a)))

;; }}}
;; mklist {{{

(test-all
  (mkalist-01 (mkalist nil) nil)
  (mkalist-02 (mkalist 'a) '((a)))
  (mkalist-03 (mkalist '(a)) '((a)))
  (mkalist-04 (mkalist '(a b)) '((a) (b)))
  (mkalist-05 (mkalist '((a a) b)) '((a a) (b)))
  (mkalist-06 (mkalist '(a (b b))) '((a) (b b)))
  (mkalist-07 (mkalist '((a a) (b b))) '((a a) (b b)))
  (mkalist-07 (mkalist '((a a a) (b b b))) '((a a a) (b b b))))

;; }}}
;; iota {{{

(test-all
  (iota-01 (iota 0 1) '(0 1))
  (iota-02 (iota 0 1 1) '(0 1))
  (iota-03 (iota 0 1 2) '(0))
  (iota-04 (iota 0 1 0.5) '(0 0.5 1.0)))

;; }}}
;; longer? {{{

(test-all
  (longer?-01 (longer? '(1) '(1)) nil)
  (longer?-02 (longer? '(1 2) '(1)) t)
  (longer?-03 (longer? '(1) '(1 2)) nil))

;; }}}
;; filter {{{

(test-all
  (filter-01 (filter (lambda (x)
                       (if (oddp x)
                         (+ x 10)))
                     '(1 2 3 4 5))
             '(11 13 15)))

;; }}}
;; group {{{

(test-all
  (group-01 (group nil 2) nil)
  (group-01 (group '(1 2 3 4 5) 2) '((1 2) (3 4) (5)))
  (group-02 (group '(1 2 3 4 5) 1) '((1) (2) (3) (4) (5))))

;; }}}
;; flatten {{{

(test-all
  (flatten-01 (flatten nil) nil)
  (flatten-02 (flatten '((1 2) (3 4) (5))) '(1 2 3 4 5))
  (flatten-03 (flatten '((1) (2) (3) (4) (5))) '(1 2 3 4 5))
  (flatten-04 (flatten '((1) (2) (nil) (4) (5))) '(1 2 nil 4 5)))

;; }}}
;; prune {{{

(test-all
  (prune-01 (prune #'oddp
                   '(1 ((2 3) 4) 5 (6 7) 8 9))
            '(((2) 4) (6) 8)))

;; }}}
;; before? {{{

(test-all
  (before?-01 (before? 'a 'c nil) nil)
  (before?-02 (before? 'a 'c '(a b c)) '(a b c))
  (before?-03 (before? 'b 'c '(a b c)) '(b c))
  (before?-04 (before? 'c 'a '(a b c)) nil)
  (before?-05 (before? "a" "c" '("a" "b" "c") :test #'equal) '("a" "b" "c")))

;; }}}
;; after? {{{

(test-all
  (after?-01 (after? 'a 'c nil) nil)
  (after?-02 (after? 'c 'a '(a b c)) '(c))
  (after?-03 (after? 'b 'a '(a b c)) '(b c))
  (after?-04 (after? 'a 'c '(a b c)) nil)
  (after?-05 (after? "c" "a" '("a" "b" "c") :test #'equal) '("c")))

;; }}}
;; last-member {{{

(test-all
  (last-member-01 (last-member 'a  nil) nil)
  (last-member-02 (last-member 'a  '(a 1)) '(a 1))
  (last-member-03 (last-member 'a  '(a 1 a 2 a 3)) '(a 3)))

;; }}}
;; most {{{

(test-all
  (most-01 (most #'length '((1) (1 2) (1 2 3) (1 2 3 4))) '(1 2 3 4) 4)
  (most-02 (most #'values '(1 2 3 4 5)) 5 5))

;; }}}

