
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


