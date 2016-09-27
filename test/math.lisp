
(require "math" *module-math*)
(require "test-utils" *module-test-utils*)

;; fact {{{

(test-all
  (fact-01 (fact 0) 1)
  (fact-02 (fact 1) 1)
  (fact-03 (fact 3) 6))

;; }}}
;; sigma {{{

(test-all
  (sigma-01 (sigma 0 10 #'identity) 55)
  (sigma-02 (sigma 1 1 #'identity) 1)
  (sigma-03 (sigma 1 10 (lambda (n) (* n n))) 385))

;; }}}
;; pi {{{

(test-all
  (pi-01 (pi 0 10 #'identity) 0)
  (pi-02 (pi 1 5 #'identity) 120)
  (pi-03 (pi 1 1 #'identity) 1))

;; }}}

