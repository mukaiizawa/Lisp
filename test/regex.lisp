
(require "stdlib" *module-stdlib*)
(require "ahead-reader" *module-ahead-reader*)
(require "regex" *module-regex*)
(require "test-utils" *module-test-utils*)

(test-all
  ;; no metacharacter {{{

  (no-metacharacter-01 (match? "a" "a") '(0 1))
  (no-metacharacter-02 (match? "a" "za") '(1 2))
  (no-metacharacter-03 (match? "a" "") nil)

  ; ;; }}}
  ;; selectioin `[]' {{{

  (selection-01 (match? "[ab]" "z") nil)
  (selection-02 (match? "[ab]" "a") '(0 1))
  (selection-03 (match? "[ab]" "b") '(0 1))
  (selection-04 (match? "[a-c]" "a") '(0 1))
  (selection-05 (match? "[a-c]" "b") '(0 1))
  (selection-06 (match? "[a-c]" "c") '(0 1))

  ;; }}}
  ;; not-selectioin `[^]' {{{

  (not-selection-01 (match? "[^ab]" "z") '(0 1))
  (not-selection-02 (match? "[^ab]" "a") nil)
  (not-selection-03 (match? "[^ab]" "b") nil)
  (not-selection-04 (match? "[^a-c]" "a") nil)
  (not-selection-05 (match? "[^a-c]" "b") nil)
  (not-selection-06 (match? "[^a-c]" "c") nil)

  ;; }}}
  ;; grouping `()' {{{

  (grouping-01 (match? "(ab|cd)" "a") nil)
  (grouping-02 (match? "(ab|cd)" "ab") '(0 2))
  (grouping-03 (match? "(ab|cd)" "cd") '(0 2))
  (grouping-04 (match? "(a*|b*)" "aa") '(0 2))
  (grouping-05 (match? "(a*|b*)" "bb") '(0 0))
  (grouping-06 (match? "(a*?|b*?)" "aa") '(0 0))
  (grouping-07 (match? "(a*?|b*?)" "bb") '(0 0))

  ;; }}}
  ;; group-rec {{{

  (group-rec-01 (match? "((ab|cd)e|fg)" "abe") '(0 3))
  (group-rec-02 (match? "((ab|cd)e|fg)" "cde") '(0 3))
  (group-rec-03 (match? "((ab|cd)e|fg)" "fg") '(0 2))
  (group-rec-04 (match? "((ab|cd)e|fg)*" "abecdefg") '(0 8))

  ;; }}}
  ;; dot {{{

  (dot-01 (match? "." "a") '(0 1))

  ;; }}}
  ;; start {{{

  (start-01 (match? "^a" "a") '(0 1))
  (start-02 (match? "^a" "za") nil)
  (start-03 (match? "^a*" "a") '(0 1))
  (start-04 (match? "^a*?" "a") '(0 0))

  ;; }}}
  ;; end {{{

  (end-01 (match? "a$" "a") '(0 1))
  (end-02 (match? "a$" "z") nil)

  ;; }}}
  ;; start-end {{{

  (start-end-01 (match? "^a$" "a") '(0 1))
  (start-end-02 (match? "^a$" "abc") nil)

  ;; }}}
  ;; question {{{

  (question-01 (match? "a?" "a") '(0 1))
  (question-02 (match? "a?" "z") '(0 0))
  (question-03 (match? "a??" "a") '(0 0))
  (question-04 (match? "a??" "z") '(0 0))

  ;; }}}
  ;; plus {{{

  (plus-01 (match? "a+" "a") '(0 1))
  (plus-02 (match? "a+" "aa") '(0 2))
  (plus-03 (match? "a+?" "aa") '(0 1))

  ;; }}}
  ;; aster {{{

  (aster-01 (match? "a*" "z") '(0 0))
  (aster-02 (match? "a*" "a") '(0 1))
  (aster-03 (match? "a*" "aa") '(0 2))
  (aster-04 (match? "a*?" "z") '(0 0))
  (aster-05 (match? "a*?" "a") '(0 0))
  (aster-06 (match? "a*?" "aa") '(0 0))

  ;; }}}
  ;; more-n `{n,}' {{{

  (more-n-01 (match? "a{2,}" "a") nil)
  (more-n-02 (match? "a{2,}" "aa") '(0 2))
  (more-n-03 (match? "a{2,}" "aaa") '(0 3))
  (more-n-04 (match? "a{2,}?" "a") nil)
  (more-n-05 (match? "a{2,}?" "aa") '(0 2))
  (more-n-06 (match? "a{2,}?" "aaa") '(0 2))

  ;; }}}
  ;; between-n-and-m `{n,m}' {{{

  (between-n-and-m-01 (match? "a{2,3}" "a") nil)
  (between-n-and-m-02 (match? "a{2,3}" "aa") '(0 2))
  (between-n-and-m-03 (match? "a{2,3}" "aaa") '(0 3))
  (between-n-and-m-04 (match? "a{2,3}" "aaaa") '(0 3))
  (between-n-and-m-05 (match? "a{2,3}?" "a") nil)
  (between-n-and-m-06 (match? "a{2,3}?" "aa") '(0 2))
  (between-n-and-m-07 (match? "a{2,3}?" "aaa") '(0 2))
  (between-n-and-m-08 (match? "a{2,3}?" "aaaa") '(0 2))

  ;; }}}
  ;; exactory-n {{{

  (exactory-n-01 (match? "a{2}" "a") nil)
  (exactory-n-02 (match? "a{2}" "aa") '(0 2))
  (exactory-n-03 (match? "a{2}" "aaa") '(0 2))

  ;; }}}
  ;; escape {{{

  (escape-01 (match? "\\[a\\]" "[a]") '(0 3))
  (escape-02 (match? "[\\[\\]]" "[") '(0 1))
  (escape-03 (match? "[\\[\\]]" "]") '(0 1))
  (escape-04 (match? "[^\\[\\]]" "]") nil)
  (escape-05 (match? "[^\\[\\]]" "[") nil)
  (escape-06 (match? "\\(a\\)" "(a)") '(0 3))
  (escape-07 (match? "\\[a\\]" "[a]") '(0 3))

  ;; }}}
  ;; etc {{{

  (phone-number (match? "([0-9]|-)*" "01-2345-6789") '(0 12))
  (date-01 (match? "[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}" "1900/01/01") '(0 10))
  (date-02 (match? "[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}" "2000/2/2") '(0 8))

  ;; }}}
  )

