
(load "compute")
(require :test-utils *module-test-utils*)

(defmacro test-calc (str)
  `(with-input-from-string (in ,str)
     (compute in)))

(test-all
  ('single-01
   (test-calc "1")
   1)
  ('single-02
   (test-calc "+1")
   1)
  ('single-03
   (test-calc "-1")
   -1)
  ('plus-01
   (test-calc "1+2")
   3)
  ('plus-02
   (test-calc "1+2+3")
   6)
  ('minus-01
   (test-calc "1-2")
   -1)
  ('minus-02
   (test-calc "1-2-3")
   -4)
  ('mul-01
   (test-calc "1*2")
   2)
  ('mul-02
   (test-calc "1*2*3")
   6)
  ('div-01
   (test-calc "2/1")
   2)
  ('div-02
   (test-calc "1/2")
   1/2)
  ('paren-01
   (test-calc "1*2*(3+4)")
   14)
  ('paren-02
   (test-calc "(1+2)*(3+4)")
   21)
  )

