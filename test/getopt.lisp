
(load "../lib/stdlib")
(load "../lib/test-utils")

;; Attention: ccl bug -b option can't use.

(defvar *arg0* "head")
(defun msg0 (str)
  (mapcar (lambda (str)
            (format nil "~A: `~A' invalid option" *arg0* str))
          (mklist str)))
(defun msg1 (str) 
  (mapcar (lambda (str)
            (format nil "~A: option requires an argument -- `~A'" *arg0* str))
          (mklist str)))

(test-all
  (1
   (getopt '(#.*arg0* "-a") "a")
   nil '(("a" t)) nil)
  (2
   (getopt '(#.*arg0* "-a") "a:")
   nil '(("a" nil)) '#.(msg1 "-a"))
  (3
   (getopt '(#.*arg0* "-a") "b")
   nil '(("b" nil)) '#.(msg0 "-a"))
  (4
   (getopt '(#.*arg0* "-a20") "a")
   nil '(("a" t)) '#.(msg0 "-a20"))
  (5
   (getopt '(#.*arg0* "-a20") "b")
   nil '(("b" nil))  '#.(msg0 "-a20"))
  (6
   (getopt '(#.*arg0* "-a20") "a;b")
   nil '(("a" t) ("b" nil))  '#.(msg0 "-a20"))
  (7
   (getopt '(#.*arg0* "-a20") "a:b")
   nil '(("a" "20") ("b" nil)) nil)
  (8
   (getopt '(#.*arg0* "-a" "20") "a:b")
   nil '(("a" "20") ("b" nil)) nil)
  (9
   (getopt '(#.*arg0* "-a" "20") "a;b")
   '("20") '(("a" t) ("b" nil)) nil)
  (10
   (getopt '(#.*arg0* "-a" "20") "a:")
   nil '(("a" "20")) nil)
  (11
   (getopt '(#.*arg0* "-a" "20") "a:b")
   nil '(("a" "20") ("b" nil)) nil)
  (12
   (getopt '(#.*arg0* "-a" "abc") "a;")
   '("abc") '(("a" t)) nil)
  (13
   (getopt '(#.*arg0* "-a" "abc") "a:")
   nil '(("a" "abc")) nil)
  (14
   (getopt '(#.*arg0* "-a" "abc") "a;b")
   '("abc") '(("a" t) ("b" nil)) nil)
  (15
   (getopt '(#.*arg0* "-a" "abc") "a:b")
   nil '(("a" "abc") ("b" nil)) nil)
  (16
   (getopt '(#.*arg0* "-a" "-b") "a")
   nil '(("a" t))  '#.(msg0 "-b"))
  (17
   (getopt '(#.*arg0* "-a" "-b") "a:")
   nil '(("a" nil)) '#.(msg1 "-a"))
  (18
   (getopt '(#.*arg0* "-a" "-b") "a:b")
   nil '(("a" nil) ("b" nil)) '#.(msg1 "-a"))
  (19
   (getopt '(#.*arg0* "-a" "-b") "a:b:")
   nil '(("a" nil) ("b" nil)) '#.(msg1 "-a"))
  (20
   (getopt '(#.*arg0* "-a" "-b") "a;b")
   nil '(("a" t) ("b" t)) nil)
  (21
   (getopt '(#.*arg0* "-a" "-b") "a;b:")
   nil '(("a" t) ("b"nil)) '#.(msg1 "-b"))
  (22
   (getopt '(#.*arg0* "-a" "-b") "a;b;c")
   nil '(("a" t) ("b" t) ("c" nil)) nil)
  (23
   (getopt '(#.*arg0* "-a" "-b") "a:b;c")
   nil '(("a" nil) ("b" nil) ("c" nil)) '#.(msg1 "-a"))
  (24
   (getopt '(#.*arg0* "--ab" "-ab") "a;ab;")
   nil '(("a" t) ("ab" t)) '#.(msg0 '("-ab")))
  (25
   (getopt '(#.*arg0* "--ab" "-ab") "a;b;ab;")
   nil '(("a" t) ("b" t) ("ab" t)) nil)
  (26
   (getopt '(#.*arg0* "--ab" "-ab") "a;b:ab;")
   nil '(("a" t) ("b" nil) ("ab" t)) '#.(msg1 "-b"))
  (27
   (getopt '(#.*arg0* "--ab" "-ab") "a;b;ab:")
   nil '(("a" nil) ("b" nil) ("ab" nil)) '#.(msg1 "--ab"))
  (28
   (getopt '(#.*arg0* "--ab" "-ab") "a;b:ab:")
   nil '(("a" nil) ("b" nil) ("ab" nil)) '#.(msg1 "--ab"))
  (29
   (getopt '(#.*arg0* "--ab" "-ab" "a") "ab;a;b;")
   '("a") '(("ab" t) ("a" t) ("b" t)) nil)
  (30
   (getopt '(#.*arg0* "--ab" "-ab" "a") "ab;a;b:")
   nil '(("ab" t) ("a" t) ("b" "a")) nil)
  (31
   (getopt '(#.*arg0* "--ab" "a" "-ab") "ab:a;b;")
   nil '(("ab" "a") ("a" t) ("b" t)) nil)
  (32
   (getopt '(#.*arg0* "-ab" "--ab" "a") "ab:a;b;")
   nil '(("ab" "a") ("a" t) ("b" t)) nil)
  (33
   (getopt '(#.*arg0* "-ab" "--ab" "a") "ab;a;b;")
   '("a") '(("ab" t) ("a" t) ("b" t)) nil)
  (34
   (getopt '(#.*arg0* "-ab" "a" "--ab") "ab;a;b:")
   nil '(("ab" t) ("a" t) ("b" "a")) nil)
  (35
   (getopt '(#.*arg0* "--ab" "-ab") "a;b:ab:")
   nil '(("a" nil) ("b" nil) ("ab" nil)) '#.(msg1 "--ab"))
  (36
   (getopt '(#.*arg0* "--ab" "-ab") "a;b;ab;")
   nil '(("a" t) ("b" t) ("ab" t)) nil)
  (37
   (getopt '(#.*arg0* "--line-num=20") "line-num:")
   nil '(("line-num" "20")) nil)
  (38
   (getopt '(#.*arg0* "--line-num20") "line-num:")
   nil '(("line-num" "20")) nil)
  (39
   (getopt '(#.*arg0* "--line-num" "20") "line-num:")
   nil '(("line-num" "20")) nil)
  (40
   (getopt '(#.*arg0* "--" "-a") "a")
   '("-a") '(("a" nil)) nil)
  (41
   (getopt '(#.*arg0* "--" "-a" "-b") "a")
   '("-a" "-b") '(("a" nil)) nil)
  (42
   (getopt '(#.*arg0* "-a" "--") "a")
   nil '(("a" t)) nil)
  (43
   (getopt '(#.*arg0* "user/pass@sid" "--table=test") "table:")
   '("user/pass@sid") '(("table" "test")) nil)
  (44
   (getopt '(#.*arg0* "user/pass@sid" "--tabletest") "table:")
   '("user/pass@sid") '(("table" "test")) nil)
  (45
   (getopt '(#.*arg0* "user/pass@sid" "--table" "test") "table:")
   '("user/pass@sid") '(("table" "test")) nil)
  (46
   (getopt '(#.*arg0* "a-z" "A-Z") "a:z:")
   '("a-z" "A-Z") '(("a" nil) ("z" nil)) nil)
  (47
   (getopt '(#.*arg0* "a-z" "A-Z") "a;z")
   '("a-z" "A-Z") '(("a" nil) ("z" nil)) nil)
  )

