

;; Function EQ  {{{
;; 
;; 
;; Syntax:
;; eq x y => generalized-boolean
;; 
;; Arguments and Values:
;; x---an object. 
;; y---an object. 
;; generalized-boolean---a generalized boolean. 
;; 
;; Description:
;; Returns true if its arguments are the same, identical object; otherwise, returns false. 
;; 
;; Examples:
;; 
;;  (eq 'a 'b) =>  false
;;  (eq 'a 'a) =>  true
;;  (eq 3 3)
;;    =>  true
;; OR =>  false
;;  (eq 3 3.0) =>  false
;;  (eq 3.0 3.0)
;;    =>  true
;; OR =>  false
;;  (eq #c(3 -4) #c(3 -4))
;;    =>  true
;; OR =>  false
;;  (eq #c(3 -4.0) #c(3 -4)) =>  false
;;  (eq (cons 'a 'b) (cons 'a 'c)) =>  false
;;  (eq (cons 'a 'b) (cons 'a 'b)) =>  false
;;  (eq '(a . b) '(a . b))
;;    =>  true
;; OR =>  false
;;  (progn (setq x (cons 'a 'b)) (eq x x)) =>  true
;;  (progn (setq x '(a . b)) (eq x x)) =>  true
;;  (eq #\A #\A)
;;    =>  true
;; OR =>  false
;;  (let ((x "Foo")) (eq x x)) =>  true
;;  (eq "Foo" "Foo")
;;    =>  true
;; OR =>  false
;;  (eq "Foo" (copy-seq "Foo")) =>  false
;;  (eq "FOO" "foo") =>  false
;;  (eq "string-seq" (copy-seq "string-seq")) =>  false
;;  (let ((x 5)) (eq x x))
;;    =>  true
;; OR =>  false
;; 
;;
;; Side Effects: None. 
;; 
;; Affected By: None. 
;; 
;; Exceptional Situations: None. 
;; 
;; See Also:
;; eql, equal, equalp, =, Section 3.2 (Compilation) 
;; 
;; Notes:
;; Objects that appear the same when printed are not necessarily eq to each other. Symbols that print the same usually are eq to each other because of the use of the intern function. However, numbers with the same value need not be eq, and two similar lists are usually not identical. 
;; An implementation is permitted to make ``copies'' of characters and numbers at any time. The effect is that Common Lisp makes no guarantee that eq is true even when both its arguments are ``the same thing'' if that thing is a character or number. 
;; Most Common Lisp operators use eql rather than eq to compare objects, or else they default to eql and only use eq if specifically requested to do so. However, the following operators are defined to use eq rather than eql in a way that cannot be overridden by the code which employs them: 
;; }}}
;; Function EQL {{{
;; 
;; Syntax:
;; eql x y => generalized-boolean
;; 
;; Arguments and Values:
;; x---an object. 
;; y---an object. 
;; generalized-boolean---a generalized boolean. 
;;
;; Description:
;; The value of eql is true of two objects, x and y, in the folowing cases: 
;; 1. If x and y are eq. 2. If x and y are both numbers of the same type and the same value. 3. If they are both characters that represent the same character. 
;; Otherwise the value of eql is false. 
;; If an implementation supports positive and negative zeros as distinct values, then (eql 0.0 -0.0) returns false. Otherwise, when the syntax -0.0 is read it is interpreted as the value 0.0, and so (eql 0.0 -0.0) returns true. 
;; 
;; Examples:
;; 
;;  (eql 'a 'b) =>  false
;;  (eql 'a 'a) =>  true
;;  (eql 3 3) =>  true
;;  (eql 3 3.0) =>  false
;;  (eql 3.0 3.0) =>  true
;;  (eql #c(3 -4) #c(3 -4)) =>  true
;;  (eql #c(3 -4.0) #c(3 -4)) =>  false
;;  (eql (cons 'a 'b) (cons 'a 'c)) =>  false
;;  (eql (cons 'a 'b) (cons 'a 'b)) =>  false
;;  (eql '(a . b) '(a . b))
;;    =>  true
;; OR =>  false
;;  (progn (setq x (cons 'a 'b)) (eql x x)) =>  true
;;  (progn (setq x '(a . b)) (eql x x)) =>  true
;;  (eql #\A #\A) =>  true
;;  (eql "Foo" "Foo")
;;    =>  true
;; OR =>  false
;;  (eql "Foo" (copy-seq "Foo")) =>  false
;;  (eql "FOO" "foo") =>  false
;; 
;; Normally (eql 1.0s0 1.0d0) is false, under the assumption that 1.0s0 and 1.0d0 are of distinct data types. However, implementations that do not provide four distinct floating-point formats are permitted to ``collapse'' the four formats into some smaller number of them; in such an implementation (eql 1.0s0 1.0d0) might be true. 
;; 
;; Side Effects: None. 
;; 
;; Affected By: None. 
;; 
;; Exceptional Situations: None. 
;; 
;; See Also:
;; eq, equal, equalp, =, char= 
;; 
;; Notes:
;; 
;; eql is the same as eq, except that if the arguments are characters or numbers of the same type then their values are compared. Thus eql tells whether two objects are conceptually the same, whereas eq tells whether two objects are implementationally identical. It is for this reason that eql, not eq, is the default comparison predicate for operators that take sequences as arguments. 
;; eql may not be true of two floats even when they represent the same value. = is used to compare mathematical values. 
;; Two complex numbers are considered to be eql if their real parts are eql and their imaginary parts are eql. For example, (eql #C(4 5) #C(4 5)) is true and (eql #C(4 5) #C(4.0 5.0)) is false. Note that while (eql #C(5.0 0.0) 5.0) is false, (eql #C(5 0) 5) is true. In the case of (eql #C(5.0 0.0) 5.0) the two arguments are of different types, and so cannot satisfy eql. In the case of (eql #C(5 0) 5), #C(5 0) is not a complex number, but is automatically reduced to the integer 5. 
;;
;; }}}
;; Function EQUAL {{{
;; 
;; Syntax:
;; equal x y => generalized-boolean
;; 
;; Arguments and Values:
;; x---an object. 
;; y---an object. 
;; generalized-boolean---a generalized boolean. 
;; 
;; Description:
;; Returns true if x and y are structurally similar (isomorphic) objects. Objects are treated as follows by equal. 
;; 
;; Symbols, Numbers, and Characters 
;; equal is true of two objects if they are symbols that are eq, if they are numbers that are eql, or if they are characters that are eql. 
;; 
;; Conses 
;; For conses, equal is defined recursively as the two cars being equal and the two cdrs being equal. 
;; 
;; Arrays 
;; Two arrays are equal only if they are eq, with one exception: strings and bit vectors are compared element-by-element (using eql). If either x or y has a fill pointer, the fill pointer limits the number of elements examined by equal. Uppercase and lowercase letters in strings are considered by equal to be different. 
;; 
;; Pathnames 
;; Two pathnames are equal if and only if all the corresponding components (host, device, and so on) are equivalent. Whether or not uppercase and lowercase letters are considered equivalent in strings appearing in components is implementation-dependent. pathnames that are equal should be functionally equivalent. 
;; 
;; Other (Structures, hash-tables, instances, ...) 
;; Two other objects are equal only if they are eq. 
;; 
;; equal does not descend any objects other than the ones explicitly specified above. The next figure summarizes the information given in the previous list. In addition, the figure specifies the priority of the behavior of equal, with upper entries taking priority over lower ones. 
;; 
;; 
;; Type          Behavior
;; number        uses eql
;; character     uses eql
;; cons          descends
;; bit vector    descends
;; string        descends
;; pathname      ``functionally equivalent''
;; structure     uses eq
;; Other array   uses eq
;; hash table    uses eq
;; Other object  uses eq
;; 
;; Figure 5-12. Summary and priorities of behavior of equal 
;; Any two objects that are eql are also equal. 
;; equal may fail to terminate if x or y is circular. 
;; 
;; Examples:
;; 
;;  (equal 'a 'b) =>  false
;;  (equal 'a 'a) =>  true
;;  (equal 3 3) =>  true
;;  (equal 3 3.0) =>  false
;;  (equal 3.0 3.0) =>  true
;;  (equal #c(3 -4) #c(3 -4)) =>  true
;;  (equal #c(3 -4.0) #c(3 -4)) =>  false
;;  (equal (cons 'a 'b) (cons 'a 'c)) =>  false
;;  (equal (cons 'a 'b) (cons 'a 'b)) =>  true
;;  (equal #\A #\A) =>  true
;;  (equal #\A #\a) =>  false
;;  (equal "Foo" "Foo") =>  true
;;  (equal "Foo" (copy-seq "Foo")) =>  true
;;  (equal "FOO" "foo") =>  false
;;  (equal "This-string" "This-string") =>  true
;;  (equal "This-string" "this-string") =>  false
;; 
;; Side Effects: None. 
;;
;; Affected By: None. 
;; 
;; Exceptional Situations: None. 
;; 
;; 
;; 
;; See Also:
;; eq, eql, equalp, =, string=, string-equal, char=, char-equal, tree-equal 
;; 
;; 
;; 
;; Notes:
;; Object equality is not a concept for which there is a uniquely determined correct algorithm. The appropriateness of an equality predicate can be judged only in the context of the needs of some particular program. Although these functions take any type of argument and their names sound very generic, equal and equalp are not appropriate for every application. 
;; 
;; A rough rule of thumb is that two objects are equal if and only if their printed representations are the same. 
;; 
;; }}}
;; Function EQUALP  {{{
;;
;; Syntax:
;; equalp x y => generalized-boolean
;;
;; Arguments and Values:
;; x---an object. 
;; y---an object. 
;; generalized-boolean---a generalized boolean. 
;;
;; Description:
;; Returns true if x and y are equal, or if they have components that are of the same type as each other and if those components are equalp; specifically, equalp returns true in the following cases: 
;;
;; Characters 
;; If two characters are char-equal. 
;;
;; Numbers 
;; If two numbers are the same under =. 
;;
;; Conses 
;; If the two cars in the conses are equalp and the two cdrs in the conses are equalp. 
;;
;; Arrays 
;; If two arrays have the same number of dimensions, the dimensions match, and the corresponding active elements are equalp. The types for which the arrays are specialized need not match; for example, a string and a general array that happens to contain the same characters are equalp. Because equalp performs element-by-element comparisons of strings and ignores the case of characters, case distinctions are ignored when equalp compares strings. 
;;
;; Structures 
;; If two structures S1 and S2 have the same class and the value of each slot in S1 is the same under equalp as the value of the corresponding slot in S2. 
;;
;; Hash Tables 
;; equalp descends hash-tables by first comparing the count of entries and the :test function; if those are the same, it compares the keys of the tables using the :test function and then the values of the matching keys using equalp recursively. 
;;
;; equalp does not descend any objects other than the ones explicitly specified above. The next figure summarizes the information given in the previous list. In addition, the figure specifies the priority of the behavior of equalp, with upper entries taking priority over lower ones. 
;;
;;
;; Type          Behavior
;; number        uses =
;; character     uses char-equal
;; cons          descends
;; bit vector    descends
;; string        descends
;; pathname      same as equal
;; structure     descends, as described above
;; Other array   descends
;; hash table    descends, as described above
;; Other object  uses eq
;;
;;
;; Figure 5-13. Summary and priorities of behavior of equalp 
;;
;;
;;
;; Examples:
;;
;;
;;
;;
;;  (equalp 'a 'b) =>  false
;;  (equalp 'a 'a) =>  true
;;  (equalp 3 3) =>  true
;;  (equalp 3 3.0) =>  true
;;  (equalp 3.0 3.0) =>  true
;;  (equalp #c(3 -4) #c(3 -4)) =>  true
;;  (equalp #c(3 -4.0) #c(3 -4)) =>  true
;;  (equalp (cons 'a 'b) (cons 'a 'c)) =>  false
;;  (equalp (cons 'a 'b) (cons 'a 'b)) =>  true
;;  (equalp #\A #\A) =>  true
;;  (equalp #\A #\a) =>  true
;;  (equalp "Foo" "Foo") =>  true
;;  (equalp "Foo" (copy-seq "Foo")) =>  true
;;  (equalp "FOO" "foo") =>  true
;;
;;  (setq array1 (make-array 6 :element-type 'integer
;;                             :initial-contents '(1 1 1 3 5 7))) 
;; =>  #(1 1 1 3 5 7)
;;  (setq array2 (make-array 8 :element-type 'integer
;;                             :initial-contents '(1 1 1 3 5 7 2 6)
;;                             :fill-pointer 6))
;; =>  #(1 1 1 3 5 7)
;;  (equalp array1 array2) =>  true
;;  (setq vector1 (vector 1 1 1 3 5 7)) =>  #(1 1 1 3 5 7)
;;  (equalp array1 vector1) =>  true 
;;
;; Side Effects: None. 
;;
;; Affected By: None. 
;;
;; Exceptional Situations: None. 
;;
;; See Also:
;; eq, eql, equal, =, string=, string-equal, char=, char-equal 
;;
;; Notes:
;; Object equality is not a concept for which there is a uniquely determined correct algorithm. The appropriateness of an equality predicate can be judged only in the context of the needs of some particular program. Although these functions take any type of argument and their names sound very generic, equal and equalp are not appropriate for every application. 
;;
;; }}}


