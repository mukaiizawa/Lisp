(load "../lib/stdlib")

(member 'b '(a b c))
;; => (B C) 
(member '(a) '((a) (z)))
;; => NIL 
(member '(a) '((a) (z)) :test #'equal)
;; => ((A) (Z)) 
(member 'a '((a b) (c d)) :key #'car)
;; => ((A B) (C D)) 

(member-if #'oddp '(2 3 4 5 6))
;; => (3 4 5 6) 
(member-if #'evenp '(2 3 4 5 6))
;; => (2 3 4 5 6) 

(adjoin 'b '(a b c))
;; => (A B C) 
(adjoin 'z '(a b c))
;; => (Z A B C) 


(union '(a b c) '(c b s))
;; => (A C B S) 
(intersection '(a b c) '(b b c))
;; => (B C) 
(set-difference '(a b c d e) '(b e))
;; => (A C D) 


;; Function MEMBER, MEMBER-IF, MEMBER-IF-NOT {{{
;; 
;; Syntax:
;; member item list &key key test test-not => tail
;; member-if predicate list &key key => tail
;; member-if-not predicate list &key key => tail
;; 
;; Arguments and Values:
;; item---an object. 
;; list---a proper list. 
;; predicate---a designator for a function of one argument that returns a generalized boolean. 
;; test---a designator for a function of two arguments that returns a generalized boolean. 
;; test-not---a designator for a function of two arguments that returns a generalized boolean. 
;; key---a designator for a function of one argument, or nil. 
;; tail---a list. 
;; 
;; Description:
;; member, member-if, and member-if-not each search list for item or for a top-level element that satisfies the test. The argument to the predicate function is an element of list. 
;; If some element satisfies the test, the tail of list beginning with this element is returned; otherwise nil is returned. 
;; list is searched on the top level only. 
;; 
;; Examples:
;;  (member 2 '(1 2 3)) =>  (2 3)                                 
;;  (member 2 '((1 . 2) (3 . 4)) :test-not #'= :key #'cdr) =>  ((3 . 4))
;;  (member 'e '(a b c d)) =>  NIL
;;
;;  (member-if #'listp '(a b nil c d)) =>  (NIL C D)
;;  (member-if #'numberp '(a #\Space 5/3 foo)) =>  (5/3 FOO)
;;  (member-if-not #'zerop 
;;                  '(3 6 9 11 . 12)
;;                  :key #'(lambda (x) (mod x 3))) =>  (11 . 12)
;; 
;; Exceptional Situations:
;; Should be prepared to signal an error of type type-error if list is not a proper list. 
;; 
;; See Also:
;; find, position, Section 3.6 (Traversal Rules and Side Effects) 
;; 
;; Notes:
;; The :test-not parameter is deprecated. 
;; The function member-if-not is deprecated. 
;; In the following 
;;  (member 'a '(g (a y) c a d e a f)) =>  (A D E A F)
;; the value returned by member is identical to the portion of the list beginning with a. Thus rplaca on the result of member can be used to alter the part of the list where a was found (assuming a check has been made that member did not return nil). 
;;
;; }}}
;; Function SET-DIFFERENCE, NSET-DIFFERENCE {{{
;; 
;; Syntax:
;; set-difference list-1 list-2 &key key test test-not => result-list
;; nset-difference list-1 list-2 &key key test test-not => result-list
;; 
;; Arguments and Values:
;; list-1---a proper list. 
;; list-2---a proper list. 
;; test---a designator for a function of two arguments that returns a generalized boolean. 
;; test-not---a designator for a function of two arguments that returns a generalized boolean. 
;; key---a designator for a function of one argument, or nil. 
;; result-list---a list. 
;; 
;; Description:
;; set-difference returns a list of elements of list-1 that do not appear in list-2. 
;; nset-difference is the destructive version of set-difference. It may destroy list-1. 
;; For all possible ordered pairs consisting of one element from list-1 and one element from list-2, the :test or :test-not function is used to determine whether they satisfy the test. The first argument to the :test or :test-not function is the part of an element of list-1 that is returned by the :key function (if supplied); the second argument is the part of an element of list-2 that is returned by the :key function (if supplied). 
;; If :key is supplied, its argument is a list-1 or list-2 element. The :key function typically returns part of the supplied element. If :key is not supplied, the list-1 or list-2 element is used. 
;; An element of list-1 appears in the result if and only if it does not match any element of list-2. 
;; There is no guarantee that the order of elements in the result will reflect the ordering of the arguments in any particular way. The result list may share cells with, or be eq to, either of list-1 or list-2, if appropriate. 
;; 
;; Examples:
;;  (setq lst1 (list "A" "b" "C" "d")
;;        lst2 (list "a" "B" "C" "d")) =>  ("a" "B" "C" "d")
;;  (set-difference lst1 lst2) =>  ("d" "C" "b" "A")
;;  (set-difference lst1 lst2 :test 'equal) =>  ("b" "A")
;;  (set-difference lst1 lst2 :test #'equalp) =>  NIL 
;;  (nset-difference lst1 lst2 :test #'string=) =>  ("A" "b")
;;  (setq lst1 '(("a" . "b") ("c" . "d") ("e" . "f")))
;; =>  (("a" . "b") ("c" . "d") ("e" . "f")) 
;;  (setq lst2 '(("c" . "a") ("e" . "b") ("d" . "a")))
;; =>  (("c" . "a") ("e" . "b") ("d" . "a")) 
;;  (nset-difference lst1 lst2 :test #'string= :key #'cdr)
;; =>  (("c" . "d") ("e" . "f")) 
;;  lst1 =>  (("a" . "b") ("c" . "d") ("e" . "f")) 
;;  lst2 =>  (("c" . "a") ("e" . "b") ("d" . "a")) 
;; 
;; ;; Remove all flavor names that contain "c" or "w".
;;  (set-difference '("strawberry" "chocolate" "banana"
;;                   "lemon" "pistachio" "rhubarb")
;;           '(#\c #\w)
;;           :test #'(lambda (s c) (find c s)))
;; =>  ("banana" "rhubarb" "lemon")    ;One possible ordering.
;; 
;; Side Effects:
;; nset-difference may destroy list-1. 
;; 
;; Exceptional Situations:
;; Should be prepared to signal an error of type type-error if list-1 and list-2 are not proper lists. 
;; 
;; See Also:
;; Section 3.2.1 (Compiler Terminology), Section 3.6 (Traversal Rules and Side Effects) 
;; 
;; Notes:
;; The :test-not parameter is deprecated. 
;;
;; }}}
;; Function UNION, NUNION {{{
;; 
;; Syntax:
;; union list-1 list-2 &key key test test-not => result-list
;; nunion list-1 list-2 &key key test test-not => result-list
;; 
;; Arguments and Values:
;; list-1---a proper list. 
;; list-2---a proper list. 
;; test---a designator for a function of two arguments that returns a generalized boolean. 
;; test-not---a designator for a function of two arguments that returns a generalized boolean. 
;; key---a designator for a function of one argument, or nil. 
;; result-list---a list. 
;; 
;; Description:
;; union and nunion return a list that contains every element that occurs in either list-1 or list-2. 
;; For all possible ordered pairs consisting of one element from list-1 and one element from list-2, :test or :test-not is used to determine whether they satisfy the test. The first argument to the :test or :test-not function is the part of the element of list-1 extracted by the :key function (if supplied); the second argument is the part of the element of list-2 extracted by the :key function (if supplied). 
;; The argument to the :key function is an element of list-1 or list-2; the return value is part of the supplied element. If :key is not supplied or nil, the element of list-1 or list-2 itself is supplied to the :test or :test-not function. 
;; For every matching pair, one of the two elements of the pair will be in the result. Any element from either list-1 or list-2 that matches no element of the other will appear in the result. 
;; If there is a duplication between list-1 and list-2, only one of the duplicate instances will be in the result. If either list-1 or list-2 has duplicate entries within it, the redundant entries might or might not appear in the result. 
;; The order of elements in the result do not have to reflect the ordering of list-1 or list-2 in any way. The result list may be eq to either list-1 or list-2 if appropriate. 
;; 
;; Examples:
;;  (union '(a b c) '(f a d))
;; =>  (A B C F D)
;; OR=>  (B C F A D)
;; OR=>  (D F A B C)
;;  (union '((x 5) (y 6)) '((z 2) (x 4)) :key #'car)
;; =>  ((X 5) (Y 6) (Z 2))
;; OR=>  ((X 4) (Y 6) (Z 2))
;; 
;;  (setq lst1 (list 1 2 '(1 2) "a" "b")
;;        lst2 (list 2 3 '(2 3) "B" "C"))
;; =>  (2 3 (2 3) "B" "C")
;;  (nunion lst1 lst2)
;; =>  (1 (1 2) "a" "b" 2 3 (2 3) "B" "C") 
;; OR=>  (1 2 (1 2) "a" "b" "C" "B" (2 3) 3)
;; 
;; Side Effects:
;; nunion is permitted to modify any part, car or cdr, of the list structure of list-1 or list-2. 
;; 
;; Exceptional Situations:
;; Should be prepared to signal an error of type type-error if list-1 and list-2 are not proper lists. 
;; 
;; See Also:
;; intersection, Section 3.2.1 (Compiler Terminology), Section 3.6 (Traversal Rules and Side Effects) 
;; 
;; Notes:
;; The :test-not parameter is deprecated. 
;; Since the nunion side effect is not required, it should not be used in for-effect-only positions in portable code. 
;; }}}
;; Function INTERSECTION, NINTERSECTION {{{
;; 
;; Syntax:
;; intersection list-1 list-2 &key key test test-not => result-list
;; nintersection list-1 list-2 &key key test test-not => result-list
;; 
;; Arguments and Values:
;; list-1---a proper list. 
;; list-2---a proper list. 
;; test---a designator for a function of two arguments that returns a generalized boolean. 
;; test-not---a designator for a function of two arguments that returns a generalized boolean. 
;; key---a designator for a function of one argument, or nil. 
;; result-list---a list. 
;; 
;; Description:
;; intersection and nintersection return a list that contains every element that occurs in both list-1 and list-2. 
;; nintersection is the destructive version of intersection. It performs the same operation, but may destroy list-1 using its cells to construct the result. list-2 is not destroyed. 
;; The intersection operation is described as follows. For all possible ordered pairs consisting of one element from list-1 and one element from list-2, :test or :test-not are used to determine whether they satisfy the test. The first argument to the :test or :test-not function is an element of list-1; the second argument is an element of list-2. If :test or :test-not is not supplied, eql is used. It is an error if :test and :test-not are supplied in the same function call. 
;; If :key is supplied (and not nil), it is used to extract the part to be tested from the list element. The argument to the :key function is an element of either list-1 or list-2; the :key function typically returns part of the supplied element. If :key is not supplied or nil, the list-1 and list-2 elements are used. 
;; For every pair that satifies the test, exactly one of the two elements of the pair will be put in the result. No element from either list appears in the result that does not satisfy the test for an element from the other list. If one of the lists contains duplicate elements, there may be duplication in the result. 
;; There is no guarantee that the order of elements in the result will reflect the ordering of the arguments in any particular way. The result list may share cells with, or be eq to, either list-1 or list-2 if appropriate. 
;; 
;; Examples:
;;  (setq list1 (list 1 1 2 3 4 a b c "A" "B" "C" "d")
;;        list2 (list 1 4 5 b c d "a" "B" "c" "D")) 
;;   =>  (1 4 5 B C D "a" "B" "c" "D")
;;  (intersection list1 list2) =>  (C B 4 1 1)
;;  (intersection list1 list2 :test 'equal) =>  ("B" C B 4 1 1)
;;  (intersection list1 list2 :test #'equalp) =>  ("d" "C" "B" "A" C B 4 1 1) 
;;  (nintersection list1 list2) =>  (1 1 4 B C)
;;  list1 =>  implementation-dependent ;e.g.,  (1 1 4 B C)
;;  list2 =>  implementation-dependent ;e.g.,  (1 4 5 B C D "a" "B" "c" "D")
;;  (setq list1 (copy-list '((1 . 2) (2 . 3) (3 . 4) (4 . 5))))
;; =>  ((1 . 2) (2 . 3) (3 . 4) (4 . 5)) 
;;  (setq list2 (copy-list '((1 . 3) (2 . 4) (3 . 6) (4 . 8))))
;; =>  ((1 . 3) (2 . 4) (3 . 6) (4 . 8)) 
;;  (nintersection list1 list2 :key #'cdr) =>  ((2 . 3) (3 . 4)) 
;;  list1 =>  implementation-dependent ;e.g.,  ((1 . 2) (2 . 3) (3 . 4)) 
;;  list2 =>  implementation-dependent ;e.g.,  ((1 . 3) (2 . 4) (3 . 6) (4 . 8)) 
;; 
;; Side Effects:
;; nintersection can modify list-1, but not list-2. 
;; 
;; Exceptional Situations:
;; Should be prepared to signal an error of type type-error if list-1 and list-2 are not proper lists. 
;;
;; See Also:
;; union, Section 3.2.1 (Compiler Terminology), Section 3.6 (Traversal Rules and Side Effects) 
;; 
;; Notes:
;; The :test-not parameter is deprecated. 
;; Since the nintersection side effect is not required, it should not be used in for-effect-only positions in portable code. 
;; }}}


