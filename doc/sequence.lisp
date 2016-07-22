(load "../lib/stdlib")


;; before, after
(before #\s "asdfasdf")
;; => "a" 
(after #\s "asdfasdf")
;; => "dfasdf" 
(before #\s "asdfasdf" :from-end t)
;; => "asdfa" 
(after 's '(a s d f a s d f))
;; => (D F A S D F) 

;; Accessor {{{
;; nth => list
;; svref => vector
;; aref => array, vector
;; char => string
;; elt => *

(nth 1 '(0 1 2 3))
;; => 1 
(svref (vector 0 1 2 3) 1)
;; => 1 
(aref (make-array '(4) :initial-contents  '(0 1 2 3)) 1)
;; => 1 
(char "0123" 1)
;; => #\1 

(elt '(0 1 2 3) 1)
;; => 1 
(elt (vector 0 1 2 3) 1)
;; => 1 
(elt (make-array '(4) :initial-contents  '(0 1 2 3)) 1)
;; => 1 
(elt "0123" 1)
;; => #\1 


;; }}}
;; Function {{{
;; length
(length '(a b c))
;; => 3 
(length "abc")
;; => 3 

;; subseq
(subseq '(0 1 2 3) 1)
;; => (1 2 3) 
(subseq "0123" 1)
;; => "123" 

;; reverse
(reverse '(0 1 2 3))
;; => (3 2 1 0) 
(reverse "0123")
;; => "3210" 

;; }}}
;; Function COPY-SEQ  {{{
;; 
;; Syntax:
;; copy-seq sequence => copied-sequence
;; 
;; Arguments and Values:
;; sequence---a proper sequence. 
;; copied-sequence---a proper sequence. 
;; 
;; Description:
;; Creates a copy of sequence. The elements of the new sequence are the same as the corresponding elements of the given sequence. 
;; If sequence is a vector, the result is a fresh simple array of rank one that has the same actual array element type as sequence. If sequence is a list, the result is a fresh list. 
;; 
;; Examples:
;;  (setq str "a string") =>  "a string"
;;  (equalp str (copy-seq str)) =>  true
;;  (eql str (copy-seq str)) =>  false
;;
;; Exceptional Situations:
;; Should be prepared to signal an error of type type-error if sequence is not a proper sequence. 
;; 
;; See Also:
;; copy-list 
;; 
;; Notes:
;; From a functional standpoint, 
;;  (copy-seq x) ==  (subseq x 0)
;; 
;; However, the programmer intent is typically very different in these two cases.
;;
;; }}}
;; Function FILL  {{{
;; 
;; Syntax:
;; fill sequence item &key start end => sequence
;; 
;; Arguments and Values:
;; sequence---a proper sequence. 
;; item---a sequence. 
;; start, end---bounding index designators of sequence. The defaults for start and end are 0 and nil, respectively. 
;; 
;; Description:
;; Replaces the elements of sequence bounded by start and end with item. 
;; 
;; Examples:
;;  (fill (list 0 1 2 3 4 5) '(444)) =>  ((444) (444) (444) (444) (444) (444))
;;  (fill (copy-seq "01234") #\e :start 3) =>  "012ee"
;;  (setq x (vector 'a 'b 'c 'd 'e)) =>  #(A B C D E)
;;  (fill x 'z :start 1 :end 3) =>  #(A Z Z D E)
;;  x =>  #(A Z Z D E)
;;  (fill x 'p) =>  #(P P P P P)
;;  x =>  #(P P P P P)
;; 
;; Side Effects:
;; Sequence is destructively modified. 
;; 
;; Exceptional Situations:
;; Should be prepared to signal an error of type type-error if sequence is not a proper sequence. Should signal an error of type type-error if start is not a non-negative integer. Should signal an error of type type-error if end is not a non-negative integer or nil. 
;; 
;; See Also:
;; replace, nsubstitute 
;; 
;; Notes:
;; (fill sequence item) == (nsubstitute-if item (constantly t) sequence) 
;;
;; }}}
;; Function MAKE-SEQUENCE {{{
;;
;; Syntax:
;; make-sequence result-type size &key initial-element => sequence
;; 
;; Arguments and Values:
;; result-type---a sequence type specifier.
;; size---a non-negative integer.
;; initial-element---an object. The default is implementation-dependent.
;; sequence---a proper sequence.
;; 
;; Description:
;; Returns a sequence of the type result-type and of length size, each of the elements of which has been initialized to initial-element.
;; If the result-type is a subtype of list, the result will be a list.
;; If the result-type is a subtype of vector, then if the implementation can determine the element type specified for the result-type, the element type of the resulting array is the result of upgrading that element type; or, if the implementation can determine that the element type is unspecified (or *), the element type of the resulting array is t; otherwise, an error is signaled.
;; 
;; Examples:
;;  (make-sequence 'list 0) =>  ()
;;  (make-sequence 'string 26 :initial-element #\.) 
;; =>  ".........................."
;;  (make-sequence '(vector double-float) 2
;;                 :initial-element 1d0)
;; =>  #(1.0d0 1.0d0)
;;  (make-sequence '(vector * 2) 3) should signal an error
;;  (make-sequence '(vector * 4) 3) should signal an error
;;
;; }}}
;; Function REDUCE  {{{
;; 
;; Syntax:
;; reduce function sequence &key key from-end start end initial-value => result
;; 
;; Arguments and Values:
;; function---a designator for a function that might be called with either zero or two arguments. 
;; sequence---a proper sequence. 
;; key---a designator for a function of one argument, or nil. 
;; from-end---a generalized boolean. The default is false. 
;; start, end---bounding index designators of sequence. The defaults for start and end are 0 and nil, respectively. 
;; initial-value---an object. 
;; result---an object. 
;; 
;; Description:
;; reduce uses a binary operation, function, to combine the elements of sequence bounded by start and end. 
;; The function must accept as arguments two elements of sequence or the results from combining those elements. The function must also be able to accept no arguments. 
;; If key is supplied, it is used is used to extract the values to reduce. The key function is applied exactly once to each element of sequence in the order implied by the reduction order but not to the value of initial-value, if supplied. The key function typically returns part of the element of sequence. If key is not supplied or is nil, the sequence element itself is used. 
;; The reduction is left-associative, unless from-end is true in which case it is right-associative. 
;; If initial-value is supplied, it is logically placed before the subsequence (or after it if from-end is true) and included in the reduction operation. 
;; In the normal case, the result of reduce is the combined result of function's being applied to successive pairs of elements of sequence. If the subsequence contains exactly one element and no initial-value is given, then that element is returned and function is not called. If the subsequence is empty and an initial-value is given, then the initial-value is returned and function is not called. If the subsequence is empty and no initial-value is given, then the function is called with zero arguments, and reduce returns whatever function does. This is the only case where the function is called with other than two arguments. 
;; 
;; Examples:
;;  (reduce #'* '(1 2 3 4 5)) =>  120
;;  (reduce #'append '((1) (2)) :initial-value '(i n i t)) =>  (I N I T 1 2)
;;  (reduce #'append '((1) (2)) :from-end t
;;                              :initial-value '(i n i t)) =>  (1 2 I N I T) 
;;  (reduce #'- '(1 2 3 4)) ==  (- (- (- 1 2) 3) 4) =>  -8
;;  (reduce #'- '(1 2 3 4) :from-end t)    ;Alternating sum.
;; ==  (- 1 (- 2 (- 3 4))) =>  -2
;;  (reduce #'+ '()) =>  0
;;  (reduce #'+ '(3)) =>  3
;;  (reduce #'+ '(foo)) =>  FOO
;;  (reduce #'list '(1 2 3 4)) =>  (((1 2) 3) 4)
;;  (reduce #'list '(1 2 3 4) :from-end t) =>  (1 (2 (3 4)))
;;  (reduce #'list '(1 2 3 4) :initial-value 'foo) =>  ((((foo 1) 2) 3) 4)
;;  (reduce #'list '(1 2 3 4)
;;         :from-end t :initial-value 'foo) =>  (1 (2 (3 (4 foo))))
;; }}}
;; Function COUNT, COUNT-IF, COUNT-IF-NOT {{{
;;
;; Syntax:
;; count item sequence &key from-end start end key test test-not => n
;; count-if predicate sequence &key from-end start end key => n
;; count-if-not predicate sequence &key from-end start end key => n
;; 
;; Arguments and Values:
;; item---an object.
;; sequence---a proper sequence.
;; predicate---a designator for a function of one argument that returns a generalized boolean.
;; from-end---a generalized boolean. The default is false.
;; test---a designator for a function of two arguments that returns a generalized boolean.
;; test-not---a designator for a function of two arguments that returns a generalized boolean.
;; start, end---bounding index designators of sequence. The defaults for start and end are 0 and nil, respectively.
;; key---a designator for a function of one argument, or nil.
;; n---a non-negative integer less than or equal to the length of sequence.
;; 
;; Description:
;; count, count-if, and count-if-not count and return the number of elements in the sequence bounded by start and end that satisfy the test.
;; The from-end has no direct effect on the result. However, if from-end is true, the elements of sequence will be supplied as arguments to the test, test-not, and key in reverse order, which may change the side-effects, if any, of those functions.
;; 
;; Examples:
;;  (count #\a "how many A's are there in here?") =>  2
;;  (count-if-not #'oddp '((1) (2) (3) (4)) :key #'car) =>  2
;;  (count-if #'upper-case-p "The Crying of Lot 49" :start 4) =>  2 
;; 
;; }}}
;; Function FIND, FIND-IF, FIND-IF-NOT {{{
;;
;; Syntax:
;; find item sequence &key from-end test test-not start end key => element
;; find-if predicate sequence &key from-end start end key => element
;; find-if-not predicate sequence &key from-end start end key => element
;; 
;; Arguments and Values:
;; item---an object.
;; sequence---a proper sequence.
;; predicate---a designator for a function of one argument that returns a generalized boolean.
;; from-end---a generalized boolean. The default is false.
;; test---a designator for a function of two arguments that returns a generalized boolean.
;; test-not---a designator for a function of two arguments that returns a generalized boolean.
;; start, end---bounding index designators of sequence. The defaults for start and end are 0 and nil, respectively.
;; key---a designator for a function of one argument, or nil.
;; element---an element of the sequence, or nil.
;; 
;; Description:
;; find, find-if, and find-if-not each search for an element of the sequence bounded by start and end that satisfies the predicate predicate or that satisfies the test test or test-not, as appropriate.
;; If from-end is true, then the result is the rightmost element that satisfies the test.
;; If the sequence contains an element that satisfies the test, then the leftmost or rightmost sequence element, depending on from-end, is returned; otherwise nil is returned.
;; 
;; Examples:
;;  (find #\d "here are some letters that can be looked at" :test #'char>)
;; =>  #\Space 
;;  (find-if #'oddp '(1 2 3 4 5) :end 3 :from-end t) =>  3
;;  (find-if-not #'complexp
;;              '#(3.5 2 #C(1.0 0.0) #C(0.0 1.0))
;;              :start 2) =>  NIL 
;; }}}
;; Function POSITION, POSITION-IF, POSITION-IF-NOT {{{
;; Syntax:
;;
;; position item sequence &key from-end test test-not start end key => position
;; position-if predicate sequence &key from-end start end key => position
;; position-if-not predicate sequence &key from-end start end key => position
;;
;; Arguments and Values:
;; item---an object.
;; sequence---a proper sequence.
;; predicate---a designator for a function of one argument that returns a generalized boolean.
;; from-end---a generalized boolean. The default is false.
;; test---a designator for a function of two arguments that returns a generalized boolean.
;; test-not---a designator for a function of two arguments that returns a generalized boolean.
;; start, end---bounding index designators of sequence. The defaults for start and end are 0 and nil, respectively.
;; key---a designator for a function of one argument, or nil.
;; position---a bounding index of sequence, or nil.
;;
;; Description:
;; position, position-if, and position-if-not each search sequence for an element that satisfies the test.
;; The position returned is the index within sequence of the leftmost (if from-end is true) or of the rightmost (if from-end is false) element that satisfies the test; otherwise nil is returned. The index returned is relative to the left-hand end of the entire sequence, regardless of the value of start, end, or from-end.
;;
;; Examples:
;;
;;  (position #\a "baobab" :from-end t) =>  4
;;  (position-if #'oddp '((1) (2) (3) (4)) :start 1 :key #'car) =>  2
;;  (position 595 '()) =>  NIL
;;  (position-if-not #'integerp '(1 2 3 4 5.0)) =>  4 
;;
;; }}}
;; Function REMOVE, REMOVE-IF, REMOVE-IF-NOT, DELETE, DELETE-IF, DELETE-IF-NOT {{{
;;
;; Syntax:
;; remove item sequence &key from-end test test-not start end count key => result-sequence
;; remove-if test sequence &key from-end start end count key => result-sequence
;; remove-if-not test sequence &key from-end start end count key => result-sequence
;; delete item sequence &key from-end test test-not start end count key => result-sequence
;; delete-if test sequence &key from-end start end count key => result-sequence
;; delete-if-not test sequence &key from-end start end count key => result-sequence
;; 
;; Arguments and Values:
;; item---an object.
;; sequence---a proper sequence.
;; test---a designator for a function of one argument that returns a generalized boolean.
;; from-end---a generalized boolean. The default is false.
;; test---a designator for a function of two arguments that returns a generalized boolean.
;; test-not---a designator for a function of two arguments that returns a generalized boolean.
;; start, end---bounding index designators of sequence. The defaults for start and end are 0 and nil, respectively.
;; count---an integer or nil. The default is nil.
;; key---a designator for a function of one argument, or nil.
;; result-sequence---a sequence.
;; 
;; Description:
;; remove, remove-if, and remove-if-not return a sequence from which the elements that satisfy the test have been removed.
;; delete, delete-if, and delete-if-not are like remove, remove-if, and remove-if-not respectively, but they may modify sequence.
;; If sequence is a vector, the result is a vector that has the same actual array element type as sequence. If sequence is a list, the result is a list.
;; Supplying a from-end of true matters only when the count is provided; in that case only the rightmost count elements satisfying the test are deleted.
;; Count, if supplied, limits the number of elements removed or deleted; if more than count elements satisfy the test, then of these elements only the leftmost or rightmost, depending on from-end, are deleted or removed, as many as specified by count. If count is supplied and negative, the behavior is as if zero had been supplied instead. If count is nil, all matching items are affected.
;; For all these functions, elements not removed or deleted occur in the same order in the result as they did in sequence.
;; remove, remove-if, remove-if-not return a sequence of the same type as sequence that has the same elements except that those in the subsequence bounded by start and end and satisfying the test have been removed. This is a non-destructive operation. If any elements need to be removed, the result will be a copy. The result of remove may share with sequence; the result may be identical to the input sequence if no elements need to be removed.
;; delete, delete-if, and delete-if-not return a sequence of the same type as sequence that has the same elements except that those in the subsequence bounded by start and end and satisfying the test have been deleted. Sequence may be destroyed and used to construct the result; however, the result might or might not be identical to sequence.
;; delete, when sequence is a list, is permitted to setf any part, car or cdr, of the top-level list structure in that sequence. When sequence is a vector, delete is permitted to change the dimensions of the vector and to slide its elements into new positions without permuting them to produce the resulting vector.
;; delete-if is constrained to behave exactly as follows:
;;  (delete nil sequence
;;              :test #'(lambda (ignore item) (funcall test item))
;;              ...)
;; Examples:
;;  (remove 4 '(1 3 4 5 9)) =>  (1 3 5 9)
;;  (remove 4 '(1 2 4 1 3 4 5)) =>  (1 2 1 3 5)
;;  (remove 4 '(1 2 4 1 3 4 5) :count 1) =>  (1 2 1 3 4 5)
;;  (remove 4 '(1 2 4 1 3 4 5) :count 1 :from-end t) =>  (1 2 4 1 3 5)
;;  (remove 3 '(1 2 4 1 3 4 5) :test #'>) =>  (4 3 4 5)
;;  (setq lst '(list of four elements)) =>  (LIST OF FOUR ELEMENTS)
;;  (setq lst2 (copy-seq lst)) =>  (LIST OF FOUR ELEMENTS)
;;  (setq lst3 (delete 'four lst)) =>  (LIST OF ELEMENTS)
;;  (equal lst lst2) =>  false
;;  (remove-if #'oddp '(1 2 4 1 3 4 5)) =>  (2 4 4)
;;  (remove-if #'evenp '(1 2 4 1 3 4 5) :count 1 :from-end t) 
;; =>  (1 2 4 1 3 5)
;;  (remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9) :count 2 :from-end t)
;; =>  (1 2 3 4 5 6 8)
;;  (setq tester (list 1 2 4 1 3 4 5)) =>  (1 2 4 1 3 4 5)
;;  (delete 4 tester) =>  (1 2 1 3 5)
;;  (setq tester (list 1 2 4 1 3 4 5)) =>  (1 2 4 1 3 4 5)
;;  (delete 4 tester :count 1) =>  (1 2 1 3 4 5)
;;  (setq tester (list 1 2 4 1 3 4 5)) =>  (1 2 4 1 3 4 5)
;;  (delete 4 tester :count 1 :from-end t) =>  (1 2 4 1 3 5)
;;  (setq tester (list 1 2 4 1 3 4 5)) =>  (1 2 4 1 3 4 5)
;;  (delete 3 tester :test #'>) =>  (4 3 4 5)
;;  (setq tester (list 1 2 4 1 3 4 5)) =>  (1 2 4 1 3 4 5)
;;  (delete-if #'oddp tester) =>  (2 4 4)
;;  (setq tester (list 1 2 4 1 3 4 5)) =>  (1 2 4 1 3 4 5)
;;  (delete-if #'evenp tester :count 1 :from-end t) =>  (1 2 4 1 3 5)    
;;  (setq tester (list 1 2 3 4 5 6)) =>  (1 2 3 4 5 6) 
;;  (delete-if #'evenp tester) =>  (1 3 5) 
;;  tester =>  implementation-dependent
;;  (setq foo (list 'a 'b 'c)) =>  (A B C)
;;  (setq bar (cdr foo)) =>  (B C)
;;  (setq foo (delete 'b foo)) =>  (A C)
;;  bar =>  ((C)) or ...
;;  (eq (cdr foo) (car bar)) =>  T or ...
;; 
;; }}}
;; Function SUBSTITUTE, SUBSTITUTE-IF, SUBSTITUTE-IF-NOT, NSUBSTITUTE, NSUBSTITUTE-IF, NSUBSTITUTE-IF-NOT {{{
;;
;; Syntax:
;; substitute newitem olditem sequence &key from-end test test-not start end count key
;; => result-sequence
;; substitute-if newitem predicate sequence &key from-end start end count key
;; => result-sequence
;; substitute-if-not newitem predicate sequence &key from-end start end count key
;; => result-sequence
;; nsubstitute newitem olditem sequence &key from-end test test-not start end count key
;; => sequence
;; nsubstitute-if newitem predicate sequence &key from-end start end count key
;; => sequence
;; nsubstitute-if-not newitem predicate sequence &key from-end start end count key
;; => sequence
;; 
;; Arguments and Values:
;; newitem---an object.
;; olditem---an object.
;; sequence---a proper sequence.
;; predicate---a designator for a function of one argument that returns a generalized boolean.
;; from-end---a generalized boolean. The default is false.
;; test---a designator for a function of two arguments that returns a generalized boolean.
;; test-not---a designator for a function of two arguments that returns a generalized boolean.
;; start, end---bounding index designators of sequence. The defaults for start and end are 0 and nil, respectively.
;; count---an integer or nil. The default is nil.
;; key---a designator for a function of one argument, or nil.
;; result-sequence---a sequence.
;; 
;; Description:
;; substitute, substitute-if, and substitute-if-not return a copy of sequence in which each element that satisfies the test has been replaced with newitem.
;; nsubstitute, nsubstitute-if, and nsubstitute-if-not are like substitute, substitute-if, and substitute-if-not respectively, but they may modify sequence.
;; If sequence is a vector, the result is a vector that has the same actual array element type as sequence. If sequence is a list, the result is a list.
;; Count, if supplied, limits the number of elements altered; if more than count elements satisfy the test, then of these elements only the leftmost or rightmost, depending on from-end, are replaced, as many as specified by count. If count is supplied and negative, the behavior is as if zero had been supplied instead. If count is nil, all matching items are affected.
;; Supplying a from-end of true matters only when the count is provided (and non-nil); in that case, only the rightmost count elements satisfying the test are removed (instead of the leftmost).
;; predicate, test, and test-not might be called more than once for each sequence element, and their side effects can happen in any order.
;; The result of all these functions is a sequence of the same type as sequence that has the same elements except that those in the subsequence bounded by start and end and satisfying the test have been replaced by newitem.
;; substitute, substitute-if, and substitute-if-not return a sequence which can share with sequence or may be identical to the input sequence if no elements need to be changed.
;; nsubstitute and nsubstitute-if are required to setf any car (if sequence is a list) or aref (if sequence is a vector) of sequence that is required to be replaced with newitem. If sequence is a list, none of the cdrs of the top-level list can be modified.
;; 
;; Examples:
;;  (substitute #\. #\SPACE "0 2 4 6") => "0.2.4.6"
;;  (substitute 9 4 '(1 2 4 1 3 4 5)) => (1 2 9 1 3 9 5)
;;  (substitute 9 4 '(1 2 4 1 3 4 5) :count 1) => (1 2 9 1 3 4 5)
;;  (substitute 9 4 '(1 2 4 1 3 4 5) :count 1 :from-end t)  => (1 2 4 1 3 9 5)
;;  (substitute 9 3 '(1 2 4 1 3 4 5) :test #'>) =>  (9 9 4 9 3 4 5)
;;  (substitute-if 0 #'evenp '((1) (2) (3) (4)) :start 2 :key #'car) => ((1) (2) (3) 0)
;;  (substitute-if 9 #'oddp '(1 2 4 1 3 4 5)) =>  (9 2 4 9 9 4 9)
;;  (substitute-if 9 #'evenp '(1 2 4 1 3 4 5) :count 1 :from-end t) => (1 2 4 1 3 9 5)
;;  (setq some-things (list 'a 'car 'b 'cdr 'c)) =>  (A CAR B CDR C)
;;  (nsubstitute-if "function was here" #'fboundp some-things :count 1 :from-end t) => (A CAR B "function was here" C)
;;  some-things => (A CAR B "function was here" C)
;;  (setq alpha-tester (copy-seq "ab ")) =>  "ab "
;;  (nsubstitute-if-not #\z #'alpha-char-p alpha-tester) =>  "abz"
;;  alpha-tester =>  "abz"
;;
;; Side Effects:
;; nsubstitute, nsubstitute-if, and nsubstitute-if-not modify sequence.
;; 
;; Exceptional Situations:
;; Should be prepared to signal an error of type type-error if sequence is not a proper sequence.
;; 
;; 
;; Notes:
;; If sequence is a vector, the result might or might not be simple, and might or might not be identical to sequence.
;; The :test-not argument is deprecated.
;; The functions substitute-if-not and nsubstitute-if-not are deprecated.
;; nsubstitute and nsubstitute-if can be used in for-effect-only positions in code.
;; Because the side-effecting variants (e.g., nsubstitute) potentially change the path that is being traversed, their effects in the presence of shared or circular structure may vary in surprising ways when compared to their non-side-effecting alternatives. To see this, consider the following side-effect behavior, which might be exhibited by some implementations:
;;  (defun test-it (fn)
;;    (let ((x (cons 'b nil)))
;;      (rplacd x x)
;;      (funcall fn 'a 'b x :count 1)))
;;  (test-it #'substitute) =>  (A . #1=(B . #1#))
;;  (test-it #'nsubstitute) =>  (A . #1#)
;; 
;; }}}
;; Function SUBST, SUBST-IF, SUBST-IF-NOT, NSUBST, NSUBST-IF, NSUBST-IF-NOT {{{
;;
;; Syntax:
;; subst new old tree &key key test test-not => new-tree
;; subst-if new predicate tree &key key => new-tree
;; subst-if-not new predicate tree &key key => new-tree
;; nsubst new old tree &key key test test-not => new-tree
;; nsubst-if new predicate tree &key key => new-tree
;; nsubst-if-not new predicate tree &key key => new-tree
;; 
;; Arguments and Values:
;; new---an object.
;; old---an object.
;; predicate---a symbol that names a function, or a function of one argument that returns a generalized boolean value.
;; tree---a tree.
;; test---a designator for a function of two arguments that returns a generalized boolean.
;; test-not---a designator for a function of two arguments that returns a generalized boolean.
;; key---a designator for a function of one argument, or nil.
;; new-tree---a tree.
;; 
;; Description:
;; subst, subst-if, and subst-if-not perform substitution operations on tree. Each function searches tree for occurrences of a particular old item of an element or subexpression that satisfies the test.
;; nsubst, nsubst-if, and nsubst-if-not are like subst, subst-if, and subst-if-not respectively, except that the original tree is modified.
;; subst makes a copy of tree, substituting new for every subtree or leaf of tree (whether the subtree or leaf is a car or a cdr of its parent) such that old and the subtree or leaf satisfy the test.
;; nsubst is a destructive version of subst. The list structure of tree is altered by destructively replacing with new each leaf of the tree such that old and the leaf satisfy the test.
;; For subst, subst-if, and subst-if-not, if the functions succeed, a new copy of the tree is returned in which each occurrence of such an element is replaced by the new element or subexpression. If no changes are made, the original tree may be returned. The original tree is left unchanged, but the result tree may share storage with it.
;; For nsubst, nsubst-if, and nsubst-if-not the original tree is modified and returned as the function result, but the result may not be eq to tree.
;; 
;; Examples:
;;  (setq tree1 '(1 (1 2) (1 2 3) (1 2 3 4))) =>  (1 (1 2) (1 2 3) (1 2 3 4))
;;  (subst "two" 2 tree1) =>  (1 (1 "two") (1 "two" 3) (1 "two" 3 4))
;;  (subst "five" 5 tree1) =>  (1 (1 2) (1 2 3) (1 2 3 4))
;;  (eq tree1 (subst "five" 5 tree1)) =>  implementation-dependent
;;  (subst 'tempest 'hurricane
;;         '(shakespeare wrote (the hurricane)))
;; =>  (SHAKESPEARE WROTE (THE TEMPEST))
;;  (subst 'foo 'nil '(shakespeare wrote (twelfth night)))
;; =>  (SHAKESPEARE WROTE (TWELFTH NIGHT . FOO) . FOO)
;;  (subst '(a . cons) '(old . pair)
;;         '((old . spice) ((old . shoes) old . pair) (old . pair))
;;         :test #'equal)
;; =>  ((OLD . SPICE) ((OLD . SHOES) A . CONS) (A . CONS))
;; 
;;  (subst-if 5 #'listp tree1) =>  5
;;  (subst-if-not '(x) #'consp tree1) 
;; =>  (1 X)
;; 
;;  tree1 =>  (1 (1 2) (1 2 3) (1 2 3 4))
;;  (nsubst 'x 3 tree1 :key #'(lambda (y) (and (listp y) (third y)))) 
;; =>  (1 (1 2) X X)
;;  tree1 =>  (1 (1 2) X X)
;;
;; Side Effects:
;; nsubst, nsubst-if, and nsubst-if-not might alter the tree structure of tree.
;; 
;; Notes:
;; 
;; The :test-not parameter is deprecated.
;; 
;; The functions subst-if-not and nsubst-if-not are deprecated.
;; 
;; One possible definition of subst:
;; 
;;  (defun subst (old new tree &rest x &key test test-not key)
;;    (cond ((satisfies-the-test old tree :test test
;;                               :test-not test-not :key key)
;;           new)
;;          ((atom tree) tree)
;;          (t (let ((a (apply #'subst old new (car tree) x))
;;                   (d (apply #'subst old new (cdr tree) x)))
;;               (if (and (eql a (car tree))
;;                        (eql d (cdr tree)))
;;                   tree
;;                   (cons a d))))))
;; }}}
;; Function SEARCH  {{{
;; 
;; Syntax:
;; search sequence-1 sequence-2 &key from-end test test-not key start1 start2 end1 end2
;; => position
;; 
;; Arguments and Values:
;; Sequence-1---a sequence. 
;; Sequence-2---a sequence. 
;; from-end---a generalized boolean. The default is false. 
;; test---a designator for a function of two arguments that returns a generalized boolean. 
;; test-not---a designator for a function of two arguments that returns a generalized boolean. 
;; key---a designator for a function of one argument, or nil. 
;; start1, end1---bounding index designators of sequence-1. The defaults for start1 and end1 are 0 and nil, respectively. 
;; start2, end2---bounding index designators of sequence-2. The defaults for start2 and end2 are 0 and nil, respectively. 
;; position---a bounding index of sequence-2, or nil. 
;; 
;; Description:
;; Searches sequence-2 for a subsequence that matches sequence-1. 
;; The implementation may choose to search sequence-2 in any order; there is no guarantee on the number of times the test is made. For example, when start-end is true, the sequence might actually be searched from left to right instead of from right to left (but in either case would return the rightmost matching subsequence). If the search succeeds, search returns the offset into sequence-2 of the first element of the leftmost or rightmost matching subsequence, depending on from-end; otherwise search returns nil. 
;; If from-end is true, the index of the leftmost element of the rightmost matching subsequence is returned. 
;; 
;; Examples:
;;  (search "dog" "it's a dog's life") =>  7
;;  (search '(0 1) '(2 4 6 1 3 5) :key #'oddp) =>  2
;;
;; }}}
;; Function REMOVE-DUPLICATES, DELETE-DUPLICATES {{{
;; 
;; Syntax:
;; remove-duplicates sequence &key from-end test test-not start end key
;; => result-sequence
;;
;; delete-duplicates sequence &key from-end test test-not start end key
;; => result-sequence
;; 
;; Arguments and Values:
;; sequence---a proper sequence. 
;; from-end---a generalized boolean. The default is false. 
;; test---a designator for a function of two arguments that returns a generalized boolean. 
;; test-not---a designator for a function of two arguments that returns a generalized boolean. 
;; start, end---bounding index designators of sequence. The defaults for start and end are 0 and nil, respectively. 
;; key---a designator for a function of one argument, or nil. 
;; result-sequence---a sequence. 
;; 
;; Description:
;; remove-duplicates returns a modified copy of sequence from which any element that matches another element occurring in sequence has been removed. 
;; If sequence is a vector, the result is a vector that has the same actual array element type as sequence. If sequence is a list, the result is a list. 
;; delete-duplicates is like remove-duplicates, but delete-duplicates may modify sequence. 
;; The elements of sequence are compared pairwise, and if any two match, then the one occurring earlier in sequence is discarded, unless from-end is true, in which case the one later in sequence is discarded. 
;; remove-duplicates and delete-duplicates return a sequence of the same type as sequence with enough elements removed so that no two of the remaining elements match. The order of the elements remaining in the result is the same as the order in which they appear in sequence. 
;; remove-duplicates returns a sequence that may share with sequence or may be identical to sequence if no elements need to be removed. 
;; delete-duplicates, when sequence is a list, is permitted to setf any part, car or cdr, of the top-level list structure in that sequence. When sequence is a vector, delete-duplicates is permitted to change the dimensions of the vector and to slide its elements into new positions without permuting them to produce the resulting vector. 
;; 
;; Examples:
;;  (remove-duplicates "aBcDAbCd" :test #'char-equal :from-end t) =>  "aBcD"
;;  (remove-duplicates '(a b c b d d e)) =>  (A C B D E)
;;  (remove-duplicates '(a b c b d d e) :from-end t) =>  (A B C D E)
;;  (remove-duplicates '((foo #\a) (bar #\%) (baz #\A))
;;      :test #'char-equal :key #'cadr) =>  ((BAR #\%) (BAZ #\A))
;;  (remove-duplicates '((foo #\a) (bar #\%) (baz #\A)) 
;;      :test #'char-equal :key #'cadr :from-end t) =>  ((FOO #\a) (BAR #\%))
;;  (setq tester (list 0 1 2 3 4 5 6))
;;  (delete-duplicates tester :key #'oddp :start 1 :end 6) =>  (0 4 5 6)
;; 
;; Side Effects:
;; delete-duplicates might destructively modify sequence. 
;; 
;; Exceptional Situations:
;; Should signal an error of type type-error if sequence is not a proper sequence. 
;; 
;; Notes:
;; If sequence is a vector, the result might or might not be simple, and might or might not be identical to sequence. 
;; The :test-not argument is deprecated. 
;; These functions are useful for converting sequence into a canonical form suitable for representing a set. 
;;
;; }}}
