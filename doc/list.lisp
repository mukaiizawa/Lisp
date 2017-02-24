;; See Also:
;; sequence.lisp
;; set.lisp

;; Function push, pop
(defparameter *stack* nil)

(push 'a *stack*)
;; <=> (setf *stack* (cons ('a *stack*)))

(pop *stack*)
;; => A

;; Function assoc
(defparameter *alist* '((1 . a) (2 . b) (3 . c)))

(assoc 1 *alist*)
;; => (1 . A) 
(assoc 2 *alist*)
;; => (2 . B) 
(assoc 3 *alist*)
;; => (3 . C) 

;; Function SUBLIS, NSUBLIS {{{
;; 
;; Syntax:
;; sublis alist tree &key key test test-not => new-tree
;; nsublis alist tree &key key test test-not => new-tree
;; 
;; Arguments and Values:
;; alist---an association list. 
;; tree---a tree. 
;; test---a designator for a function of two arguments that returns a generalized boolean. 
;; test-not---a designator for a function of two arguments that returns a generalized boolean. 
;; key---a designator for a function of one argument, or nil. 
;; new-tree---a tree. 
;; 
;; Description:
;; sublis makes substitutions for objects in tree (a structure of conses). nsublis is like sublis but destructively modifies the relevant parts of the tree. 
;; sublis looks at all subtrees and leaves of tree; if a subtree or leaf appears as a key in alist (that is, the key and the subtree or leaf satisfy the test), it is replaced by the object with which that key is associated. This operation is non-destructive. In effect, sublis can perform several subst operations simultaneously. 
;; If sublis succeeds, a new copy of tree is returned in which each occurrence of such a subtree or leaf is replaced by the object with which it is associated. If no changes are made, the original tree is returned. The original tree is left unchanged, but the result tree may share cells with it. 
;; nsublis is permitted to modify tree but otherwise returns the same values as sublis. 
;; 
;; Examples:
;;  (sublis '((x . 100) (z . zprime))
;;          '(plus x (minus g z x p) 4 . x))
;; =>  (PLUS 100 (MINUS G ZPRIME 100 P) 4 . 100)
;;  (sublis '(((+ x y) . (- x y)) ((- x y) . (+ x y)))
;;          '(* (/ (+ x y) (+ x p)) (- x y))
;;          :test #'equal)
;; =>  (* (/ (- X Y) (+ X P)) (+ X Y))
;;  (setq tree1 '(1 (1 2) ((1 2 3)) (((1 2 3 4)))))
;; =>  (1 (1 2) ((1 2 3)) (((1 2 3 4))))
;;  (sublis '((3 . "three")) tree1) 
;; =>  (1 (1 2) ((1 2 "three")) (((1 2 "three" 4))))
;;  (sublis '((t . "string"))
;;           (sublis '((1 . "") (4 . 44)) tree1)
;;           :key #'stringp)
;; =>  ("string" ("string" 2) (("string" 2 3)) ((("string" 2 3 44))))
;;  tree1 =>  (1 (1 2) ((1 2 3)) (((1 2 3 4))))
;;  (setq tree2 '("one" ("one" "two") (("one" "Two" "three"))))
;; =>  ("one" ("one" "two") (("one" "Two" "three"))) 
;;  (sublis '(("two" . 2)) tree2) 
;; =>  ("one" ("one" "two") (("one" "Two" "three"))) 
;;  tree2 =>  ("one" ("one" "two") (("one" "Two" "three"))) 
;;  (sublis '(("two" . 2)) tree2 :test 'equal) 
;; =>  ("one" ("one" 2) (("one" "Two" "three"))) 
;;  (nsublis '((t . 'temp))
;;            tree1
;;            :key #'(lambda (x) (or (atom x) (< (list-length x) 3))))
;; =>  ((QUOTE TEMP) (QUOTE TEMP) QUOTE TEMP) 
;; 
;; Side Effects:
;; nsublis modifies tree. 
;; 
;; See Also:
;; subst, Section 3.2.1 (Compiler Terminology), Section 3.6 (Traversal Rules and Side Effects) 
;; 
;; Notes:
;; The :test-not parameter is deprecated. 
;; Because the side-effecting variants (e.g., nsublis) potentially change the path that is being traversed, their effects in the presence of shared or circular structure structure may vary in surprising ways when compared to their non-side-effecting alternatives. To see this, consider the following side-effect behavior, which might be exhibited by some implementations: 
;;  (defun test-it (fn)
;;    (let* ((shared-piece (list 'a 'b))
;;           (data (list shared-piece shared-piece)))
;;      (funcall fn '((a . b) (b . a)) data)))
;;  (test-it #'sublis) =>  ((B A) (B A))
;;  (test-it #'nsublis) =>  ((A B) (A B))
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
;; See Also:
;; substitute, nsubstitute, Section 3.2.1 (Compiler Terminology), Section 3.6 (Traversal Rules and Side Effects) 
;; 
;; Notes:
;; The :test-not parameter is deprecated. 
;; The functions subst-if-not and nsubst-if-not are deprecated. 
;; One possible definition of subst: 
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
;; 
;; }}}
