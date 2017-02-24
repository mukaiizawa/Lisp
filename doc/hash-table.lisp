(load "../lib/stdlib")

(defparameter ht (make-hash-table))

(gethash 'color ht)
;; => NIL
;;    NIL

(setf (gethash 'color ht) 'red)
;; => RED

(gethash 'color ht)
;; => RED
;;    T

(push 'dog (gethash 'animal ht))
;; => DOG

(gethash 'animal ht)
;; => DOG
;;    T

(remhash 'animal ht)
;; => T

(remhash 'animal ht)
;; => NIL

(setf (gethash 'shape ht) 'spherical
      (gethash 'size ht) 'giant)
;; => GIANT

(maphash (lambda (k v)
           (format t "~A = ~A~%" k v))
         ht)
;; => SIZE = GIANT
;;    SHAPE = SPHERICAL
;;    COLOR = RED

(clrhash ht)

(dotimes (i 100)
  (setf (gethash i ht) (* i i)))

(dotimes (i (hash-table-count ht))
  (print (gethash i ht)))


;; Function MAKE-HASH-TABLE {{{
;;
;; Syntax:
;; make-hash-table &key test size rehash-size rehash-threshold => hash-table
;; Arguments and Values:
;; test---a designator for one of the functions eq, eql, equal, or equalp. The default is eql.
;; size---a non-negative integer. The default is implementation-dependent.
;; rehash-size---a real of type (or (integer 1 *) (float (1.0) *)). The default is implementation-dependent.
;; rehash-threshold---a real of type (real 0 1). The default is implementation-dependent.
;; hash-table---a hash table.
;; 
;; Description:
;; Creates and returns a new hash table.
;; test determines how keys are compared. An object is said to be present in the hash-table if that object is the same under the test as the key for some entry in the hash-table.
;; size is a hint to the implementation about how much initial space to allocate in the hash-table. This information, taken together with the rehash-threshold, controls the approximate number of entries which it should be possible to insert before the table has to grow. The actual size might be rounded up from size to the next `good' size; for example, some implementations might round to the next prime number.
;; rehash-size specifies a minimum amount to increase the size of the hash-table when it becomes full enough to require rehashing; see rehash-theshold below. If rehash-size is an integer, the expected growth rate for the table is additive and the integer is the number of entries to add; if it is a float, the expected growth rate for the table is multiplicative and the float is the ratio of the new size to the old size. As with size, the actual size of the increase might be rounded up.
;; rehash-threshold specifies how full the hash-table can get before it must grow. It specifies the maximum desired hash-table occupancy level.
;; The values of rehash-size and rehash-threshold do not constrain the implementation to use any particular method for computing when and by how much the size of hash-table should be enlarged. Such decisions are implementation-dependent, and these values only hints from the programmer to the implementation, and the implementation is permitted to ignore them.
;; 
;; Examples:
;;  (setq table (make-hash-table)) =>  #<HASH-TABLE EQL 0/120 46142754>
;;  (setf (gethash "one" table) 1) =>  1
;;  (gethash "one" table) =>  NIL, false
;;  (setq table (make-hash-table :test 'equal)) =>  #<HASH-TABLE EQUAL 0/139 46145547>
;;  (setf (gethash "one" table) 1) =>  1
;;  (gethash "one" table) =>  1, T
;;  (make-hash-table :rehash-size 1.5 :rehash-threshold 0.7) 
;; =>  #<HASH-TABLE EQL 0/120 46156620>
;; }}}
;; Function HASH-TABLE-COUNT {{{
;; 
;; Syntax:
;; hash-table-count hash-table => count
;; Arguments and Values:
;; hash-table---a hash table.
;; count---a non-negative integer.
;; 
;; Description:
;; Returns the number of entries in the hash-table. If hash-table has just been created or newly cleared (see clrhash) the entry count is 0.
;; 
;; Examples:
;;  (setq table (make-hash-table)) =>  #<HASH-TABLE EQL 0/120 32115135>
;;  (hash-table-count table) =>  0
;;  (setf (gethash 57 table) "fifty-seven") =>  "fifty-seven"
;;  (hash-table-count table) =>  1
;;  (dotimes (i 100) (setf (gethash i table) i)) =>  NIL
;;  (hash-table-count table) =>  100
;; Side Effects: None.
;; 
;; Notes:
;; 
;; The following relationships are functionally correct, although in practice using hash-table-count is probably much faster:
;; 
;;  (hash-table-count table) == 
;;  (loop for value being the hash-values of table count t) == 
;;  (let ((total 0))
;;    (maphash #'(lambda (key value)
;;                 (declare (ignore key value))
;;                 (incf total))
;;             table)
;;    total)
;; }}}
;; Function HASH-TABLE-SIZE {{{
;;
;; Syntax:
;; hash-table-size hash-table => size
;; 
;; Arguments and Values:
;; hash-table---a hash table.
;; size---a non-negative integer.
;; 
;; Description:
;; Returns the current size of hash-table,
;; which is suitable for use in a call to make-hash-table in order to produce a hash table with state corresponding to the current state of the hash-table.
;; 
;; 
;; Exceptional Situations:
;; Should signal an error of type type-error if hash-table is not a hash table.
;; 
;; 
;; }}}
;; Function CLRHASH {{{
;; 
;; Syntax:
;; clrhash hash-table => hash-table
;; 
;; Arguments and Values:
;; hash-table---a hash table.
;; 
;; Description:
;; Removes all entries from hash-table, and then returns that empty hash table.
;; 
;; Examples:
;;  (setq table (make-hash-table)) =>  #<HASH-TABLE EQL 0/120 32004073>
;;  (dotimes (i 100) (setf (gethash i table) (format nil "~R" i))) =>  NIL
;;  (hash-table-count table) =>  100
;;  (gethash 57 table) =>  "fifty-seven", true
;;  (clrhash table) =>  #<HASH-TABLE EQL 0/120 32004073>
;;  (hash-table-count table) =>  0
;;  (gethash 57 table) =>  NIL, false
;;
;; }}}
;; Accessor GETHASH {{{
;; 
;; Syntax:
;; gethash key hash-table &optional default => value, present-p
;; (setf (gethash key hash-table &optional default) new-value)
;; 
;; Arguments and Values:
;; key---an object.
;; hash-table---a hash table.
;; default---an object. The default is nil.
;; value---an object.
;; present-p---a generalized boolean.
;; 
;; Description:
;; Value is the object in hash-table whose key is the same as key under the hash-table's equivalence test. If there is no such entry, value is the default.
;; Present-p is true if an entry is found; otherwise, it is false.
;; setf may be used with gethash to modify the value associated with a given key, or to add a new entry. When a gethash form is used as a setf place, any default which is supplied is evaluated according to normal left-to-right evaluation rules, but its value is ignored.
;; 
;; Examples:
;;  (setq table (make-hash-table)) =>  #<HASH-TABLE EQL 0/120 32206334>
;;  (gethash 1 table) =>  NIL, false
;;  (gethash 1 table 2) =>  2, false
;;  (setf (gethash 1 table) "one") =>  "one"
;;  (setf (gethash 2 table "two") "two") =>  "two"
;;  (gethash 1 table) =>  "one", true
;;  (gethash 2 table) =>  "two", true
;;  (gethash nil table) =>  NIL, false
;;  (setf (gethash nil table) nil) =>  NIL 
;;  (gethash nil table) =>  NIL, true
;;  (defvar *counters* (make-hash-table)) =>  *COUNTERS*
;;  (gethash 'foo *counters*) =>  NIL, false
;;  (gethash 'foo *counters* 0) =>  0, false
;;  (defmacro how-many (obj) `(values (gethash ,obj *counters* 0))) =>  HOW-MANY
;;  (defun count-it (obj) (incf (how-many obj))) =>  COUNT-IT
;;  (dolist (x '(bar foo foo bar bar baz)) (count-it x))
;;  (how-many 'foo) =>  2
;;  (how-many 'bar) =>  3
;;  (how-many 'quux) =>  0
;; 
;; }}}
;; Function REMHASH {{{
;;
;; Syntax:
;; remhash key hash-table => generalized-boolean
;;
;; Arguments and Values:
;; key---an object.
;; hash-table---a hash table.
;; generalized-boolean---a generalized boolean.
;;
;; Description:
;; Removes the entry for key in hash-table, if any. Returns true if there was such an entry, or false otherwise.
;;
;; Examples:
;;  (setq table (make-hash-table)) =>  #<HASH-TABLE EQL 0/120 32115666>
;;  (setf (gethash 100 table) "C") =>  "C"
;;  (gethash 100 table) =>  "C", true
;;  (remhash 100 table) =>  true
;;  (gethash 100 table) =>  NIL, false
;;  (remhash 100 table) =>  false
;;
;; }}}
;; Function MAPHASH {{{
;; 
;; Syntax:
;; maphash function hash-table => nil
;; 
;; Arguments and Values:
;; function---a designator for a function of two arguments, the key and the value.
;; hash-table---a hash table.
;; 
;; Description:
;; Iterates over all entries in the hash-table. For each entry, the function is called with two arguments--the key and the value of that entry.
;; The consequences are unspecified if any attempt is made to add or remove an entry from the hash-table while a maphash is in progress, with two exceptions: the function can use can use setf of gethash to change the value part of the entry currently being processed, or it can use remhash to remove that entry.
;; 
;; Examples:
;;  (setq table (make-hash-table)) =>  #<HASH-TABLE EQL 0/120 32304110>
;;  (dotimes (i 10) (setf (gethash i table) i)) =>  NIL
;;  (let ((sum-of-squares 0))
;;     (maphash #'(lambda (key val) 
;;                  (let ((square (* val val)))
;;                    (incf sum-of-squares square)
;;                    (setf (gethash key table) square)))
;;              table)
;;     sum-of-squares) =>  285
;;  (hash-table-count table) =>  10
;;  (maphash #'(lambda (key val)
;;                (when (oddp val) (remhash key table)))
;;            table) =>  NIL
;;  (hash-table-count table) =>  5
;;  (maphash #'(lambda (k v) (print (list k v))) table)
;; (0 0) 
;; (8 64) 
;; (2 4) 
;; (6 36) 
;; (4 16) 
;; =>  NIL
;; 
;; }}}
