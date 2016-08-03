
(require "stdlib" *module-stdlib*)

(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(defmacro abbrevs (&rest pairs)
  `(progn
     ,@(mapcar (lambda (pair)
                 `(abbrev ,@pair))
               pairs)))

(abbrevs 
  (+= incf)
  (-= decf)
  (dbind destructuring-bind)
  (mbind multiple-value-bind)
  (mlist multiple-value-list)
  (hash-size hash-table-count)
  (mkhash make-hash-table))

;; change predicates from `xxxp' to `xxx?'.
(defmacro defpredicates (&rest predicates)
  (labels ((change-name (str)
                        (let* ((len (length str))
                               (lastchar (char str (1- len))))
                          (if (or (char-equal lastchar #\p)     ; like a `listp'
                                  (char-equal lastchar #\-))    ; like a `alpha-char-p'
                            (change-name (subseq str 0 (1- len)))
                            (mksym (mkstr str #\?))))))
    `(progn
       ,@(mapcar (lambda (predicate)
                   `(abbrev ,(change-name (mkstr predicate)) ,predicate))
                 `,predicates))))

(defpredicates
  ;; end with #\p {{{

  adjustable-array-p
  alpha-char-p
  alphanumericp
  array-has-fill-pointer-p
  array-in-bounds-p
  arrayp
  bit-vector-p
  both-case-p
  boundp
  char-greaterp
  char-lessp
  char-not-greaterp
  char-not-lessp
  characterp
  compiled-function-p
  complexp
  consp
  constantp
  digit-char-p
  endp
  equalp
  evenp
  fboundp
  floatp
  functionp
  graphic-char-p
  hash-table-p
  input-stream-p
  integerp
  interactive-stream-p
  keywordp
  listp
  logbitp
  lower-case-p
  minusp
  next-method-p
  numberp
  oddp
  open-stream-p
  output-stream-p
  packagep
  pathname-match-p
  pathnamep
  plusp
  random-state-p
  rationalp
  readtablep
  realp
  remprop
  simple-bit-vector-p
  simple-string-p
  simple-vector-p
  slot-boundp
  slot-exists-p
  special-operator-p
  standard-char-p
  streamp
  string-greaterp
  string-lessp
  string-not-greaterp
  string-not-lessp
  stringp
  subsetp
  subtypep
  symbolp
  tailp
  typep
  upper-case-p
  vector-pop
  vectorp
  wild-pathname-p
  y-or-n-p
  yes-or-no-p
  zerop

  ; }}}
  ; other predicates {{{

  eq
  eql
  ; equal
  ; equalp
  null)

  ;; }}}

;; Examples:
(eq 'a 'a)
;; ==
(eq? 'a 'a)
;; => t

