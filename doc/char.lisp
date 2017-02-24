;; Accessor CHAR, SCHAR {{{
;; Syntax:
;; char string index => character
;; schar string index => character
;; (setf (char string index) new-character)
;; (setf (schar string index) new-character)
;; 
;; Arguments and Values:
;; string---for char, a string; for schar, a simple string.
;; index---a valid array index for the string.
;; character, new-character---a character.
;; 
;; Description:
;; char and schar access the element of string specified by index.
;; char ignores fill pointers when accessing elements.
;; 
;; Examples:
;;  (setq my-simple-string (make-string 6 :initial-element #\A)) =>  "AAAAAA"
;;  (schar my-simple-string 4) =>  #\A
;;  (setf (schar my-simple-string 4) #\B) =>  #\B
;;  my-simple-string =>  "AAAABA"
;;  (setq my-filled-string
;;        (make-array 6 :element-type 'character
;;                      :fill-pointer 5
;;                      :initial-contents my-simple-string))
;; =>  "AAAAB"
;;  (char my-filled-string 4) =>  #\B
;;  (char my-filled-string 5) =>  #\A
;;  (setf (char my-filled-string 3) #\C) =>  #\C
;;  (setf (char my-filled-string 5) #\D) =>  #\D
;;  (setf (fill-pointer my-filled-string) 6) =>  6
;;  my-filled-string =>  "AAACBD"
;;  }}}
;; Function CHAR=, CHAR/=, CHAR<, CHAR>, CHAR<=, CHAR>=, CHAR-EQUAL, CHAR-NOT-EQUAL, CHAR-LESSP, CHAR-GREATERP, CHAR-NOT-GREATERP, CHAR-NOT-LESSP {{{
;; Syntax:
;; char= &rest characters+ => generalized-boolean
;; char/= &rest characters+ => generalized-boolean
;; char< &rest characters+ => generalized-boolean
;; char> &rest characters+ => generalized-boolean
;; char<= &rest characters+ => generalized-boolean
;; char>= &rest characters+ => generalized-boolean
;; char-equal &rest characters+ => generalized-boolean
;; char-not-equal &rest characters+ => generalized-boolean
;; char-lessp &rest characters+ => generalized-boolean
;; char-greaterp &rest characters+ => generalized-boolean
;; char-not-greaterp &rest characters+ => generalized-boolean
;; char-not-lessp &rest characters+ => generalized-boolean
;; 
;; Arguments and Values:
;; character---a character.
;; generalized-boolean---a generalized boolean.
;; 
;; Description:
;; These predicates compare characters.
;; char= returns true if all characters are the same; otherwise, it returns false. If two characters differ in any implementation-defined attributes, then they are not char=.
;; char/= returns true if all characters are different; otherwise, it returns false.
;; char< returns true if the characters are monotonically increasing; otherwise, it returns false. If two characters have identical implementation-defined attributes, then their ordering by char< is consistent with the numerical ordering by the predicate < on their codes.
;; char> returns true if the characters are monotonically decreasing; otherwise, it returns false. If two characters have identical implementation-defined attributes, then their ordering by char> is consistent with the numerical ordering by the predicate > on their codes.
;; char<= returns true if the characters are monotonically nondecreasing; otherwise, it returns false. If two characters have identical implementation-defined attributes, then their ordering by char<= is consistent with the numerical ordering by the predicate <= on their codes.
;; char>= returns true if the characters are monotonically nonincreasing; otherwise, it returns false. If two characters have identical implementation-defined attributes, then their ordering by char>= is consistent with the numerical ordering by the predicate >= on their codes.
;; char-equal, char-not-equal, char-lessp, char-greaterp, char-not-greaterp, and char-not-lessp are similar to char=, char/=, char<, char>, char<=, char>=, respectively, except that they ignore differences in case and might have an implementation-defined behavior for non-simple characters. For example, an implementation might define that char-equal, etc. ignore certain implementation-defined attributes. The effect, if any, of each implementation-defined attribute upon these functions must be specified as part of the definition of that attribute.
;; 
;; Examples:
;; 
;;  (char= #\d #\d) =>  true
;;  (char= #\A #\a) =>  false
;;  (char= #\d #\x) =>  false
;;  (char= #\d #\D) =>  false
;;  (char/= #\d #\d) =>  false
;;  (char/= #\d #\x) =>  true
;;  (char/= #\d #\D) =>  true
;;  (char= #\d #\d #\d #\d) =>  true
;;  (char/= #\d #\d #\d #\d) =>  false
;;  (char= #\d #\d #\x #\d) =>  false
;;  (char/= #\d #\d #\x #\d) =>  false
;;  (char= #\d #\y #\x #\c) =>  false
;;  (char/= #\d #\y #\x #\c) =>  true
;;  (char= #\d #\c #\d) =>  false
;;  (char/= #\d #\c #\d) =>  false
;;  (char< #\d #\x) =>  true
;;  (char<= #\d #\x) =>  true
;;  (char< #\d #\d) =>  false
;;  (char<= #\d #\d) =>  true
;;  (char< #\a #\e #\y #\z) =>  true
;;  (char<= #\a #\e #\y #\z) =>  true
;;  (char< #\a #\e #\e #\y) =>  false
;;  (char<= #\a #\e #\e #\y) =>  true
;;  (char> #\e #\d) =>  true
;;  (char>= #\e #\d) =>  true
;;  (char> #\d #\c #\b #\a) =>  true
;;  (char>= #\d #\c #\b #\a) =>  true
;;  (char> #\d #\d #\c #\a) =>  false
;;  (char>= #\d #\d #\c #\a) =>  true
;;  (char> #\e #\d #\b #\c #\a) =>  false
;;  (char>= #\e #\d #\b #\c #\a) =>  false
;;  (char> #\z #\A) =>  implementation-dependent
;;  (char> #\Z #\a) =>  implementation-dependent
;;  (char-equal #\A #\a) =>  true
;;  (stable-sort (list #\b #\A #\B #\a #\c #\C) #'char-lessp)
;; =>  (#\A #\a #\b #\B #\c #\C)
;;  (stable-sort (list #\b #\A #\B #\a #\c #\C) #'char<)
;; =>  (#\A #\B #\C #\a #\b #\c) ;Implementation A
;; =>  (#\a #\b #\c #\A #\B #\C) ;Implementation B
;; =>  (#\a #\A #\b #\B #\c #\C) ;Implementation C
;; =>  (#\A #\a #\B #\b #\C #\c) ;Implementation D
;; =>  (#\A #\B #\a #\b #\C #\c) ;Implementation E
;;
;; }}}
;; Function CHARACTER {{{
;; Syntax:
;; character character => denoted-character
;; 
;; Arguments and Values:
;; character---a character designator.
;; denoted-character---a character.
;; 
;; Description:
;; Returns the character denoted by the character designator.
;; 
;; Examples:
;; 
;;  (character #\a) =>  #\a
;;  (character "a") =>  #\a
;;  (character 'a) =>  #\A
;;  (character '\a) =>  #\a
;;  (character 65.) is an error.
;;  (character 'apple) is an error.
;;  }}}
;; Function CHARACTERP {{{
;; Syntax:
;; characterp object => generalized-boolean
;; 
;; Arguments and Values:
;; object---an object.
;; generalized-boolean---a generalized boolean.
;; 
;; Description:
;; Returns true if object is of type character; otherwise, returns false.
;; 
;; Examples:
;;
;;  (characterp #\a) =>  true
;;  (characterp 'a) =>  false
;;  (characterp "a") =>  false
;;  (characterp 65.) =>  false
;;  (characterp #\Newline) =>  true
;;  ;; This next example presupposes an implementation 
;;  ;; in which #\Rubout is an implementation-defined character.
;;  (characterp #\Rubout) =>  true
;;
;;  }}}
;; Function GRAPHIC-CHAR-P {{{
;;
;; Syntax:
;; graphic-char-p char => generalized-boolean
;; 
;; Arguments and Values:
;; char---a character.
;; generalized-boolean---a generalized boolean.
;; 
;; Description:
;; Returns true if character is a graphic character; otherwise, returns false.
;; 
;; Examples:
;;  (graphic-char-p #\G) =>  true
;;  (graphic-char-p #\#) =>  true
;;  (graphic-char-p #\Space) =>  true
;;  (graphic-char-p #\Newline) =>  false
;; 
;; }}}
;; Function ALPHA-CHAR-P {{{
;; Syntax:
;; alpha-char-p character => generalized-boolean
;; 
;; Arguments and Values:
;; character---a character.
;; generalized-boolean---a generalized boolean.
;; 
;; Description:
;; Returns true if character is an alphabetic[1] character; otherwise, returns false.
;; 
;; Examples:
;;  (alpha-char-p #\a) =>  true
;;  (alpha-char-p #\5) =>  false
;;  (alpha-char-p #\Newline) =>  false
;;  ;; This next example presupposes an implementation
;;  ;; in which #\<ALPHA> is a defined character.
;;  (alpha-char-p #\<ALPHA>) =>  implementation-dependent
;;
;;  }}}
;; Function ALPHANUMERICP {{{
;; 
;; Syntax:
;; alphanumericp character => generalized-boolean
;; Arguments and Values:
;; character---a character.
;; generalized-boolean---a generalized boolean.
;; 
;; Description:
;; Returns true if character is an alphabetic[1] character or a numeric character; otherwise, returns false.
;; 
;; Examples:
;;  (alphanumericp #\Z) =>  true
;;  (alphanumericp #\9) =>  true
;;  (alphanumericp #\Newline) =>  false
;;  (alphanumericp #\#) =>  false
;;
;;  }}}
;; Function DIGIT-CHAR {{{
;; 
;; Syntax:
;; digit-char weight &optional radix => char
;; 
;; Arguments and Values:
;; weight---a non-negative integer.
;; radix---a radix. The default is 10.
;; char---a character or false.
;; 
;; Description:
;; If weight is less than radix, digit-char returns a character which has that weight when considered as a digit in the specified radix. If the resulting character is to be an alphabetic[1] character, it will be an uppercase character.
;; If weight is greater than or equal to radix, digit-char returns false.
;; 
;; Examples:
;;  (digit-char 0) =>  #\0
;;  (digit-char 10 11) =>  #\A
;;  (digit-char 10 10) =>  false
;;  (digit-char 7) =>  #\7
;;  (digit-char 12) =>  false
;;  (digit-char 12 16) =>  #\C  ;not #\c
;;  (digit-char 6 2) =>  false
;;  (digit-char 1 2) =>  #\1
;; }}}
;; Function DIGIT-CHAR-P {{{
;;
;; Syntax:
;; digit-char-p char &optional radix => weight
;; 
;; Arguments and Values:
;; char---a character.
;; radix---a radix. The default is 10.
;; weight---either a non-negative integer less than radix, or false.
;; 
;; Description:
;; Tests whether char is a digit in the specified radix (i.e., with a weight less than radix). If it is a digit in that radix, its weight is returned as an integer; otherwise nil is returned.
;; 
;; Examples:
;;  (digit-char-p #\5)    =>  5
;;  (digit-char-p #\5 2)  =>  false
;;  (digit-char-p #\A)    =>  false
;;  (digit-char-p #\a)    =>  false
;;  (digit-char-p #\A 11) =>  10
;;  (digit-char-p #\a 11) =>  10
;;  (mapcar #'(lambda (radix) 
;;              (map 'list #'(lambda (x) (digit-char-p x radix)) 
;;                   "059AaFGZ"))
;;          '(2 8 10 16 36))
;;  =>  ((0 NIL NIL NIL NIL NIL NIL NIL)
;;      (0 5 NIL NIL NIL NIL NIL NIL)
;;      (0 5 9 NIL NIL NIL NIL NIL)
;;      (0 5 9 10 10 15 NIL NIL)
;;      (0 5 9 10 10 15 16 35))
;; }}}
;; Function CHAR-UPCASE, CHAR-DOWNCASE {{{
;; Syntax:
;; char-upcase character => corresponding-character
;; char-downcase character => corresponding-character
;; 
;; Arguments and Values:
;; character, corresponding-character---a character.
;; 
;; Description:
;; If character is a lowercase character, char-upcase returns the corresponding uppercase character. Otherwise, char-upcase just returns the given character.
;; If character is an uppercase character, char-downcase returns the corresponding lowercase character. Otherwise, char-downcase just returns the given character.
;; The result only ever differs from character in its code attribute; all implementation-defined attributes are preserved.
;; 
;; Examples:
;;  (char-upcase #\a) =>  #\A
;;  (char-upcase #\A) =>  #\A
;;  (char-downcase #\a) =>  #\a
;;  (char-downcase #\A) =>  #\a
;;  (char-upcase #\9) =>  #\9
;;  (char-downcase #\9) =>  #\9
;;  (char-upcase #\@) =>  #\@
;;  (char-downcase #\@) =>  #\@
;; Note that this next example might run for a very long time in 
;; some implementations if CHAR-CODE-LIMIT happens to be very large
;; for that implementation.
;;  (dotimes (code char-code-limit)
;;    (let ((char (code-char code)))
;;      (when char
;;        (unless (cond ((upper-case-p char) (char= (char-upcase (char-downcase char)) char))
;;                      ((lower-case-p char) (char= (char-downcase (char-upcase char)) char))
;;                      (t (and (char= (char-upcase (char-downcase char)) char)
;;                              (char= (char-downcase (char-upcase char)) char))))
;;          (return char)))))
;; =>  NIL
;; }}}
;; Function UPPER-CASE-P, LOWER-CASE-P, BOTH-CASE-P {{{
;;
;; Syntax:
;; upper-case-p character => generalized-boolean
;; lower-case-p character => generalized-boolean
;; both-case-p character => generalized-boolean
;; 
;; Arguments and Values:
;; character---a character.
;; generalized-boolean---a generalized boolean.
;; 
;; Description:
;; These functions test the case of a given character.
;; upper-case-p returns true if character is an uppercase character; otherwise, returns false.
;; lower-case-p returns true if character is a lowercase character; otherwise, returns false.
;; both-case-p returns true if character is a character with case; otherwise, returns false.
;; 
;; Examples:
;;  (upper-case-p #\A) =>  true
;;  (upper-case-p #\a) =>  false
;;  (both-case-p #\a) =>  true
;;  (both-case-p #\5) =>  false
;;  (lower-case-p #\5) =>  false
;;  (upper-case-p #\5) =>  false
;;  ;; This next example presupposes an implementation 
;;  ;; in which #\Bell is an implementation-defined character.
;;  (lower-case-p #\Bell) =>  false
;; }}}
