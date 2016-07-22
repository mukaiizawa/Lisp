(load "../lib/stdlib")

;; See Also: sequence.lisp

;; mkstr
(mkstr "abc" "def" #\h #\i "klmb")
;; => "abcdefhiklmb" 

;; position
(position #\3 "12345")
;; => 2

;; before, after
(before #\3 "12345")
;; => "12"
(after #\3 "12345")
;; => "45"

;; string->list
(string->list #\, "abcd,e,f,,g,hi,j,klmnop,qr,")
;; => ("abcd" "e" "f" "" "g" "hi" "j" "klmnop" "qr" "") 


;; FUNCTION STRING {{{
;; Syntax:
;; string x => string
;;
;; Arguments and Values:
;; x---a string, a symbol, or a character.
;; string---a string.
;;
;; Description:
;; Returns a string described by x; specifically:
;; If x is a string, it is returned.
;; If x is a symbol, its name is returned.
;; If x is a character, then a string containing that one character is returned.
;; string might perform additional, implementation-defined conversions.
;;
;; Examples:
;;  (string "already a string") =>  "already a string"
;;  (string 'elm) =>  "ELM"
;;  (string #\c) =>  "c"
;; }}}
;; Function MAKE-STRING {{{
;; 
;; Syntax:
;; make-string size &key initial-element element-type => string
;;
;; Arguments and Values:
;; size---a valid array dimension. 
;; initial-element---a character. The default is implementation-dependent. 
;; element-type---a type specifier. The default is character. 
;; string---a simple string. 
;; 
;; Description:
;; make-string returns a simple string of length size whose elements have been initialized to initial-element. 
;; The element-type names the type of the elements of the string; a string is constructed of the most specialized type that can accommodate elements of the given type. 
;; 
;; Examples:
;;  (make-string 10 :initial-element #\5) =>  "5555555555"
;;  (length (make-string 10)) =>  10
;; 
;; }}}
;; FUNCTION STRING=, STRING/=, STRING<, STRING>, ...{{{
;; string= string1 string2 &key start1 end1 start2 end2 => generalized-boolean
;; string/= string1 string2 &key start1 end1 start2 end2 => mismatch-index
;; string< string1 string2 &key start1 end1 start2 end2 => mismatch-index
;; string> string1 string2 &key start1 end1 start2 end2 => mismatch-index
;; string<= string1 string2 &key start1 end1 start2 end2 => mismatch-index
;; string>= string1 string2 &key start1 end1 start2 end2 => mismatch-index
;; string-equal string1 string2 &key start1 end1 start2 end2 => generalized-boolean
;; string-not-equal string1 string2 &key start1 end1 start2 end2 => mismatch-index
;; string-lessp string1 string2 &key start1 end1 start2 end2 => mismatch-index
;; string-greaterp string1 string2 &key start1 end1 start2 end2 => mismatch-index
;; string-not-greaterp string1 string2 &key start1 end1 start2 end2 => mismatch-index
;; string-not-lessp string1 string2 &key start1 end1 start2 end2 => mismatch-index

;; string1---a string designator.
;; string2---a string designator.
;; start1, end1---bounding index designators of string1. The defaults for start and end are 0 and nil, respectively.
;; start2, end2---bounding index designators of string2. The defaults for start and end are 0 and nil, respectively.
;; generalized-boolean---a generalized boolean.
;; mismatch-index---a bounding index of string1, or nil.

(string= "foo" "foo")
;; =>  true
(string= "foo" "Foo")
;; =>  false
(string= "foo" "bar")
;; =>  false
(string= "together" "frog" :start1 1 :end1 3 :start2 2)
;; =>  true
(string-equal "foo" "Foo")
;; =>  true
(string= "abcd" "01234abcd9012" :start2 5 :end2 9)
;; =>  true
(string< "aaaa" "aaab")
;; =>  3
(string>= "aaaaa" "aaaa")
;; =>  4
(string-not-greaterp "Abcde" "abcdE")
;; =>  5
(string-lessp "012AAAA789" "01aaab6" :start1 3 :end1 7
              :start2 2 :end2 6)
;; =>  6
(string-not-equal "AAAA" "aaaA")
;; =>  false

;; }}}
;; Function STRING-UPCASE, STRING-DOWNCASE, STRING-CAPITALIZE, NSTRING-UPCASE, NSTRING-DOWNCASE, NSTRING-CAPITALIZE {{{
;; 
;; Syntax:
;; string-upcase string &key start end => cased-string
;; string-downcase string &key start end => cased-string
;; string-capitalize string &key start end => cased-string
;; nstring-upcase string &key start end => string
;; nstring-downcase string &key start end => string
;; nstring-capitalize string &key start end => string
;; 
;; Arguments and Values:
;; string---a string designator. For nstring-upcase, nstring-downcase, and nstring-capitalize, the string designator must be a string. 
;; start, end---bounding index designators of string. The defaults for start and end are 0 and nil, respectively. 
;; cased-string---a string. 
;; 
;; Description:
;; string-upcase, string-downcase, string-capitalize, nstring-upcase, nstring-downcase, nstring-capitalize change the case of the subsequence of string bounded by start and end as follows: 
;; string-upcase 
;; string-upcase returns a string just like string with all lowercase characters replaced by the corresponding uppercase characters. More precisely, each character of the result string is produced by applying the function char-upcase to the corresponding character of string. 
;; string-downcase 
;; string-downcase is like string-upcase except that all uppercase characters are replaced by the corresponding lowercase characters (using char-downcase). 
;; string-capitalize 
;; string-capitalize produces a copy of string such that, for every word in the copy, the first character of the ``word,'' if it has case, is uppercase and any other characters with case in the word are lowercase. For the purposes of string-capitalize, a ``word'' is defined to be a consecutive subsequence consisting of alphanumeric characters, delimited at each end either by a non-alphanumeric character or by an end of the string. 
;; nstring-upcase, nstring-downcase, nstring-capitalize  
;; nstring-upcase, nstring-downcase, and nstring-capitalize are identical to string-upcase, string-downcase, and string-capitalize respectively except that they modify string. 
;; For string-upcase, string-downcase, and string-capitalize, string is not modified. However, if no characters in string require conversion, the result may be either string or a copy of it, at the implementation's discretion. 
;; 
;; Examples:
;;  (string-upcase "abcde") =>  "ABCDE"
;;  (string-upcase "Dr. Livingston, I presume?")
;; =>  "DR. LIVINGSTON, I PRESUME?"
;;  (string-upcase "Dr. Livingston, I presume?" :start 6 :end 10)
;; =>  "Dr. LiVINGston, I presume?"
;;  (string-downcase "Dr. Livingston, I presume?")
;; =>  "dr. livingston, i presume?"
;; 
;;  (string-capitalize "elm 13c arthur;fig don't") =>  "Elm 13c Arthur;Fig Don'T"
;;  (string-capitalize " hello ") =>  " Hello "
;;  (string-capitalize "occlUDeD cASEmenTs FOreSTAll iNADVertent DEFenestraTION")
;; =>   "Occluded Casements Forestall Inadvertent Defenestration"
;;  (string-capitalize 'kludgy-hash-search) =>  "Kludgy-Hash-Search"
;;  (string-capitalize "DON'T!") =>  "Don'T!"    ;not "Don't!"
;;  (string-capitalize "pipe 13a, foo16c") =>  "Pipe 13a, Foo16c"
;; 
;;  (setq str (copy-seq "0123ABCD890a")) =>  "0123ABCD890a"
;;  (nstring-downcase str :start 5 :end 7) =>  "0123AbcD890a"
;;  str =>  "0123AbcD890a"
;; 
;; Side Effects:
;; nstring-upcase, nstring-downcase, and nstring-capitalize modify string as appropriate rather than constructing a new string. 
;; 
;; }}}
;; Function SEARCH {{{
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
;; Notes:
;; The :test-not argument is deprecated. 
;; 
;; 
;; }}}
