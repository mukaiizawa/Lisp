
;; Basic Output
;; {{{

;; Character
(format nil "~C" #\Space)
;; => " "

;; Newline
(format nil "~3%")

;; Fresh-line
(format nil "~3&")

;; Page
(format nil "~3|")

;; Tilde
(format nil "~3~")
;; => "~~~"

;; Output with conmma
(format nil "~:D" 1000000)
;; => "1,000,000"

;; }}}
;; Radix Control
;; {{{

;; Radix
(format nil "~2R" 10) 
;; => "1010"
(format nil "~4R" 10) 
;; => "22"
(format nil "~8R" 10) 
;; => "12"
(format nil "~10R" 10) 
;; => "10"
(format nil "~16R" 10) 
;; => "A"

;; Cardinal, Ordinal, Roman
(format nil "~R" 4) 
;; => "four"
(format nil "~:R" 4) 
;; => "fourth"
(format nil "~@R" 4) 
;; => "IV"

;; Decimal
(format nil "~8D" 12345) 
;; "   12345" 
(format nil "~:D" 10000) 
;; => "10,000"

;; Binary
(format nil "~B" 10) 
;; => "1010"

;; Octal
(format nil "~O" 10) 
;; => "12"

;; Hexadecimal
(format nil "~X" 10) 
;; => "A"

;; }}}
;; Floating-Point Printers
;; {{{

;; Floating-Point
(format nil "~10F" 3.0)
;; => "       3.0" 
(format nil "~,5F" 3.0)
;; => "3.00000" 
(format nil "~10,5F" 3.0)
;; => "   3.00000" 

;; Exponential
(format nil "~10,4E" 637.5)
;; => " 6.3750E+2" 


;; }}}
;; Printer Operations
;; {{{

;; Aesthetic
(format nil "~A" "hello")
;; => "hello" 
(format nil "~10A" "hello")
;; => "hello     " 
(format nil "~10@A" "hello")
;; => "     hello" 

(format nil "~A" nil)
;; => "NIL"
(format nil "~:A" nil)
;; => "()" 

;; Standard
;; just like ~A but arg is printed with escape characters.
(format nil "~S" "hello")
;; => "\"hello\"" 
(format nil "~10S" "hello")
;; => "\"hello\"   " 
(format nil "~10@S" "hello")
;; => "   \"hello\"" 

(format nil "~S" nil)
;; => "NIL"
(format nil "~:S" nil)
;; => "()" 


;; }}}
;; Iteration
;; {{{

(format nil "The winners are:~{ ~A~}." '(fred harry jill)) 
;; =>  "The winners are: FRED HARRY JILL."
(format nil "The winners are:~{ ~A~}." '(fred harry jill)) 
;; =>  "The winners are: FRED HARRY JILL."
(format nil "The winners are:~{ ~A~^,~}." '(fred harry jill)) 
;; =>  "The winners are: FRED HARRY JILL."

(format nil "Pairs:~{ <~A,~A>~}." '(a 1 b 2 c 3))
;; =>  "Pairs: <A,1> <B,2> <C,3>."
(format nil "Pairs:~:{ <~A,~A>~}." '((a 1) (b 2) (c 3)))
;; =>  "Pairs: <A,1> <B,2> <C,3>."
(format nil "Pairs:~@{ <~A,~A>~} ." 'a 1 'b 2 'c 3)
;; =>  "Pairs: <A,1> <B,2> <C,3>."
(format nil "Pairs:~:@{ <~S,~S>~} ." '(a 1) '(b 2) '(c 3)) 
;; =>  "Pairs: <A,1> <B,2> <C,3>."

;; }}}
;; Case Conversion
;; {{{

(format nil "before:~A, after:~1:*~(~A~)" "ASDF")
;; =>  "before:ASDF, after:asdf" 
(format nil "before:~A, after:~1:*~@(~A~)" "ASDF")
;; =>  "before:ASDF, after:Asdf" 
(format nil "before:~A, after:~1:*~:@(~A~)" "ASDF")
;; =>  "before:ASDF, after:ASDF" 


;; }}}
;; Justification
;;{{{

(format nil "~10<foo~;bar~>")
;; => "foo    bar" 
(format nil "~10:<foo~;bar~>")
;; => "  foo  bar" 
(format nil "~10<foobar~>")
;; => "    foobar" 
(format nil "~10:<foobar~>")
;; => "    foobar" 
(format nil "~10:@<foo~;bar~>")
;; => "  foo bar " 
(format nil "~10@<foobar~>")
;; => "foobar    " 
(format nil "~10:@<foobar~>")
;; => "  foobar  " 

;; }}}
;; Plural
;; {{{

(format nil "~D tr~:@P/~D win~:P" 7 1) 
;; => "7 tries/1 win" 
(format nil "~D tr~:@P/~D win~:P" 1 0) 
;; => "1 try/0 wins" 
(format nil "~D tr~:@P/~D win~:P" 1 3) 
;; => "1 try/3 wins" 

;;}}}
;; Escape Upword
;; {{{

(format nil "~15<~S~;~^~S~;~^~S~>" 'foo)
;; =>   "            FOO"
(format nil "~15<~S~;~^~S~;~^~S~>" 'foo 'bar)
;; =>   "FOO         BAR"
(format nil "~15<~S~;~^~S~;~^~S~>" 'foo 'bar 'baz)
;; =>   "FOO   BAR   BAZ"

;;}}}

