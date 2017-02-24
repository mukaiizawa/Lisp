(require :stdlib *module-stdlib*)
(require :test-utils *module-test-utils*)

(defun match? (pat line &key (start 0))
  (when (<= start (length line))
    (aif (anchored-match? pat line start)
      (list start it)
      (match? pat line :start (1+ start)))))

(defun anchored-match? (pat line i)
  (if (null pat)
    (return-from anchored-match? i)    ; matched
    (let* ((line-length (length line))
           (curr-pat (first pat))
           (rest-pat (rest pat))
           (next-pat (first rest-pat)))
      (when (or (< i line-length)
                (eq curr-pat 'end)
                (eq next-pat 'aster)
                (eq next-pat 'question))
        (cond 
          ;;; closure `*' case
          ((eq next-pat 'aster)
           (or
             ;; match greater than zero
             (closure-match? (make-sequence 'list (- line-length i) :initial-element curr-pat)
                             (rest rest-pat) line i)
             ;; match zero
             (zero-match? (rest rest-pat) line i)))
          ;;; question `?' case
          ((eq next-pat 'question)
           (or
             ;; match one
             (anchored-match? (cons curr-pat (rest rest-pat)) line i)
             ;; match zero
             (zero-match? (rest rest-pat) line i)))
          ;;; wild card `.' case
          ((eq curr-pat 'dot)
           (anchored-match? rest-pat line (1+ i)))
          ;;; group `() case
          ((alist? curr-pat)
           (and curr-pat
                (or (anchored-match? (append (first curr-pat) rest-pat) line i)
                    (anchored-match? (cons (rest curr-pat) rest-pat) line i))))
          ;;; set of characters `[]' case
          ((listp curr-pat)
           (and curr-pat
                (or (anchored-match? (cons (first curr-pat) rest-pat) line i)
                    (anchored-match? (cons (rest curr-pat) rest-pat) line i))))
          ;;; character case
          ((characterp curr-pat)
           (and (char= curr-pat (char line i))
                (anchored-match? rest-pat line (1+ i))))
          ;;; starting position `^'
          ((eq 'start curr-pat)
           (and (= i 0)
                (anchored-match? rest-pat line i)))
          ;;; ending position `$'
          ((eq 'end curr-pat)
           (and (= i line-length)
                (anchored-match? nil line i)))
          ;;; unknown
          (t
            (error "anchored-match?: An error has occurred pattern. `~A', string `~A', current position `~A'" pat line i)))))))

(defun closure-match? (closure-pat rest-pat line i)
  (and closure-pat
    (or (anchored-match? (append closure-pat rest-pat) line i)
        (closure-match? (cdr closure-pat) rest-pat line i))))

(defun zero-match? (rest-pat line i)
  (anchored-match? rest-pat line i))

(test-all
  ;; no metacharacter
  ('no-metacharacter-01
   (match? '(#\a #\b) "ab")
   '(0 2))
  ('no-metacharacter-02
   (match? '(#\a #\b) "AB")
   nil)
  ('no-metacharacter-03
   (match? '(#\a #\b) "")
   nil)
  ;; selectioin `[]'
  ('selection-01
   (match? '(#\a #\b) "ab")
   '(0 2))
  ('selection-02
   (match? '(#\a (#\b #\B) #\c) "abc")
   '(0 3))
  ('selection-03
   (match? '(#\a (#\b #\B) #\c) "aBc")
   '(0 3))
  ('selection-04
   (match? '(#\a (#\b #\B) #\c) "ababab")
   nil)
  ('selection-05
   (match? '(#\a (#\b #\B) #\c) "ababab")
   nil)
  ('selection-06
   (match? '((#\a #\b #\c) #\d) "cd")
   '(0 2))
  ('selectioin-07
   (match? '((#\a #\b)) "fdsa")
   '(3 4))
  ;; group `()'
  ('group-01
   (match? '(((#\a #\b #\c) (#\d #\e #\f))) "a")
   nil)
  ('group-02
   (match? '(((#\a #\b #\c) (#\d #\e #\f))) "abc")
   '(0 3))
  ('group-03
   (match? '(((#\a #\b #\c) (#\d #\e #\f))) "def")
   '(0 3))
  ('group-04
   (match? '(((#\a #\b #\c) (#\d #\e #\f))) "ad")
   nil)
  ('group-05
   (match? '(((#\a dot #\c) (#\d #\e #\f))) "aec")
   '(0 3))
  ;; dot
  ('dot-01
   (match? '(dot) "abc")
   '(0 1))
  ('dot-02
   (match? '(dot #\b #\c) "abc")
   '(0 3))
  ('dot-03
   (match? '(#\a dot #\c) "abc")
   '(0 3))
  ('dot-04
   (match? '(#\a #\b dot) "abc")
   '(0 3))
  ('dot-05
   (match? '(#\d dot) "abc")
   nil)
  ;; start
  ('start-01
   (match? '(start #\a) "abc")
   '(0 1))
  ('start-02
   (match? '(start #\a #\b) "abc")
   '(0 2))
  ('start-03
   (match? '(start #\b) "abc")
   nil)
  ;; end
  ('end-01
   (match? '(#\a end) "abc")
   nil)
  ('end-02
   (match? '(#\c end) "abc")
   '(2 3))
  ;; start-end
  ('start-end-01
   (match? '(start #\a end) "a")
   '(0 1))
  ('start-end-02
   (match? '(start #\a end) "abc")
   nil)
  ('start-end-03
   (match? '(start #\a dot aster end) "abc")
   '(0 3))
  ;; question
  ('question-1
   (match? '(#\a  #\b question) "abc")
   '(0 2))
  ('question-2
   (match? '(#\a  #\b question) "a")
   '(0 1))
  ('question-3
   (match? '(#\a question) "b")
   '(0 0))
  ;; closure
  ('closure-01
   (match? '(dot aster) "abc")
   '(0 3))
  ('closure-02
   (match? '(dot aster aster) "aaaaa")
   '(0 5))
  ('closure-03
   (match? '(#\a aster) "aaa")
   '(0 3))
  ('closure-04
   (match? '(#\a aster) "aab")
   '(0 2))
  ('closure-05
   (match? '(#\a aster) "b")
   '(0 0))
  ('closure-06
   (match? '(#\a aster) "")
   '(0 0))
  ('closure-07
   (match? '(#\a aster) "aa")
   '(0 2))
  ('closure-08
   (match? '(#\a aster) "a")
   '(0 1))
  ('closure-09
   (match? '(#\a (#\b #\c) aster) "acb")
   '(0 3))
  ('closure-10
   (match? '(#\a #\b aster #\c) "ac")
   '(0 2))
  ('closure-11
   (match? '(#\a #\b aster #\c) "abc")
   '(0 3))
  ('closure-12
   (match? '(#\a #\b aster #\c) "abbc")
   '(0 4))
  ('closure-13
   (match? '(#\a #\b aster #\c) "abbbc")
   '(0 5))
  ('closure-14
   (match? '(#\a aster) "a")
   '(0 1))
  ('closure-15
   (match? '((#\a #\b) aster #\c) "c")
   '(0 1))
  ('closure-16
   (match? '(#\a #\a aster #\c) "bac")
   '(1 3))
  )
