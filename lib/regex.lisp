
(require "stdlib" *module-stdlib*)
(require "ahead-reader" *module-ahead-reader*)
(require "test-utils" *module-test-utils*)
(provide "regex")

;; read-macro #~ {{{

(set-dispatch-macro-character #\# #\~
  (lambda (stream c n)
    (declare (ignore c n))
    (with-ahead-reader (reader stream)
      (let* ((match-mode (get-curr (read-next reader :cache nil)))
             (segment (get-curr (read-next reader :cache nil)))
             (segment-reader (lambda ()
                               (do ((result))
                                 ((and (reader-next-in? reader segment)
                                       (read-next reader :cache nil))    ; skip segment
                                  (nreverse result))
                                 (cond ((reach-eof? reader)
                                        (error "read-macro #~~: reach eof."))
                                       ((and (reader-next-in? reader #\$)
                                             (char= (get-next reader 2) #\{))
                                        (push
                                          (intern
                                            (string-upcase
                                              (get-buf
                                                (read-paren (read-next reader :cache nil)))))    ; skip `$'
                                          result))
                                       (t
                                         (push (get-buf (read-next reader))
                                               result)))))))
        (with-gensyms (text)
          (cond
            ((char= match-mode #\m)
             `(lambda (,text)
                (match?->string
                  (apply #'mkstr (list ,@(funcall segment-reader)))    ; pat
                  ,text)))
            ((char= match-mode #\s)
             `(lambda (,text)
                (match?->replace
                  (apply #'mkstr (list ,@(funcall segment-reader)))    ; replace from
                  (apply #'mkstr (list ,@(funcall segment-reader)))    ; replace to
                  ,text
                  ,(and (reader-next-in? reader #\g)
                        (read-next reader)))))
            (t
              (error "Read macro #~~: Unknown mode character `~A'" match-mode))))))))

;; }}}
;; match?->string {{{

(defun match?->string (pat line &key (start 0))
  (awhen (match? pat line :start start)
    (apply #'subseq line it)))

;; }}}
;; match?->replace {{{

(defun match?->replace (pat to line &optional g)
  (labels ((rec (line start)
                (aif (match? pat line :start start)
                  (let ((new-line 
                          (concatenate 'string
                                       (subseq line 0 (first it))
                                       to
                                       (subseq line (second it)))))
                    (if g
                      (rec new-line (+ start (length to)))
                      new-line))
                  line)))
    (rec line 0)))

;; }}}
;; regex {{{

(defstruct regex
  (key nil :type symbol)
  (pat nil :type t)
  (closure (make-closure) :type closure))

;; }}}
;; closure {{{

(defstruct closure
  (key 'none :type symbol)
  (greedy? t :type boolean)
  (n -1 :type number)
  (m -1 :type number))

;; * Matches the preceding element zero or more times.
;; ? Matches the preceding element zero or one times.
;; + Matches the preceding element one or more times.
;; {n} The preceding item is matched exactly n times. 
;; {n,} The preceding item is matched n or more times. 
;; {n,m} The preceding item is matched at least n times, but not more than m times. 

;; closure-key kind
;; 'none
;; 'exactory-n
;; 'more-n
;; 'between-n-and-m

;; }}}
;; regex-eq? {{{

(defmethod regex-eq? ((regex regex) (key symbol))
  (or (eq (closure-key (regex-closure regex)) key)
      (eq (regex-key regex) key)))

;; }}}
;; regex-replace-closure {{{

(defmethod regex-replace-closure ((regex regex) (closure closure))
  (let ((new-regex (copy-structure regex)))
    (setf (regex-closure new-regex) (copy-structure closure))
    (values new-regex)))

;; }}}
;; regex-enable-zero-match? {{{

(defmethod regex-enable-zero-match? ((regex regex))
  (and (not (null (closure-key (regex-closure regex))))
       (= (closure-n (regex-closure regex)) 0)))

;; }}}
;; regex-more-n->between-n-and-m {{{

(defmethod regex-more-n->between-n-and-m ((regex regex) (m fixnum))
  (let ((closure (regex-closure regex)))
    (when (<= (closure-n closure) m)
      (values (regex-replace-closure regex (make-closure
                                             :key 'between-n-and-m
                                             :greedy? (closure-greedy? closure)
                                             :n (closure-n closure)
                                             :m m))))))

;; }}}
;; regex-between-n-and-m->exactory-n {{{

(defmethod regex-between-n-and-m->exactory-n ((regex regex))
  (let* ((closure (regex-closure regex))
         (greedy? (closure-greedy? closure)))
    (values (regex-replace-closure regex
                                   (make-closure
                                     :key 'exactory-n
                                     :greedy? greedy?
                                     :n (if greedy?
                                          (closure-m closure)
                                          (closure-n closure)))))))

;; }}}
;; regex-between-n-and-m->between-n-and-m-1 {{{

(defmethod regex-between-n-and-m->between-n-and-m-1 ((regex regex))
  (let* ((closure (regex-closure regex))
         (n (closure-n closure))
         (m (closure-m closure))
         (greedy? (closure-greedy? closure)))
    (when (/= n m)
      (values (regex-replace-closure regex
                                     (make-closure
                                       :key 'between-n-and-m
                                       :greedy? greedy?
                                       :n (if greedy? n (1+ n))
                                       :m (if greedy? (1- m) m)))))))

;; }}}
;; match? {{{

(defun match? (pat line &key (start 0))
  (do* ((line-length (length line))
        (regex (with-string-ahead-reader (reader pat)
                 (parse-regex reader)))
        (start start (1+ start))
        (match? (anchored-match? regex line start)
                (anchored-match? regex line start)))
    ((or (> start line-length) match?)
     (aand match? (list start it)))))

;; }}}
;; anchored-match? {{{

(defun anchored-match? (pat line i)
  (if (null pat)
    (return-from anchored-match? i)    ; matched
    (let ((line-length (length line))
          (first-regex (first pat))
          (rest-regex (rest pat)))
      (when (or (< i line-length)
                (regex-eq? first-regex 'end)
                (regex-enable-zero-match? first-regex))
        (cond
          ;; closure `{n,}' case
          ((regex-eq? first-regex 'more-n)
           (aand (regex-more-n->between-n-and-m first-regex (- line-length i))
                 (anchored-match? (cons it rest-regex) line i)))
          ;; closure `{n,m}' case
          ((regex-eq? first-regex 'between-n-and-m)
           (or (anchored-match? (cons (regex-between-n-and-m->exactory-n first-regex) rest-regex) line i)
               (aand (regex-between-n-and-m->between-n-and-m-1 first-regex)
                     (anchored-match? (cons it rest-regex) line i))))
          ;; closure `{n}' case
          ((regex-eq? first-regex 'exactory-n)
           (aand (match-exactory-n? first-regex line i)
                 (anchored-match? rest-regex line it)))
          ;; grouping `()' case
          ((regex-eq? first-regex 'grouping)
           (aand first-regex
                 (regex-pat it)
                 (or (anchored-match? (append (first it) rest-regex) line i)
                     (anchored-match? (cons (make-regex :key 'grouping
                                                        :pat (rest it)
                                                        :closure (regex-closure first-regex))
                                            rest-regex)
                                      line i))))
          ;; selection `[]' case
          ((regex-eq? first-regex 'selection)
           (when (if (char= (first (regex-pat first-regex)) #\^)
                   (not (find (char line i) (rest (regex-pat first-regex))))
                   (find (char line i) (regex-pat first-regex)))
             (anchored-match? (rest pat) line (1+ i))))
          ;; character case
          ((and (regex-eq? first-regex 'character)
                (char= (regex-pat first-regex) (char line i))
                (anchored-match? rest-regex line (1+ i))))
          ;; starting position `^'
          ((and (regex-eq? first-regex 'start)
                (= i 0)
                (anchored-match? rest-regex line i)))
          ;; ending position `$'
          ((and (regex-eq? first-regex 'end)
                (= i line-length)
                (null rest-regex)
                (anchored-match? nil line i)))
          ;; wild card `.' case
          ((and (regex-eq? first-regex 'dot)
                (anchored-match? rest-regex line (1+ i))))
          ;; mismatch case
          (t
            nil))))))

;; }}}
;; match-exactory-n? {{{

(defun match-exactory-n? (regex line i)
  (do* ((n (closure-n (regex-closure regex)))
        (regex-no-closure (list (regex-replace-closure regex (make-closure))))
        (count 0 (1+ count))
        (match? i (anchored-match? regex-no-closure line match?)))
    ((or (not match?) (= n count)) match?)))

;; }}}
;; char->key {{{

(defun char->key (c)
  (case c
    (#\. 'dot)
    (#\^ 'start)
    (#\$ 'end)
    (t 'character)))

;; }}}
;; char-pat {{{

(defun char->pat (c)
  (and (char/= c #\. #\^ #\$) c))

;; }}}
;; read-closure {{{

(defmethod read-closure ((reader ahead-reader))
  (let* ((next (get-next reader))
         (closure (case next
                    (#\* (make-closure :key 'more-n :n 0))
                    (#\+ (make-closure :key 'more-n :n 1))
                    (#\? (make-closure :key 'between-n-and-m :n 0 :m 1))
                    (#\{ (let* ((inner-paren (get-buf (read-paren reader))))
                           (cond ((not (position #\, inner-paren))
                                  (make-closure :key 'exactory-n :n (parse-int inner-paren) :m -1))
                                 ((empty? (after #\, inner-paren))
                                  (make-closure :key 'more-n :n (parse-int (before #\, inner-paren)) :m -1))
                                 (t
                                   (make-closure :key 'between-n-and-m
                                                 :n (parse-int (before #\, inner-paren))
                                                 :m (parse-int (after #\, inner-paren))))))))))
    (when (char/= next #\{)
      (read-next reader :cache nil))
    (when (char= (get-next reader) #\?)
      (read-next reader :cache nil)
      (setf (closure-greedy? closure) nil))
    (values closure)))

;; }}}
;; parse-regex {{{

(defun parse-regex (reader)
  (do ((regex nil)
       (key nil)
       (pat nil)
       (closure nil)
       (c (get-next reader) (get-next reader)))
    ((or (reader-next-in? reader #\| #\))
         (reach-eof? reader))
     (nreverse regex))
    (cond ((char= c #\[)
           (setq key 'selection
                 pat (coerce (expand-hyphen (get-buf (read-paren reader))) 'list)))
          ((char= c #\()
           (setq key 'grouping
                 pat (do ((inner-regex nil))
                       ((and (char= (get-next reader) #\)) (read-next reader :cache nil))
                        (nreverse inner-regex))
                       (push (parse-regex (read-next reader :cache nil))
                             inner-regex))))
          (t
            (let ((c (coerce (get-buf (read-next reader)) 'character)))
              (setq key (char->key c) pat (char->pat c)))))
    (if (find (get-next reader) '(#\* #\? #\+ #\{))
      (setq closure (read-closure reader))
      (setq closure (make-closure)))
    (push (make-regex :key key :pat pat :closure closure) regex)))

;; }}}

