
(load "../../lib/stdlib")
(load "../../lib/ahead-reader")

;;
;; <expr>   ::= <term> | <expr> "+" <term> | <expr> "-" <term>
;; <term>   ::= <factor> | <term> "*" <factor> | <term> "/" <factor>
;; <factor> ::= <number> | "(" <expr> ")"
;;
;; <expr>   ::= <term> [ ('+'|'-') <term> ]*
;; <term>   ::= <factor> [ ('*'|'/') <factor> ]*
;; <factor> ::= <number> | '(' <expr> ')'
;;

(defparameter *tokens* nil)

;; to-token {{{

(defmethod to-token ((reader ahead-reader))
  (let (tokens)
    (while (not (reach-eof? reader))
      (let ((c (get-next reader)))
        (cond ((char= c #\Space)
               (read-if (lambda (x) (char= x #\Space)) reader :cache nil))
              ((digit-char-p c)
               (push (cons 'number
                           (parse-int (get-buf (read-number reader))))
                     tokens))
              (t
                (push (list (mksym (get-buf (read-next reader)))) tokens)))))
    (nreverse tokens)))

;; }}}
;; token-kind {{{

(defun token-kind (token)
  (car token))

;; }}}
;; token-val {{{

(defun token-val (token)
  (cdr token))

;; }}}
;; parse-expression {{{

(defun parse-expression ()
  (do* ((term (parse-term))
        (token (first *tokens*) (first *tokens*)))
    ((and (not (eq '+ (token-kind token)))
          (not (eq '- (token-kind token)))) term)
    (pop *tokens*)
    (setq term (funcall (symbol-function (token-kind token))
                        term 
                        (parse-term)))))

;; }}}
;; parse-term {{{

(defun parse-term ()
  (do* ((factor (parse-factor))
        (token (first *tokens*) (first *tokens*)))
    ((and (not (eq '* (token-kind token)))
          (not (eq '/ (token-kind token)))) factor)
    (pop *tokens*)
    (setq factor (funcall (symbol-function (token-kind token))
                          factor 
                          (parse-factor)))))

;; }}}
;; parse-factor {{{

(defun parse-factor ()
  (let ((curr-token (first *tokens*)))
    (case (token-kind curr-token)
      (|(|
          (pop *tokens*)
          (let ((val (parse-expression)))
            (if (eq (token-kind (pop *tokens*)) '|)|)
            val
            (error "parse-factor: `)' expected"))))
      (+
        (pop *tokens*)
        (parse-factor))
      (-
        (pop *tokens*)
        (- (parse-factor)))
      (number
        (pop *tokens*)
        (token-val curr-token))
      (t
        (error "unexpected token `~A'" (token-kind (first *tokens*)))))))

;; }}}

(defun main (stream)
  (with-ahead-reader (reader stream)
    (setq *tokens* (to-token reader)))
  (parse-expression))
