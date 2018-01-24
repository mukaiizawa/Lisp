; pict wrapper

(provide :pict)

; original constraint grammer
; <constraints> ::= <constraint> [<constraint>]
; <constraint> ::= <predicate>
;                    | IF <predicate> THEN <predicate> [ELSE <predicate>];
; <predicate> ::= { ( <predicate> )
;                    | NOT <predicate>
;                    | <param_name> { = | <> | > | >= | < | <= }
;                                   { <term_param>> | <value>}
;                    | <param_name> IN '{' <value> [, <value>] ... '}'
;                    | <param_name> LIKE <regex> }
;                 [ { AND | OR } <predicate> ]
; <param_name> ::= '[' <identifier> ']'
; <value> ::= { <param_name> | " <identifier> " | <number> }
; <identifier> -- whatever is typically regarded as a identifier
; <number> -- whatever is typically regarded as a number
; <regex> -- string with embedded special characters
;              *: a series of characters of any length (can be zero)
;              ?: any one character

; pict DSL grammer
; (pict <params> &key <submodels> <constraints>)
; <params> ::= (<param> [<param>] ...)
; <param> ::= (<symbol> <param_value> [<param_value>] ...)
; <param_value> ::= { <alias> | <value> }
; <alias> ::= ( <value> [<value>] ...)
; <submodels> ::= (<submodel> [<submodel>] ...)
; <submodel> ::= (<number> <symbol> [<symbol>] ...)
; <constraints> ::= (<constraint> [<constraint>] ...)
; <constraint> ::= { <predicate> | (if <predicate> <predicate> [<predicate>]) }
; <predicate> ::= {
;   (not <predicate>)
;   | (in <symbol> <value>>)
;   | (like <symbol> <regex>)
;   | ({ and | or } <predicate> [<predicate>] ...)
;   | ({ = | /= | < | <= | > | >= } <symbol> <value>)
; }
; <value> ::= { <symbol> | <string> | <number> }
; <symbol> -- a symbol
; <string> -- a string
; <number> -- a number

(defun map-op (op)
  (case op
    ((< <= > >= =) op)
    ((/=) '<>)))

(defun pict (params &key submodels constraints (file "pict.arg"))
  (with-open-file (out file :direction :output :if-exists :supersede)
    (let (param-names)
      (labels
        ((parse-params ()
           (dolist (param params)
             (let ((param-name (car param)) (param-values (cdr param)))
               (if (not (atom param-name))
                 (error "parameter name must be symbol `~A'." param-name))
               (push param-name param-names)
               (format out "~A: ~{~A~^ ,~}~%"
                       param-name
                       (mapcar (lambda (v)
                                 (if (atom v) v (format nil "~{~A~^ | ~}" v)))
                               param-values)))))
         (parse-submodels ()
           (dolist (submodel submodels)
             (let ((order (car submodel)) (submodel-members (cdr submodel)))
               (if (not (numberp order))
                 (error "submodel order `~A' must be number." order))
               (dolist (param-name submodel-members)
                 (if (not (find param-name param-names))
                   (error "submodel member `~A' must be in ~A."
                          param-name param-names)))
               (format out "{ ~{~A~^, ~} } @ ~A~%" submodel-members order))))
         (parse-conditions ()
           (dolist (constraint constraints)
             (if (not (eq (car constraint) 'if))
               (format out "~A" (parse-predicate constraint))
               (let ((predicate-num (length (cdr constraint))))
                 (if (not(or (= predicate-num 2) (= predicate-num 3)))
                   (error "illegal arguments ~A." constraint))
                 (format out "IF ~A THEN ~A"
                         (parse-predicate (cadr constraint))
                         (parse-predicate (caddr constraint)))
                 (if (= predicate-num 3)
                   (format out " ELSE ~A"
                           (parse-predicate (cadddr constraint))))))
             (format out ";~%")))
         (parse-predicate (predicate)
           (with-output-to-string (s)
             (let* ((op (car predicate))
                    (args (cdr predicate))
                    (argc (length args)))
               (if (not (symbolp op)) (error "operator `~A' must be symbol" op))
               (case op
                 ((not)
                  (if (/= argc 1) (error "illegal arguments ~A" args))
                  (format s "not ( ~A )" (parse-predicate (car args))))
                 ((in)
                  (if (not (find (car args) param-names))
                    (error "undefined parameter name `~A'." (car args)))
                  (format s "[~A] in {~{~A~^, ~}}"
                          (car args)
                          (mapcar #'parse-predicate-value (cdr args))))
                 ((like) (error "like not supported."))
                 ((and or)
                  (if (< argc 2) (error "illegal ~A arguments" op))
                  (reduce (lambda (x y)
                            (format s "( ~A ~A ~A )"
                                    (parse-predicate x) op
                                    (parse-predicate y)))
                          args))
                 ((= /= < <= > >=)
                  (if (/= argc 2) (error "illegal arguments ~A" args))
                  (if (not (find (car args) param-names))
                    (error "illegal parameter name `~A'." (car args)))
                  (format s "[~A] ~A ~A"
                          (car args)
                          (map-op op)
                          (parse-predicate-value (cadr args))))))
             s))
         (parse-predicate-value (val)
           (cond ((numberp val) val)
                 ((stringp val) (format nil "\"~A\"" val))
                 ((symbolp val)
                  (if (find val param-names)
                    (format nil "[~A]" val)
                    (format nil "\"~A\"" val)))
                 (t (error "illegal predicate value `~A'." val)))))
        (format out "# following rules are generated automatically.~%")
        (parse-params)
        (parse-submodels)
        (parse-conditions)))))
