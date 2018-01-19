#|
 | pict wrapper
 |#

(require :stdlib *module-stdlib*)
(provide :pict)

(defun parse-condition (tokens)
  (with-output-to-string (out)
    (let ((ope (car tokens)))
      (cond ((or (eq ope 'and) (eq ope 'or))
             (if (< (length tokens) 3) (error "illegal ~A clause" ope))
             (format out " (")
             (reduce (lambda (x y)
                       (format out " ~A ~A ~A"
                               (parse-condition x) ope (parse-condition y)))
                     (rest tokens))
             (format out ")"))
            ((eq ope 'in)
             (if (< (length tokens) 3) (error "illegal in clause"))
             (format out " [~A] in {~{\"~A\"~^, ~}}"
                     (second tokens) (cddr tokens)))
            ((or (eq ope '=) (eq ope '/=))
             (if (/= (length tokens) 3) (error "illegal ~A clause" ope))
             (format out " [~A] ~A \"~A\"" (second tokens) ope (third tokens)))
            ((or (eq ope '<) (eq ope '<=) (eq ope '>) (eq ope '>=))
             (if (/= (length tokens) 3) (error "illegal ~A clause" ope))
             (format out " [~A] ~A ~A" (second tokens) ope (third tokens)))
            (t (error "illegal operator ~A" ope))))
    out))

(defun parse-clause (tokens)
  (with-output-to-string (out)
    (if (/= (length tokens) 3) (error "illegal clause."))
    (if (not (eq (first tokens) 'if)) (error "if is exprected."))
    (format out "IF ~A THEN ~A;"
            (parse-condition (second tokens))
            (parse-condition (third tokens)))
    out))

; (pict <factor-list> [<submodel-list>] [<condition-list>])
; <factor-list> ::= (factor-name factor [factor] ...)
; <submodel-list> ::= (submodel [submodel] ...)
; <submodel> ::= (factor-name factor-name [factor-name] ...)
; <constraint-list> ::= (<constraint> [<constraint>] ...)
; <constraint> ::= <clause> [<clasuse>] ...
; <clause> ::= (if <condition> <condition>)
; <condition> ::= ({ and | or } <condition> [<condition>] ...)
;                 | (in factor [factor] ...)
;                 | ({ = | /= | < | <= | > | >= } factor factor)
(defun pict (factor-list &optional model-list condition-list
                         &key (output "pict.out") invoke?)
  (with-open-file (pict-in output :direction :output :if-exists :supersede)
    (format pict-in "# following rules are generated automatically.~%")
    (dolist (factor factor-list)
      (format pict-in "~A: ~{~A~^, ~}~%" (car factor) (cdr factor)))
    (dolist (model model-list)
      (format pict-in "{ ~{~A~^, ~} } @ ~A~%" model (length model)))
    (dolist (condition condition-list)
      (format pict-in "~A~%" (parse-clause condition))))
  (if invoke? (call "pict" (list output))))
