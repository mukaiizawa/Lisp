
(require "stdlib" *module-stdlib*)
(provide "test-utils")

(defmacro test-all (&body tests)
  `(progn ,@(mapcar (lambda (test)
                      `(test ,test))
                    tests)
          (echo "finished.")))

(defmacro test (expr)
  (with-gensyms (fn result)
    (let ((title (first expr))
          (fn `(multiple-value-list ,(second expr)))
          (result (cons 'list (nthcdr 2 expr))))
      `(unless (equal ,fn ,result)
         (echo (make-string 80 :initial-element #\- ))
         (echo ,title)
         (echo (rest ',fn))
         (echo " => " (eval ',fn))
         (echo "    " (eval ',result))))))

