(require :stdlib *module-stdlib*)

(defmacro dlambda (&rest body)
  (with-gensyms (args)
    `(lambda (&rest ,args)
       (case (car ,args)
         ,@(mapcar
             (lambda (d)
               `(,(if (eq t (car d))
                    t
                    (list (car d)))
                  (apply (lambda ,@(cdr d))
                         ,(if (eq t (car d))
                            args
                            `(cdr ,args)))))
             body)))))

;; Examples:
(setf (symbol-function 'count-test)
      (let ((count 0))
        (dlambda
          (:inc () (incf count))
          (:dec () (decf count))
          (:reset (&optional a) (setq count (aif a it 0))))))
(count-test :inc)
(count-test :dec)
(count-test :dec)
(count-test :reset 20)
(count-test :dec)
(count-test :dec)

(defmacro olambda (letargs &body body)
  (let ((letargs (canonical-letargs letargs)))
    (flet ((olambda-get ()
                        `(case x
                           ,@(mapcar (lambda (x)
                                       `((,(car x)) ,(car x)))
                                     letargs)
                           (t (error "olambda-get: No such a symbol `~A'" x))))
           (olambda-set ()
                        `(case x
                           ,@(mapcar (lambda (x)
                                       `((,(car x)) (setq ,(car x) val)))
                                     letargs)
                           (t (error "olambda-set: No such a symbol `~A'" x)))))
      `(let* ,letargs
         (dlambda
           (:get (x) ,(olambda-get))
           (:set (x val) ,(olambda-set))
           ,@body)))))

;; Examples
(setf (symbol-function 'count-test)
      (olambda ((count 0))
        (:inc () (incf count))
        (:dec () (decf count))
        (:reset (&optional a) (setq count (aif a it 0)))))
(count-test :get 'count)
(count-test :set 'count 100)
(count-test :inc)
(count-test :dec)
(count-test :reset)
