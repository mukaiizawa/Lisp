
(load "stdlib" :if-does-not-exist nil)

(defmacro defcollection ((collection-name key super-name) &body slots)
  (with-gensyms (g1 g2)
    (let ((collection-type g1)
          (type-satisfier g2))
      `(progn
         (defun ,type-satisfier (lis)
           (and (listp lis)
                (not (find-if (lambda (obj)
                                (not (typep obj ',super-name)))
                              lis))))
         (deftype ,collection-type ()
           '(satisfies ,type-satisfier))
         (defstruct ,(mksym collection-name)
           (,(mksym key) nil :type ,collection-type)
           ,@slots)))))

