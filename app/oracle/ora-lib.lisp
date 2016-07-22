
;; required function 'usage'
(defmacro with-dbinfo (args &body body)
  `(destructuring-bind
     (schema pass sid)
     (mapcar (lambda (str)
               (trimstr str " "))
             (list (string-upcase (before #\/ ,args)) (after #\/ (before #\@ ,args)) (after #\@ ,args)))
     (cond ((or --help
                errors
                (string= "NIL" schema)
                (null pass)
                (null sid))
            (funcall usage))
           (t ,@body))))

;; required in a with-dbinfo context.
(defmacro execute-query (query &key (spool nil))
  `(let ((arg-sqlfile (mkstr (random (expt 2 50) (make-random-state t)) ".cache")))    ; unique file name.
     (write-to!
       (flatten
         (list "SET ECHO OFF"
               "SET HEAD OFF"
               "SET LINESIZE 32767"
               "SET PAGESIZE 0" 
               "SET TRIMSPOOL ON"
               "SET FEEDBACK OFF"
               "SET COLSEP ','"
               (awhen ,spool
                 (mkstr "SPOOL " it))
               ,query
               (awhen ,spool
                 "SPOOL OFF")
               "EXIT"))
       arg-sqlfile
       :enc :cp932
       :ff :windows)
     (call-sqlplus schema pass sid arg-sqlfile)
     (when (file-exists? arg-sqlfile)
       (delete-file arg-sqlfile))))

(defun call-sqlplus (schema pass sid argfile)
  (call "sqlplus" (mkstr "-L -S " schema "/" pass "@" sid " @" argfile) *standard-output*))

