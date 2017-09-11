(require :stdlib *module-stdlib*)

;; required function 'usage'
(defmacro with-dbinfo (args &body body)
  `(destructuring-bind
     (schema pass sid)
     (mapcar (lambda (str)
               (string-trim " " str))
             (list (mkstr-aif (before #\/ ,args)
                     (string-upcase it))
                   (after #\/ (before #\@ ,args))
                   (after #\@ ,args)))
     (cond ((or --help
                errors
                (string= +empty-string+ schema)
                (null pass)
                (null sid))
            (funcall usage))
           (t ,@body))))

;; required in a with-dbinfo context.
(defmacro execute-query (query &key (spool nil))
  `(let1 (arg-sqlfile (mkstr (random (expt 2 50) (make-random-state t)) ".cache"))    ; unique file name.
     (with-encoding (:cp932 :windows)
       (write-to!
         (flatten
           (list "SET ECHO OFF"
                 "SET HEAD OFF"
                 "SET LINESIZE 32767"
                 "SET PAGESIZE 0" 
                 "SET TRIMSPOOL ON"
                 "SET FEEDBACK OFF"
                 "SET COLSEP ','"
                 "SET SQLBLANKLINES ON"
                 (mkstr-aif ,spool "SPOOL " it)
                 (aif ,query it "")
                 (mkstr-if ,spool "SPOOL OFF")
                 "EXIT"))
         arg-sqlfile))
     (call-sqlplus schema pass sid arg-sqlfile)
     (when (file-exists? arg-sqlfile)
       (delete-file arg-sqlfile))))

(defun call-sqlplus (schema pass sid argfile)
  (call "sqlplus" (mkstr "-L -S " schema "/" pass "@" sid " @" argfile) *standard-output*))
