
(load "../../../lib/stdlib")
(load "../ora-lib")


(defexe spool (-l --help)
  "l:help"
  (with-encoding (:cp932 :windows)
    (let* ((usage (usage :title "spool [OPTION} [SCHEMA]/[PASSWORD]@[SID] @[FILE]"
                         :desc  "Execute sqlfile and spool result."
                         :opts  '("-l" "a path of spool file." )
                         :foot '("Example:" "  spool foo/bar@foobar @hoge.sql")))
           (arg-sql-file (after #\@ (list->string args) :from-end t))
           (args (before #\@ (list->string args) :from-end t)))
      (with-dbinfo args
                   (execute-query (list->string (read-from arg-sql-file))
                                  :spool (aif -l
                                           it
                                           (mkstr (pathname-name arg-sql-file) ".csv")))))
    (echo "finished.")))



