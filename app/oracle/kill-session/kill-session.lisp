
(load "../../../lib/stdlib")
(load "../ora-lib")


(defexe kill-session (-s --help)
  "s:help"
  (let ((args (list->string args #\Space))
        (tmp-file "inactive-session.csv")
        (usage (usage :title "kill-session [OPTION] [SCHEMA]/[PASSWORD]@[SID]"
                      :desc  '("Kill inactive session."
                               "Required root authority")
                      :opts  '("-s" "Target schema."))))
    (with-dbinfo args
                 (execute-query (list
                                  " SELECT DISTINCT "
                                  "   S.SID || ', ' || "
                                  "   S.SERIAL# "
                                  " FROM "
                                  "   V$SESSION S "
                                  " WHERE "
                                  "   S.TYPE <> 'BACKGROUND' AND "
                                  (mkstr-aif -s
                                    (format nil "~:@((~{ S.SCHEMANAME = '~A' ~^ OR ~}) AND~)"
                                            (string->list #\, it)))
                                  "   S.STATUS = 'INACTIVE';")
                                :spool tmp-file)
                 (execute-query (mapcar (lambda (sid-serial)
                                          (mkstr "ALTER SYSTEM KILL SESSION '" sid-serial "';"))
                                        (prog1 (read-from tmp-file)
                                          (delete-file tmp-file)))))
    (echo "finished")))

