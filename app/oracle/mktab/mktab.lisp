(load "../../../lib/stdlib")
(load "../ora-lib")

(defexe mktab (-n -o (-t --table) --help)
  "n;o;t:table:help"
  (with-encoding (:cp932 :windows)
    (let ((args (list->string args #\Space))
          (usage (usage :title "mktab [OPTION} [SCHEMA]/[PASSWORD]@[SID]"
                        :desc  "Create tab files."
                        :opts  '("-o" "only output tabs file"
                                 "-t, --table=[]" "output only table"
                                 "-n" "append num of records to a tabs file"))))
      (with-dbinfo args
                   (let ((tabs (mkstr schema ".tabs")))
                     (execute-query
                       (mkstr "SELECT TABLE_NAME "
                              "FROM TABS "
                              (awhen (or -t --table)
                                (mkstr "WHERE TABLE_NAME = '"
                                       (string-upcase it))) "';")
                       :spool tabs)
                     (unless -o
                       (mkdir schema)
                       (mapcan (lambda (table)
                                 (execute-query
                                   (mkstr "SELECT COLUMN_NAME "
                                          "FROM ALL_TAB_COLUMNS "
                                          "WHERE OWNER = '" schema "' AND "
                                          "TABLE_NAME = '" table "' "
                                          "ORDER BY COLUMN_ID;")
                                   :spool (mkstr schema "/" table ".tab")))
                               (read-from tabs)))
                     (when -n
                       (execute-query 
                         (mapcar (lambda (table)
                                   (mkstr "SELECT '" table "'||','|| COUNT(*) "
                                          "FROM " table ";"))
                                 (prog1
                                   (read-from tabs)
                                   (delete-file tabs)))
                         :spool tabs)))
                   (echo "finished.")))))
