
(load "../../../lib/stdlib")
(load "../ora-lib")

(defparameter *header-sql*
#<<END
 SELECT 
   'SID ,SERIAL_NO ,USER_NAME  ,MACHINE_NAME ,SESSION_STATUS ,SCHEMA_NAME ,OS_USER ,CONNECTION_TYPE ,SESSION_TYPE ,LOGIN_TIME ,SESSION_LOCK_MODE ,PROCESS_LOCK_MODE ,SQL_TEXT'
 FROM 
   DUAL
END)

(defexe get-session (-a -i -s+ --help)
  "a;i;s:help"
  (let ((args (list->string args #\Space))
        (usage (usage :title "get-session [OPTION} [SCHEMA]/[PASSWORD]@[SID]"
                      :desc  "See session."
                      :opts  '("-s" "Target schema"
                               "-a" "Display include background session"
                               "-i" "Display only inactive session"))))
    (with-dbinfo args
                 (execute-query
                   (list
                     *header-sql*
                     #\;
                     " SELECT DISTINCT "
                     "   '\"' || S.SID || '\",' || "
                     "   '\"' || S.SERIAL# || '\",' || "
                     "   '\"' || S.USERNAME || '\",' || "
                     "   '\"' || S.MACHINE || '\",' || "
                     "   '\"' || S.STATUS || '\",' || "
                     "   '\"' || S.SCHEMANAME || '\",' || "
                     "   '\"' || S.OSUSER || '\",' || "
                     "   '\"' || S.PROGRAM || '\",' || "
                     "   '\"' || S.TYPE || '\",' || "
                     "   '\"' || S.LOGON_TIME || '\",' || "
                     "   '\"' || L.LMODE || '\",' || "
                     "   '\"' || L.REQUEST || '\",' || "
                     "   '\"' || Q.SQL_TEXT || '\"'"
                     " FROM "
                     "   V$SESSION S "
                     "   LEFT JOIN V$SQL Q "
                     "     ON Q.SQL_ID = S.PREV_SQL_ID "
                     "   LEFT JOIN V$LOCK L  "
                     "     ON S.SID = L.SID  "
                     " WHERE "
                     (mkstr-aif -s+
                       (format nil "~:@((~{ S.SCHEMANAME = '~A' ~^ OR ~}) AND~)"
                               (string->list #\, it)))
                     (mkstr-if (not -a)
                       "   S.TYPE <> 'BACKGROUND' AND ")
                     (mkstr-if -i
                       "   S.STATUS = 'INACTIVE' AND ")
                     "   1 = 1; ")))))

