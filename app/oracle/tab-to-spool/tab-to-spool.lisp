(load "../../../lib/stdlib")
(load "../ora-lib")

(defvar *batch-file* "all.bat")

(defun mkcols (cols escapep)
  (let ((cols (if escapep
                (mapcar (lambda (x)
                          ; (mkstr "REPLACE(REPLACE(" x ", ',', '###'), CHR(13)||CHR(10), '\\n')"))
                          (mkstr "REPLACE(" x ",CHR(10), '\\n')"))
                        cols)
                cols)))
    (append (mapcar (lambda (x)
                      (mkstr x " ||',' ||"))
                    (butlast cols))
            (last1 cols))))

(defexe tab-to-spool (-e --help)
  "e;help"
  (with-encoding (:cp932 :windows)
    (let ((args (list->string args #\Space))
          (usage (usage :title "tab-to-spool [OPTION] [SCHEMA]/[PASSWORD]@[SID]"
                        :desc  "Convert tab file to sql. (for sqlplus)"
                        :opts  '("-e" "escape comma and CRLF."))))
      (with-dbinfo args
                   (mkfile! *batch-file*)
                   (mapfile
                     (lambda (pathname)
                       (info 'start pathname)
                       (let ((fname (pathname-name pathname))
                             (cols (read-from pathname)))
                         (write-to (mkstr "call sqlplus " schema "/" pass "@" sid " @" fname ".sql")
                                   *batch-file*
                                   :enc :cp932
                                   :ff :windows)
                         (write-to!  (flatten (list "SET ECHO OFF"
                                                    "SET HEAD OFF"
                                                    "SET LINESIZE 32767"
                                                    "SET PAGESIZE 0"
                                                    "SET TRIMSPOOL ON"
                                                    "SET FEEDBACK OFF"
                                                    "SET COLSEP ','"
                                                    (mkstr "SPOOL " fname ".csv")
                                                    "SELECT"
                                                    (mkcols cols -e)
                                                    (mkstr "FROM " schema "." fname ";")
                                                    "SPOOL OFF"
                                                    "EXIT"))
                                     (mkstr fname ".sql")
                                     :enc :cp932
                                     :ff :windows)
                         (info 'end)))
                     :extension 'tab)))))
