(load "../../../lib/stdlib")

(defun mkcols (cols)
  (append (mapcar (lambda (x)
                    (mkstr x ","))
                  (butlast cols))
          (last1 cols)))

(defexe tab-to-ctl ()
  (with-encoding (:cp932 :windows)
    (mapfile (lambda (pathname)
               (let ((table-name (pathname-name pathname)))
                 (write-to!  (flatten (list
                                        "LOAD DATA"
                                        (mkstr "INFILE '" table-name ".csv'")
                                        (mkstr "INTO TABLE " table-name)
                                        "APPEND"
                                        "FIELDS TERMINATED BY ','"
                                        "TRAILING NULLCOLS"
                                        "("
                                        (mkcols (read-from pathname))
                                        ")"))
                             (mkstr table-name ".ctl"))))
             :extension 'tab))
  (echo "finished."))
