(load "../../lib/stdlib")


(defexe nsort ((-e --extension) -r --help)
  "e:extension:r;help"
  (let ((extension (parse-sym (or -e --extension 'csv)))
        (usage (usage :title "sort [OPTION]..."
                      :desc  "File sort utility."
                      :opts '("-e, --extension=" "target file extension(default `csv')"
                              "-r" "recursive subdirectory or not"))))
    (when (or --help errors)
      (funcall usage))
    (mapfile (lambda (pathname)
               (let ((buf (read-from pathname)))
                 (info 'start pathname)
                 (info "sorting " pathname)
                 (write-to! (sort buf #'string<)
                            pathname
                            :ff :windows
                            :enc :cp932))
               (info 'end pathname))
             :extension extension
             :recursive -r))
  (echo "finished."))
