(load "../lib/stdlib")

(defexe head ((-c --bytes) (-n --lines) (-q --quiet) (-v --verbose))
  "c:bytes:n:lines:q;quiet;v;verbose"
  (let ((usage (usage :title "head [OPTION]... [FILE]"
                      :desc '("Print the first 10 lines of each FILE to standard output."
                              "With more than one FILE, precede each with a header giving the file name."
                              "With no FILE, read standard input.")
                      :opts '("-c, --bytes=[-]N" "print the first N bytes of each file"
                              "-n, --lines=[-]N" "print the first N lines instead of the first 10"
                              "-q, --quiet" "never print headers giving file names"
                              "-v, --verbose" "always print headers giving file names")
                      :foot "N may have a multiplier suffix: b 512, k 1024, m 1024*1024.")))
    (if args
      (dolist (i args)
        (with-open-file (in i :direction :input :external-format (guess-encoding i))
          (each-line (echo line-num ":" line))))
      (awhile (read-line *standard-input* nil nil)
        (echo it)))))
