
(load "compute")

(defexe compute (--help)
  "help"
  (let* ((usage (usage :title "compute [OPTION] [EXPR]"
                       :desc  '("Calculate numbers from statndard input or command line argument,"
                                "writing to standard output."))))
    (when (or --help errors)
      (funcall usage))
    (print
      (if args
        (with-input-from-string (in (list->string args))
          (main in))
        (main *standard-input*)))))

