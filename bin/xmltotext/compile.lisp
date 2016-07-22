
(load "xmltotext")

(defexe xmltotext (--help)
  "help"
  (let ((usage (usage :title "xmltotext [OPTION]"
                      :desc  '("Convert xml to text."))))
    (when (or --help errors)
      (funcall usage))
    (princ (main *standard-input*))))

