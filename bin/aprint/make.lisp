
(load "aprint")

(defexe aprint ((-s --surround-with) (-g --bg-char) --help)
  "s:surround-with:g:bg-char:help"
  (let ((usage (usage :title "aprint"
                      :desc  "Convert Text to ASCII Art"
                      :opts '("-s, --surround-with=" "surround with a character."
                              "-g, --bg-char=" "background character."))))
    (setq *surround-character* (or -s --surround-with)
          *bg-character* (or -g --bg-char))
    (when (or --help
              errors
              (> (length *surround-character*) 1))
      (funcall usage))
    (princ
      (if args
        (with-input-from-string (in (list->string args))
          (aprint in))
        (aprint *standard-input*)))))

