(load "../../lib/stdlib")


(defexe rm-line (--help)
  "help"
  (let ((file (car args))
        (files (cdr args))
        (usage (usage :title "rm-line [FILE] [FILE1] [FILE2]..."
                      :desc  "remove the line included in FILE1, FILE2... from the FILE.")))
    (when (or --help
              (null files))
      (funcall usage))
    (labels ((rec (candidate files)
                  (if files
                    (rec (progn (dolist (line (read-from (car files)))
                                  (setq candidate (delete line candidate :test #'string-equal)))
                                candidate)
                         (cdr files))
                    candidate)))
      (mapcar #'echo (rec (read-from file) files)))))
