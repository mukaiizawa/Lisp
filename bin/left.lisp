
(load "../lib/stdlib")

(defun get-line (line columns)
  (if (> columns (length line))
    line
    (subseq line 0 columns)))

(defexe left ((-c --columns) --help)
  "c:columns:help;"
  (let* ((usage (usage :title "left [OPTION]... [FILE]"
                       :desc '("Print the left 10 columns of each FILE to standard output."
                               "With no FILE, read standard input.")
                       :opts '("-c, --columns=[N]" "print the first N columns instead of the first 10")))
         (columns (parse-int (or -c --columns 10)))
         (file (first args)))
    (cond ((or --help errors)
           (funcall usage))
          ((and file
                (not (file-exists? file)))
           (format *error-output* "left: cannot open `~A' for reading~%" file))
          ((null file)
           (do ((line (read-line *standard-input*)
                      (read-line *standard-input* nil 'EOF)))
             ((eq line 'EOF))
             (princln (get-line line columns))))
          (t
            (mapcan (lambda (line)
                      (princln (get-line line columns)))
                    (read-from file))))))

