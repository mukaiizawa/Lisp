(load "../lib/stdlib")

(defstruct sfile ()
  (fname nil)
  (stream nil)
  (curr-line nil))

(defun create-sfiles (lis)
  (if lis
    (cons (let* ((fname (car lis))
                 (stream (open fname :direction :input :if-does-not-exist :error))
                 (sfile (make-sfile :fname fname :stream stream :curr-line nil)))
            (set-next-line sfile)
            (values sfile))
          (create-sfiles (cdr lis)))
    nil))

(defun get-candidate-sfile (sfiles &key (reverse? nil))
  (let ((sorter (if reverse? #'string> #'string<)))
    (reduce (lambda (sfile1 sfile2)
              (if (funcall sorter (sfile-curr-line sfile1) (sfile-curr-line sfile2))
                sfile1
                sfile2))
            sfiles)))

(defmethod reach-eof? ((sfile sfile))
  (eq (sfile-curr-line sfile) 'EOF))

(defmethod set-next-line ((sfile sfile))
  (setf (sfile-curr-line sfile) (read-line (sfile-stream sfile) nil 'EOF))
  (when (reach-eof? sfile)
    (close (sfile-stream sfile))))

(defexe external-sort ((-r --reverse)--help)
  "r;reverse;help;"
  (let* ((usage (usage :title "outer-sort [OPTION] [FILE]..."
                       :desc  '("Merge two or more FILE together."
                                "Each FILE should be sorted.")
                       :opts  '("-r, --reverse" "reverse the result of comparisons")))
         (reverse? (or -r --reverse))
         (sfiles (create-sfiles args)))
    (when (or --help
              errors)
      (funcall usage))
    (unwind-protect
      (while (setq sfiles (delete-if #'reach-eof? sfiles))
        (let* ((sfile (get-candidate-sfile sfiles :reverse? reverse?))
               (line (sfile-curr-line sfile)))
          (echo line)
          (set-next-line sfile)))
      (mapcan (lambda (sfile)
                (when (open-stream? (sfile-stream sfile))
                  (close (sfile-stream sfile))))
              sfiles))))
