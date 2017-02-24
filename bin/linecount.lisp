(load "../lib/stdlib")

;; count lines in standard input

(defexe linecount ()
  (let ((count 0))
    (awhile (read-line *standard-input* nil nil)
      (incf count))
    (print count)))
