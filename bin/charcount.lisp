
(load "../lib/stdlib")

;; count size of data from stdin

(defexe charcount ()
  (let ((count 0))
    (awhile (read-char *standard-input* nil nil)
      (incf count))
    (print count)))

