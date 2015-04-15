(defun replace-string (from to tar-str)
  (labels ((rec (str acc)
                (if (string= "" str)
                  acc
                  (if (string= from (subseq str 0 1))
                    (rec (subseq str 1) (format nil "~A~A" acc to))
                    (rec (subseq str 1) (format nil "~A~A" acc (subseq str 0 1)))))))
    (rec tar-str "")))


