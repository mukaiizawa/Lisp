(defun mkstr (&rest args)
  (with-output-to-string (str)
    (dolist (i args)
      (princ i str))))
; (print (mkstr pi " pieces of " 'Pi))


(defun replace-string (from to tar-str)
  (labels ((rec (str acc)
                (let ((from-len (length from))
                      (str-len  (length str)))
                  (if (< str-len from-len)
                    (mkstr acc str)
                    (if (string= from (subseq str 0 from-len))
                      (rec (subseq str from-len)
                           (mkstr acc to))
                      (rec (subseq str from-len)
                           (mkstr acc (subseq str 0 from-len))))))))
    (rec tar-str "")))
; (print (replace-string "asd" "b" "asdf"))



