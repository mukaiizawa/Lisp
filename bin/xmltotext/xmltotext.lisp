
(load "../../lib/stdlib")
(load "../../lib/ahead-reader")
(load "../../lib/regex")

(defmethod read-html-escape-sequance ((reader ahead-reader))
  (let ((esc (get-buf (read-next
                        (read-if (lambda (c)
                                   (not (find c '(#\; #\Space))))
                                 (read-next reader))))))
    (cond ((string-equal esc "&quot;") #\')
          ((string-equal esc "&amp;") #\&)
          ((string-equal esc "&lt;") #\<)
          ((string-equal esc "&gt;") #\>)
          ((string-equal esc "&nbsp;") #\Space)
          ((string-equal esc "&copy;") #\C)
          (t esc))))

(defun main (stream)
  (with-output-to-string (out)
    (with-ahead-reader (reader stream)
      (while (not (reach-eof? reader))
        (cond ((reader-next-in? reader #\<)
               (read-paren reader :cache nil))
              ((reader-next-in? reader #\&)
               (write-string (read-html-escape-sequance reader)) out)
              (t
                (write-char (get-curr (read-next reader)) out))))
      (values out))))

