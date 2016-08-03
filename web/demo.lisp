
(load "../lib/stdlib")
(load "to-html")

; (setq *html-indent* nil)

(with-html-output ()
  (:!DOCTYPE "html")
  (:html ((lang "ja"))
    (:head
      (:meta ((charset "utf-8")))
      (:title "Page Title"))
    (:body
      (:table ((border 0) (cellpadding 40))
        (dotimes (i 5)
        (:tr ((align "right"))
          (dotimes (j 5)
            (:td ((bgcolor (if (oddp (1+ j)) "pink" "green")))
              (format nil "~@R" (* (1+ i) (1+ j )))))))))))
