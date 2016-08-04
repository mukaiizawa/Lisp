
(require "xml-manager" *module-xml-manager*)

(with-html-output ()
  (:!DOCTYPE "html")
  (:html ((lang "ja"))
    (:head
      (:meta ((charset "utf-8")))
      (:title "Page Title"))
    (:body
      (:table ((border 0) (cellpadding 10))
        (dotimes (i 5)
          (:tr ((align "right"))
            (dotimes (j 5)
              (:td ((bgcolor (if (oddp (1+ j)) "#FF0000" "#FF00FF")))
                (format nil "~@R" (+ (* 5 i) (1+ j)))))))))))

