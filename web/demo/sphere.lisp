
; (load "../../lib/stdlib")
; (load "../to-html")

(:!DOCTYPE "html")
(:html ((lang "ja"))
  (:head
    (:script ((src "sphere.js")))
    (:meta ((charset "utf-8")))
    (:title "Page Title"))
  (:body
    (:form ((id "theForm"))
      (:div
        (:label ((for "radius")) "radius")
        (:input ((type "text")
                 (name "radius")
                 (id "radius")
                 (value "0.0")
                 (required))))
      (:div
        (:label ((for "volume")) "volume")
        (:input ((type "text")
                 (name "volume")
                 (id "volume")
                 (value "0.00"))))
      (:div
        (:input ((type "submit")
                 (value "calculate")
                 (id "submmit")))))))
