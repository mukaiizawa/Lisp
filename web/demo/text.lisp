
; (load "../../lib/stdlib")
; (load "../to-html")

(:!DOCTYPE "html")
(:html ((lang "ja"))
  (:head
    (:script ((src "text.js")))
    (:meta ((charset "utf-8")))
    (:title "Page Title"))
  (:body
    (:form ((id "theForm"))
      (:div
        (:label ((for "comments")) "comments")
        (:textarea ((type "text")
                    (name "comments")
                    (id "comments")
                    (maxlength "100")
                    (required))))
      (:div
        (:label ((for "count")) "count")
        (:input ((type "number")
                 (name "count")
                 (id "count"))))
      (:div
        (:label ((for "result")) "result")
        (:textarea ((type "text")
                    (name "result")
                    (id "result"))))
      (:div
        (:input ((type "submit")
                 (value "submit")
                 (id "submit")))))))
