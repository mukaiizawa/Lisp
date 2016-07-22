
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
      (:!-- "this is a comment")
      "content start"
      (:br)
      (:dir)
      (:h1 pi "is" 3.14)
      (:input ((id "hiddenBox")
               (type "hidden")
               (value "hidden")
               (required))))))


