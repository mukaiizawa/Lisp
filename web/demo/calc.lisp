
; (load "../../lib/stdlib")
; (load "../to-html")

(:!DOCTYPE "html")
(:html ((lang "ja"))
  (:head
    (:script ((src "calc.js")))
    (:meta ((charset "utf-8")))
    (:title "Page Title"))
  (:body
    (:form ((id "theForm"))
      (:div
        (:label ((for "quantity")) "注文数")
        (:input ((type "number")
                 (name "quantity")
                 (id "quantity")
                 (value "1")
                 (min "1")
                 (required))))
      (:div
        (:label ((for "price")) "単価")
        (:input ((type "text")
                 (name "price")
                 (id "price")
                 (value "1.00")
                 (required))))
      (:div
        (:label ((for "tax")) "税率(%)")
        (:input ((type "text")
                 (name "tax")
                 (id "tax")
                 (value "0.0")
                 (required))))
      (:div
        (:label ((for "discount")) "値引き額")
        (:input ((type "text")
                 (name "discount")
                 (id "discount")
                 (value "0.00")
                 (required))))
      (:div
        (:label ((for "total")) "合計")
        (:input ((type "text")
                 (name "total")
                 (id "total")
                 (value "0.00")
                 (required))))
      (:div
        (:input ((type "submit")
                 (value "計算")
                 (id "submmit")))))))


