
(load "graphviz")

(dot (make-graph :nodes '(("start" (shape "Mdiamond"))
                          ("end" (shape "Msquare")))
                 :edges '(("start" "a0")
                          ("start" "b0")
                          ("a3" "end")
                          ("b3" "end")
                          ("a1" "b3")
                          ("b2" "a3")
                          ("b0" "a1"))
                 :subgraph (list (make-subgraph  
                                   :graph-conf '((label "process1") (style "filled") (color "coral"))
                                   :node-conf '((color "white"))
                                   :edges (make-continuous-edges '("a0" "a1" "a2" "a3")))
                                 (make-subgraph  
                                   :graph-conf '((label "process2") (style "filled") (color "aquamarine"))
                                   :node-conf '((color "black"))
                                   :edges (make-continuous-edges '("b0" "b1" "b2" "b3"))))
                 :rank '(("start" "a0" "a1" "a2" "a3")
                         ("b0" "b1" "b2" "b3" "end")))
     :file "sample-cluster")

