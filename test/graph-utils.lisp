
(require "stdlib" *module-stdlib*)
(require "graph-utils" *module-graph-utils*)
(require "test-utils" *module-test-utils*)

(test-all 
  ('make-continuous-edges-01
   (make-continuous-edges '("a0" "a1" "a2"))
   '(("a0" "a1" nil) ("a1" "a2" nil)))
  ('make-continuous-edges-02
   (make-continuous-edges '("a0" "a1" "a2") '((color "blue")))
   '(("a0" "a1" ((color "blue"))) ("a1" "a2" ((color "blue")))))
  ('make-continuous-edges-03
   (make-continuous-edges nil)
   nil)
  )

(dot
  (make-graph :nodes '(("start" ((shape "Mdiamond") (color "green")))
                       ("end" ((shape "Msquare") (color "yellow"))))
              :edges '(("start" "a0")
                       ("start" "b0")
                       ("a3" "end")
                       ("b3" "end")
                       ("a1" "b3")
                       ("b2" "a3")
                       ("b0" "a1"))
              :subgraphs (list (make-subgraph  
                                 :graph-conf '((label "process1") (style "filled") (color "coral"))
                                 :node-conf '((color "white"))
                                 :edges (make-continuous-edges '("a0" "a1" "a2" "a3") '((color "red"))))
                               (make-subgraph  
                                 :graph-conf '((label "process2") (style "filled") (color "aquamarine"))
                                 :node-conf '((color "black"))
                                 :edges (make-continuous-edges '("b0" "b1" "b2" "b3") '((color "blue")))))
              :ranks '(("start" "a0" "a1" "a2" "a3")
                       ("b0" "b1" "b2" "b3" "end")))
  :file "graph-utils-cluster-test")

