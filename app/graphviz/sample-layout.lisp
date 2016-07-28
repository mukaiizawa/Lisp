
(load "graphviz")

(defparameter *edges* nil)
(dostring (c (expand-hyphen "B-Z"))
  (push (make-edge :from #\A :to c) *edges*))

(dolist (layout (list "circo" "dot" "fdp" "neato" "osage" "sfdp" "twopi"))
  (dot (make-graph :graph-conf (set-attr! 'layout layout *global-graph-conf*)
                    :edges (reverse *edges*))
        :file (mkstr "sample-layout-" layout)))

