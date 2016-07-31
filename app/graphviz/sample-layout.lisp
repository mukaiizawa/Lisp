
(load "graphviz")

(defparameter *edges* nil)
(dostring (c (expand-hyphen "B-Z"))
  (push (list #\A c) *edges*))

(dolist (layout (list "circo" "dot" "fdp" "neato" "osage" "sfdp" "twopi"))
  (set-attr! 'layout layout *global-graph-conf*)
  (dot (make-graph :edges (reverse *edges*))
       :file (mkstr "sample-layout-" layout)))

