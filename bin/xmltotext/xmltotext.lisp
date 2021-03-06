(require :stdlib *module-stdlib*)
(require :xml-manager *module-xml-manager*)

(defun get-text (nodes)
  (labels ((rec (nodes)
                (cond ((listp nodes)
                       (dolist (i nodes)
                         (get-text i)))
                      ((xml-node-children nodes)
                       (dolist (i (xml-node-children nodes))
                         (get-text i)))
                      ((eq (xml-node-type nodes) 'text)
                       (princln (xml-node-value nodes)))
                      (t nil))))
    (rec nodes)))

(defun main (stream)
  (get-text (parse-xml stream)))
