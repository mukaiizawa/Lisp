(require :stdlib *module-stdlib*)
(require :regex *module-regex*)
(provide :graph-utils)

(defparameter *global-graph-conf* '((layout "dot")
                                    (charset "UTF8")
                                    (rankdir "TB")))
(defparameter *global-node-conf* '((shape "record") (fontname "meiryo")))
(defparameter *global-edge-conf* '((fontname "meiryo")))
(defparameter *subgraphs-count* 0)

(defstruct graph
  (graph-conf *global-graph-conf* :type list)
  (node-conf *global-node-conf* :type list)
  (edge-conf *global-edge-conf* :type list)
  (nodes nil :type list)
  (edges nil :type list)
  (subgraphs nil :type list)
  (ranks nil :type list))

(defstruct (subgraph (:include graph
                               (graph-conf nil :type list)
                               (node-conf nil :type list)
                               (edge-conf nil :type list))))

(defun replace-low-line (str)
  (funcall #~s/-/_/g (mkstr str)))

(defun attrs->dot (attrs)
  (if (null attrs)
    ""
    (mkstr " ["
           (list->string
             (mapcar (lambda (x)
                       (let* ((key (first x))
                              (value (second x))
                              (value-string? (char/= (char value 0) #\<)))
                         (format nil "~(~A~)=~A"
                                 key (if value-string?    ; else table layout.
                                       (mkstr #\" value #\")
                                       value))))
                     attrs)
             #\,)
           "]")))

(defun nodes->dot (nodes)
  (with-output-to-string (out)
    (mapcar (lambda (node)
              (format out "~A~A;~%"
                      (replace-low-line (first node))
                      (attrs->dot (second node))))
            (mkalist nodes))))

(defun edges->dot (edges &optional (directed? t))
  (with-output-to-string (out)
    (mapcar (lambda (edge)
              (format out
                      "~A -~A ~A~A;~%"
                      (replace-low-line (first edge))
                      (if directed? ">" "-")
                      (replace-low-line (second edge))
                      (attrs->dot (third edge))))
            (mkalist edges))))

(defun graph->dot (graph)
  (with-output-to-string (out)
    (mapcar (lambda (key value)
              (when value
                (format out "~A~A;~%" key (attrs->dot value))))
            '("graph" "node" "edge")
            (list (graph-graph-conf graph)
                  (graph-node-conf graph)
                  (graph-edge-conf graph)))
    (format out "~A~%" (nodes->dot (graph-nodes graph)))
    (format out "~A~%" (edges->dot (graph-edges graph)))
    (format out "~{{rank=same;~{ ~A;~}}~%~}" (mklist (graph-ranks graph)))
    (mapcar (lambda (graph)
              (format out "~%subgraph cluster_~A {~%~A}~%"
                      (incf *subgraphs-count*) (graph->dot graph)))
            (mklist (graph-subgraphs graph)))))

(defun set-graph-attr! (key val alist)
  (if (assoc key alist)
    (rplacd (assoc key alist) (list val))
    (nconc alist (list (list key val))))
  (values alist))

(defun make-continuous-edges (nodes &optional attr)
  (if (rest nodes)
    (cons (list (first nodes) (second nodes) attr)
          (make-continuous-edges (rest nodes) attr))
    nil))

(defmethod dot ((g graph) &key (digraph? t) (file "graph"))
  (let ((input-file (mkstr file ".dot"))
        (output-file (mkstr file ".gif"))
        (dot (with-output-to-string (out)
               (format out "~Agraph g {~%" (if digraph? "di" ""))
               (format out (graph->dot g))
               (format out "}" ))))
    (write-to! dot input-file)
    (call "dot" (list "-Tgif" input-file
                      "-o" output-file)
          *standard-output*)))
