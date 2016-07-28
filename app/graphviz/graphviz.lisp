
(load "../../lib/stdlib")

(defparameter *global-graph-conf* '((layout "dot") (charset "UTF-8") (rankdir "TB")))
(defparameter *global-node-conf* '((shape "record") (fontname "meiryo")))
(defparameter *global-edge-conf* '((fontname "meiryo")))
(defparameter *subgraph-count* 0)

(defstruct graph
  (graph-conf *global-graph-conf* :type list)
  (node-conf *global-node-conf* :type list)
  (edge-conf *global-edge-conf* :type list)
  (nodes nil :type list)
  (edges nil :type list)
  (subgraph nil :type list)
  (rank nil :type list))

(defstruct (subgraph (:include graph
                               (graph-conf nil :type list)
                               (node-conf nil :type list)
                               (edge-conf nil :type list))))

(defstruct node value attr)
(defstruct edge from to attr (directed? t :type boolean))
(defstruct attr key value)

(defun replace-low-line (str)
  (replstr "-" "_" (mkstr str)))

;; to-string attr {{{

(defmethod to-string ((alist list))
  (format nil "[~{~{~(~A~)=\"~A\"~}~^,~%~}]" alist))

;; }}}
;; to-string edge {{{

(defmethod to-string ((e edge))
  (mkstr (replace-low-line (edge-from e))
         (if (edge-directed? e) " -> " " -- ")
         (replace-low-line (edge-to e)) " "
         (to-string (edge-attr e))))
;; }}}
;; to-string node {{{

(defmethod to-string ((n node))
  (mkstr (replace-low-line (node-value n)) " " (to-string (node-attr n))))

;; }}}
;; to-string graph{{{

(defmethod to-string ((g graph))
  (with-output-to-string (out)
    (mapcar (lambda (key value)
              (format out "~A ~A;~%"
                      key (to-string value)))
            '("graph" "node" "edge")
            (list (graph-graph-conf g) (graph-node-conf g) (graph-edge-conf g)))
    (format out "~{~A;~%~}" (append (mapcar #'to-string (graph-nodes g))
                                    (mapcar #'to-string (graph-edges g))))
    (mapcar (lambda (graph)
              (format out "~%subgraph cluster_~A {~%~A}~%"  (incf *subgraph-count*) (to-string graph)))
            (graph-subgraph g))))

;; }}}

;; set-attr! {{{

(defun set-attr! (key val alist)
  (if (assoc key alist)
    (rplacd (assoc key alist) (list val))
    (nconc alist (list (list key val))))
  (values alist))

;; }}}
;; make-nodes {{{

(defun make-nodes (values &optional attr)
  (mapcar (lambda (value)
            (make-node :value value
                       :attr attr))
          (mklist values)))

;; }}}
;; make-record {{{

(defun make-records (lis &optional attr)
  (let (acc)
    (mapcar (lambda (lis)
              (let ((required-attr `((shape "record") (label ,(mkstr "{" (list->string lis :char #\|) "}")))))
                (push (make-node :value (first lis)
                                 :attr (if attr (append required-attr attr) required-attr))
                      acc)))
            lis)
    (nreverse acc)))

;; }}}
;; make-edges {{{

(defun make-edges (from-to-list &optional attr)
  (mapcar (lambda (from-to)
            (make-edge :from (first from-to)
                       :to (second from-to)
                       :attr attr))
          (mklist from-to-list)))

;; }}}
;; make-continuous-edges {{{

(defun make-continuous-edges (nodes &optional attr)
  (if (rest nodes)
    (cons (make-edge :from (first nodes) :to (second nodes) :attr attr)
          (make-continuous-edges (rest nodes)))
    nil))

;; }}}

(defmethod dot ((g graph) &key (digraph? t) (file "graph"))
  (let ((input-file (mkstr file ".dot"))
        (output-file (mkstr file ".gif"))
        (dot (with-output-to-string (out)
               (format out "~Agraph g {~%" (if digraph? "di" ""))
               (format out (to-string g))
               (format out "}" ))))
    (write-to! dot input-file)
    (call "dot" (list "-Tgif" input-file
                      "-o" output-file)
          *standard-output*)))

