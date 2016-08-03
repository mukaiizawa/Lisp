
(load "stdlib" :if-does-not-exist nil)

(defstruct node
  (name-space "" :type string)
  (name "" :type string)
  (attrs nil :type list)
  (children nil :type list))

(defun node->string (node)
  (format nil "(~A:~A (~{~{(~(~A~) \"~A\")~}~^ ~})~A~A)~%"
          (node-name-space node)
          (node-name node)
          (node-attrs node)
          (if (node-children node) #\Newline "")
          (format nil "~{~A~}" (mapcar (lambda (node)
                                         (node->string node))
                                       (node-children node)))))

(defvar li1 (make-node :name "li" :attrs '((class "test-li") (name "li1"))))
(defvar li2 (make-node :name "li" :attrs '((class "test-li") (name "li2"))))
(defvar li3 (make-node :name "li" :attrs '((class "test-li") (name "li3"))))
(defvar ul (make-node :name "ul"
                      :attrs '((class "test-ul") (name "ul1"))
                      :children (list li1 li2 li3)))

; (princ (node->string ul))


