
(load "../lib/stdlib" :if-does-not-exist nil)
(load "to-html" :if-does-not-exist nil)

(defstruct (node (:conc-name nil))
  (name-space nil)
  (name nil)
  (attr nil)
  (chldern nil))

(defmethod node->string ((node node))
  (format nil "(:~A (~{~{(~A \"~A\")~}~}) ~A)"
          (name node) (attr node)
          (format nil "~{~A~}" (mapcar (lambda (node)
                                         (node->string node))
                                       (chldern node)))))


(defvar li1 (make-node :name "li" :attr '((class "test-li") (name "li1"))))
(defvar li2 (make-node :name "li" :attr '((class "test-li") (name "li2"))))
(defvar li3 (make-node :name "li" :attr '((class "test-li") (name "li3"))))
(defvar ul (make-node :name "ul"
                      :attr '((class "test-ul") (name "ul1"))
                      :child-nodes (list li1 li2 li3)))

(print (read-from-string (node->string ul)))
;; (:UL ((CLASS "test-ul") (NAME "ul1"))
;;      (:LI ((CLASS "test-li") (NAME "li1")))
;;      (:LI ((CLASS "test-li") (NAME "li2")))
;;      (:LI ((CLASS "test-li") (NAME "li3")))) 


(eval (read-from-string (node->string ul)))
;; <ul class="test-ul" name="ul1">
;;   <li class="test-li" name="li1">
;;   </li>
;;   <li class="test-li" name="li2">
;;   </li>
;;   <li class="test-li" name="li3">
;;   </li>
;; </ul>


