
(require "stdlib" *module-stdlib*)
(require "ahead-reader" *module-ahead-reader*)
(require "regex" *module-regex*)
(provide "xml-manager")

;; *html-tags* {{{

(defparameter *html-tags*
  '( :a
     :abbr
     :acronym
     :address
     :applet
     :area
     :article
     :aside
     :audio
     :b
     :base
     :basefont
     :bdi
     :bdo
     :big
     :blockquote
     :body
     :br
     :button
     :canvas
     :caption
     :center
     :cite
     :code
     :col
     :colgroup
     :datalist
     :dd
     :del
     :details
     :dfn
     :dialog
     :dir
     :div
     :dl
     :dt
     :em
     :embed
     :fieldset
     :figcaption
     :figure
     :font
     :footer
     :form
     :frame
     :frameset
     :h1
     :h2
     :h3
     :h4
     :h5
     :h6
     :head
     :header
     :hr
     :html
     :i
     :iframe
     :img
     :input
     :ins
     :kbd
     :keygen
     :label
     :legend
     :li
     :link
     :main
     :map
     :mark
     :menu
     :menuitem
     :meta
     :meter
     :nav
     :noframes
     :noscript
     :object
     :ol
     :optgroup
     :option
     :output
     :p
     :param
     :pre
     :progress
     :q
     :rp
     :rt
     :ruby
     :s
     :samp
     :script
     :section
     :select
     :small
     :source
     :span
     :strike
     :strong
     :style
     :sub
     :summary
     :sup
     :table
     :tbody
     :td
     :textarea
     :tfoot
     :th
     :thead
     :time
     :title
     :tr
     :track
     :tt
     :u
     :ul
     :var
     :video
     :wbr))

;; }}}
;; *single-tags* {{{

(defparameter *single-tags*
  '( :area
     :base
     :br
     :col
     :embed
     :hr
     :img
     :input
     :keygen
     :link
     :meta
     :param
     :source))

;; }}}

(defstruct xml-node
  (type 'unspecified :type symbol)
  (name "" :type string)
  (attrs nil :type list)
  (children nil :type list)
  (single? nil :type boolean))

;; indent manager 
;; *indent* {{{

(defparameter *indent* t)

;; }}}
;; indent-manager {{{

(defstruct indent-manager
  (indent "" :type string)
  (indent-level 0 :type number)
  (tab-space 2 :type number))

;; }}}
;; change-indent-level {{{

(defun change-indent-level (indent direction)
  (when *indent*
    (if (eq direction 'inc)
      (incf (indent-manager-indent-level indent))
      (decf (indent-manager-indent-level indent)))
    (setf (indent-manager-indent indent)
          (make-string
            (* (indent-manager-tab-space indent)
               (indent-manager-indent-level indent))
            :initial-element #\Space)))
  (values))

;; }}}

;; define element
;; *node-name-mapping* {{{

(defparameter *node-name-mapping* (make-hash-table :test 'equal))

;; }}}
;; defelement {{{

(defmacro defelement (namespace name mapping-name single?)
  (set-node-name-mapping namespace name mapping-name)
  `(defmacro ,mapping-name (&optional first &body rest)
     (with-gensyms (attr body)
       (let* ((attr (when (alist? first) first))
              (body (if attr rest (cons first rest))))
         `(let ,attr
            (make-xml-node
              :type 'element
              :name ',',(merge-node-name namespace name)
              :attrs (list ,@(mapcar (lambda (x)
                                       `(list ',x ,x))
                                     (mapcar #'car attr)))
              :children (mapcar (lambda (node)
                                  (if (stringp node)
                                    (make-xml-node :type 'text :children (list node))
                                    node))
                                (remove nil (list ,@body)))
              :single? ',',single?))))))

;; }}}
;; defelements {{{

(defmacro defelements (namespace names mapping-names single-tags)
  `(progn
     ,@(mapcar (lambda (name mapping-name)
                 `(defelement ,namespace
                              ,name
                              ,mapping-name
                              ,(when (find mapping-name single-tags) t)))
               names mapping-names)))

;; }}}
;; :!DOCTYPE {{{

;; support only html5
(defmacro :!DOCTYPE (&optional (str "html"))
  `(make-xml-node :type 'document-type :children (list ,str)))

;; }}}
;; :!-- {{{

(defmacro :!-- (&rest str)
  `(make-xml-node :node-type 'comment :children (list ,@str)))

;; }}}
;; with-xml-encode {{{

(defun with-xml-encode (str)
  (with-output-to-string (buf)
    (dostring (c str)
      (princ
        (case c
          (#\" "&quot;")
          (#\& "&amp;")
          (#\< "&lt;")
          (#\> "&gt;")
          (t c))
        buf))))

;; }}}
;; merge-node-name {{{

(defun merge-node-name (namespace name)
  (with-output-to-string (out)
    (when (not (empty? namespace))
      (write-string namespace out)
      (write-string ":" out))
    (write-string name out)))

;; }}}
;; set-node-name-mapping {{{

(defun set-node-name-mapping (namespace name mapping-name)
  (setf (gethash (merge-node-name namespace name)
                 *node-name-mapping*)
        mapping-name))

;; }}}
;; get-node-name-mapping {{{

(defun get-node-name-mapping (namespace-name)
  (aif (gethash
         (apply #'merge-node-name 
                (if (position #\: namespace-name)
                  (string->list #\: namespace-name)
                  (list nil namespace-name)))
         *node-name-mapping*)
    (string-downcase (mkstr it))
    (error "get-node-name-mappin: undefined element `~A'." namespace-name)))

;; }}}

;; xml-parser
;; parse-xml {{{

(defun parse-xml (str)
  (let (nodes)
    (with-string-ahead-reader (reader str)
      (while (not (reach-eof? reader))
        (cond ((reader-next-in? reader #\Newline #\Space)
               (read-if (lambda (c)
                          (or (char= c #\Newline)
                              (char= c #\Space)))
                        reader
                        :cache nil))
              ((not (reader-next-in? reader #\<))
               ;; text-node
               (push
                 (make-xml-node
                   :type 'text
                   :children (list (get-buf
                                     (read-if (lambda (c)
                                                (char/= c #\<))
                                              reader))))
                 nodes))
              (t
                (let ((inner-paren (get-buf (read-paren reader))))
                  (cond ((funcall #~m/^!-- .* --$/ inner-paren)
                         ;; comment-node
                         (push (make-xml-node
                                 :type 'comment
                                 :children (list (subseq inner-paren 3 (- (length inner-paren) 2))))
                               nodes))
                        ((funcall #~m/^!DOCTYPE/ inner-paren)
                         ;; document-node
                         (push (make-xml-node
                                 :type 'document-type
                                 :children (list (subseq inner-paren 9)))
                               nodes))
                        (t
                          ;; element-node
                          (let ((node (parse-element inner-paren)))
                            (if (xml-node-single? node)
                              (push node nodes)
                              (progn
                                (setf (xml-node-children node)
                                      (parse-xml (read-children reader (xml-node-name node))))
                                (push node nodes)))))))))))
    (nreverse nodes)))

;; }}}
;; read-children {{{

(defmethod read-children ((reader ahead-reader) (element-name string))
  (let ((linecount (get-linecount reader))
        (matcher (mkstr "</" element-name " *>")))
    (while (not (or (reach-eof? reader)
                    (match?->string matcher (refer-buf (read-next reader))))))
    (if (match?->string matcher (refer-buf reader))
      (match?->replace matcher "" (get-buf reader))
      (error "read-children: Closing tag of `~A' at line: ~A was not found." element-name linecount))))

;; }}}
;; parse-element {{{

(defun parse-element (str)
  (with-string-ahead-reader (reader str)
    (let* ((namespace-name (get-buf
                             (read-space
                               (read-if (lambda (c)
                                          (char/= c #\Space))
                                        reader)
                               :cache nil)))
           (attrs-part (get-buf (read-if (lambda (c)
                                           (char/= c +null-character+))
                                         reader)))
           (single? (char= (char str (1- (length str))) #\/)))
      (make-xml-node :type 'element
                     :name (get-node-name-mapping namespace-name)
                     :attrs (parse-attrs (if single?
                                           (before #\/ attrs-part :from-end t)
                                           attrs-part))
                     :single? single?))))

;; }}}
;; parse-attrs {{{

(defun parse-attrs (str)
  (with-string-ahead-reader (reader (funcall #~s/\n//g str))
    (do ((key) (value) (attrs))
      ((reach-eof? (read-space reader :cache nil))
       (nreverse attrs))
      (setq key (get-buf
                  (read-if (lambda (c)
                             (and (char/= c #\Space)
                                  (char/= c #\=)))
                           reader))
            value (and (reader-next-in? (read-if (lambda (c)
                                                   (or (char= c #\=)
                                                       (char= c #\Space)))
                                                 reader :cache nil)
                                        #\' #\")
                       (get-buf (read-segment reader))))
      (push (delete nil (list key value)) attrs))))

;; }}}
;; xml-nodes->DSL {{{

(defun xml-nodes->DSL (nodes)
  (with-output-to-string (out)
    (mapcar (lambda (node)
              (case (xml-node-type node)
                ((element)
                 (format out "(:~A (~{~{(~A \"~A\")~}~^ ~})~A)"
                         (xml-node-name node)
                         (xml-node-attrs node)
                         (if (xml-node-children node)
                           (format nil "~{~%~A~}"
                                   (mapcar (lambda (child-node)
                                             (xml-nodes->DSL child-node))
                                           (xml-node-children node)))
                           +empty-string+)))
                ((text)
                 (format out "~{\"~A\"~}~%" (xml-node-children node)))
                ((document-type)
                 (format out "(:!DOCTYPE \"~{~A~}\")~%" (xml-node-children node)))
                (t
                  (error "xml-nodes->DSL: unknown node type ~A" (xml-node-type node)))))
            (mklist nodes))))

;; }}}

;; DSL->xml {{{

(defun DSL->xml (&rest nodes)
  (labels ((rec (nodes indent-manager)
                (with-output-to-string (out)
                  (let ((indent (indent-manager-indent indent-manager)))
                    (cond ((or (null nodes)
                               (and (not (listp nodes))
                                    (not (typep nodes 'xml-node))))
                           (error "DSL->xml: `~A' unexpected token." nodes))
                          ((listp nodes)
                           (format out "~{~A~}"
                                   (mapcar (lambda (node)
                                             (rec node indent-manager))
                                           nodes)))
                          ((eq (xml-node-type nodes) 'text)
                           (format out "~A~{~A~}~%" indent (mapcar #'with-xml-encode (xml-node-children nodes))))
                          ((eq (xml-node-type nodes) 'document-type)
                           (format out "~%<!DOCTYPE ~{~A~}>~%" (xml-node-children nodes)))
                          ((eq (xml-node-type nodes) 'comment)
                           (format out "~A<!-- ~{~A~^~%~} -->~%" indent (xml-node-children nodes)))
                          (t
                            (format out "~A<~A~{ ~{~(~A~)~^=\"~A\"~}~}~A>~%"
                                    indent
                                    (xml-node-name nodes)
                                    (xml-node-attrs nodes)
                                    (if (xml-node-single? nodes) "/" ""))
                            (change-indent-level indent-manager 'inc)
                            (awhen (xml-node-children nodes)
                              (format out "~A" (rec it indent-manager)))
                            (change-indent-level indent-manager 'dec)
                            (unless (xml-node-single? nodes)
                              (format out "~A</~A>~%"
                                      indent
                                      (xml-node-name nodes)))))))))
    (rec nodes (make-indent-manager))))

;; }}}
;; xml->DSL {{{

(defun xml->DSL (str)
  (xml-nodes->DSL (parse-xml str)))

;; }}}

;; define html tags
;; mapping-names must be keyword parameter
(defelements "" #.(mapcar (compose #'string-downcase #'mkstr) *html-tags*) #.*html-tags* #.*single-tags*)

;; test code
(defparameter dom
"
<html>
<!DOCTYPE html>
<html>
<meta charset='utf-8'  chaa='as'/>
<head>
<link href='css/common.css' rel='stylesheet' media='screen' />
</head>
<body>
<p>
inner
ptag
<img src='img/image003.png' />
</p>
<a href='top.html'>
</a>
</body>
</html>
</html>
"
)
#o(xml->DSL dom)
#o(read-from-string (xml->DSL dom))
; #o(dsl->xml (eval (read-from-string (xml->DSL dom))))
; #o(dsl->xml (read-from-string (xml->DSL dom)))
