
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
  (value "" :type string)
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
                                    (make-xml-node :type 'text :value node)
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

(defmacro :!DOCTYPE (&optional (str "html"))
  `(make-xml-node :type 'document-type :value ,str))

;; }}}
;; :!-- {{{

(defmacro :!-- (str)
  `(make-xml-node :node-type 'comment :value ,str))

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
  (with-string-ahead-reader (reader str)
    (do ((nodes))
      ((reach-eof? (read-if (lambda (c)
                              (find c '(#\Newline #\Space)))
                            reader
                            :cache nil))
       (nreverse nodes))
      (push (parse-node reader) nodes))))

;; }}}
;; parse-node {{{

(defun parse-node (reader)
  (if (not (reader-next-in? (read-if (lambda (c)
                                       (find c '(#\Newline #\Space)))
                                     reader
                                     :cache nil)
                            #\<))
    ;; text-node
    (make-xml-node
      :type 'text
      :value (funcall #~s/(\n| )*$// (get-buf
                                       (read-if (lambda (c)
                                                  (char/= c #\<))
                                                (read-space reader :cache nil)))))
    (let ((linecount-at-open-tag (1+ (get-linecount reader)))
          (tag (parse-tag reader)))
      (cond ((find (xml-node-type tag) '(etag document-type comment))
             tag)
            ((xml-node-single? tag) 
             (setf (xml-node-type tag) 'element)
             tag)
            (t
              (setf (xml-node-type tag)
                    'element
                    (xml-node-children tag)
                    (do* ((linecount-at-child (get-linecount reader) (get-linecount reader))
                          (error?)
                          (node (parse-node reader) (parse-node reader))
                          (nodes))
                      ((let ((match-etag? (and (eq (xml-node-type node) 'etag)
                                               (string= (xml-node-name node) (xml-node-name tag)))))
                         (cond ((and error? match-etag?)
                                (error "parse-nodes: Missing start `~A' tag at line: ~A" (xml-node-name (first error?)) (second error?)))
                               (match-etag? t)
                               ((and error? (reach-eof? reader))
                                (error "parse-nodes: Missing end `~A' tag at line: ~A" (xml-node-name tag) linecount-at-open-tag))
                               (t nil)))
                       (nreverse nodes))
                      (when (reach-eof? reader)
                        (setq error? (list node (1+ linecount-at-child))))
                      (if (eq (xml-node-type node) 'etag)
                        (setq error? (list node (1+ linecount-at-child)))
                        (push node nodes))))
              tag)))))

;; }}}
;; parse-tag {{{

(defun parse-tag (reader)
  (read-if (lambda (c)    ; skip `<' or `</'
             (find c '(#\Newline #\Space #\< #\/)))
           reader
           :cache nil)
  (let* ((tag-type (cond ((reader-curr-in? reader #\/) 'etag)
                         ((not (reader-next-in? reader #\!)) 'stag)
                         (t 'others)))
         (name (get-buf
                 (read-space
                   (read-if (lambda (c)
                              (not (find c '(#\Space #\/ #\>))))
                            reader)
                   :cache nil))))
    (cond ((find tag-type '(stag etag))
           (let ((attrs) (single?))
             (setq attrs (parse-attrs reader)
                   single? (and (reader-next-in? reader #\/) t))
             (read-n-times reader (if single? 2 1) :cache nil)    ; skip `>' or `/>'
             (make-xml-node :type tag-type
                            :name name
                            :attrs attrs
                            :single? single?)))
          ((string= name "!--")
           (make-xml-node :type 'comment
                          :value (funcall #~s/ *-//g    ; trim right and remove hyphen
                                          (get-buf
                                            (read-next    ; skip `>'
                                              (read-if (lambda(c)
                                                         (declare (ignore c))
                                                         (not (and (reader-pre-in? reader #\-)
                                                                   (reader-curr-in? reader #\-)
                                                                   (reader-next-in? reader #\>))))
                                                       reader)
                                              :cache nil)))))
          (t
            (make-xml-node :type 'document-type
                           :value (get-buf
                                    (read-next    ; skip `>'
                                      (read-if (lambda (c)
                                                 (char/= c #\>))
                                               reader)
                                      :cache nil)))))))

;; }}}
;; parse-attrs {{{

(defun parse-attrs (reader)
  (do ((key) (value) (attrs))
    ((reader-next-in? (read-if (lambda (c)
                                 (find c '(#\Space #\Newline)))
                               reader :cache nil)
                      #\/ #\>)
     (nreverse attrs))
    (setq key (get-buf
                (read-if (lambda (c)
                           (not (find c '(#\Space #\= #\/ #\>))))
                         reader))
          value (and (reader-next-in? (read-if (lambda (c)
                                                 (find c '(#\= #\Space)))
                                               reader :cache nil)
                                      #\' #\")
                     (get-buf (read-segment reader))))
    (push (delete nil (list key value)) attrs)))

;; }}}

;; converter
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
                 (format out "\"~A\"" (xml-node-value node)))
                ((document-type)
                 (format out "(:!DOCTYPE \"~A\")" (xml-node-value node)))
                ((comment)
                 (format out "(:!-- \"~A\")" (xml-node-value node)))
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
                           (format out "~A~A~%" indent (with-xml-encode (xml-node-value nodes))))
                          ((eq (xml-node-type nodes) 'document-type)
                           (format out "~%<!DOCTYPE ~A>~%" (xml-node-value nodes)))
                          ((eq (xml-node-type nodes) 'comment)
                           (format out "~A<!-- ~A -->~%" indent (xml-node-value nodes)))
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

(defparameter dom
  "
<!DOCTYPE html>
<html>
<meta charset='utf-8'/>
<head>
<link href='css/common.css' rel='stylesheet' media='screen' />
</head>
<body>
<p>
<img src='img/image003.png' />
</p>
<a href='top.html'>title</a>
</body>
</html>
"
)

; #o(xml->dsl dom)
; #o(read-from-string (xml->dsl dom))
; ; #o(dsl->xml (mapcar #'eval (read-from-string (xml->dsl dom))))


