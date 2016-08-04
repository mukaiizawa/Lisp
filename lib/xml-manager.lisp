
(require "stdlib" *module-stdlib*)
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
(defparameter *indent* t)

(defstruct xml-node
  (name-space "" :type string)
  (name "" :type string)
  (attrs nil :type list)
  (children nil :type list)
  (single? nil :type boolean))

(defstruct indent-manager
  (indent "" :type string)
  (indent-level 0 :type number)
  (tab-space 2 :type number))

;; defnode {{{

(defmacro defnode (name-space name single?)
  `(defmacro ,(mkkey name) (&optional first &body rest)
     (with-gensyms (attr body)
       (let* ((attr (when (alist? first) first))
              (body (if attr rest (cons first rest))))
         `(let ,attr
            (make-xml-node
              :name-space ,',name-space
              :name ,',(mkstr name)
              :attrs (list ,@(mapcar (lambda (x)
                                       `(list ',x ,x))
                                     (mapcar #'car attr)))
              :children (remove nil (list ,@body))
              :single? ',',single?))))))

;; }}}
;; defnodes {{{

(defmacro defnodes (name-space node-names single-tags)
  `(progn ,@(mapcar (lambda (name)
                      `(defnode ,name-space ,name ,(when (find name single-tags) t)))
                    node-names)))

;; }}}
;; :!DOCTYPE {{{

;; support only html5
(defmacro :!DOCTYPE ()
  `(make-xml-node :name "doctype"))

;; }}}
;; :!-- {{{

(defmacro :!-- (&rest str)
  `(make-xml-node :name "comment" :children (list ,@str)))

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
;; with-html-encode {{{

(defun with-html-encode (str)
  (with-output-to-string (buf)
    (dostring (c (mkstr str))
      (princ
        (case c
          (#\" "&quot;")
          (#\& "&amp;")
          (#\< "&lt;")
          (#\> "&gt;")
          (t c))
        buf))))

;; }}}

;; get-node-name {{{

(defun get-node-name (node)
  (with-output-to-string (out)
    (unless (empty? (xml-node-name-space node))
      (format out "~(~A~):" (xml-node-name-space node)))
    (format out "~(~A~)" (xml-node-name node))))

;; }}}
;; xml-nodes->string {{{

(defun xml-nodes->string (nodes &optional (indent (make-indent-manager)))
  (with-output-to-string (out)
    (cond ((null nodes)
           (error "xml-node->string: null pointer exeption"))
          ((stringp nodes)    ; text-node
           (format out "~A~A~%" (indent-manager-indent indent) (with-html-encode nodes)))
          ((and (typep nodes 'xml-node)
                (string= (xml-node-name nodes) "doctype"))
           (format out "<!DOCTYPE html>~%"))
          ((and (typep nodes 'xml-node)
                (string= (xml-node-name nodes) "comment"))
           (format out "~A<!-- ~{~A~^~%~} -->~%" (indent-manager-indent indent) (xml-node-children nodes)))
          (t
            (dolist (node (mklist nodes))
              (format out "~A<~A~{ ~{~(~A~)~^=\"~A\"~}~}~A>~%"
                      (indent-manager-indent indent)
                      (get-node-name node)
                      (xml-node-attrs node)
                      (if (xml-node-single? node) "/" ""))
              (change-indent-level indent 'inc)
              (awhen (xml-node-children node)
                (format out "~{~^~A~}"
                        (mapcar (lambda (node)
                                  (xml-nodes->string node indent))
                                it)))
              (change-indent-level indent 'dec)
              (unless (xml-node-single? node)
                (format out "~A</~A>~%"
                        (indent-manager-indent indent)
                        (get-node-name node))))))))

;; }}}

;; define html tags
(defnodes "" #.*html-tags* #.*single-tags*)

