
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
  (phisical-namespace nil :type symbol)
  (logical-namespace "" :type string)
  (phisical-name nil :type symbol)
  (logical-name "" :type string)
  (attrs nil :type list)
  (children nil :type list)
  (single? nil :type boolean))

(defstruct indent-manager
  (indent "" :type string)
  (indent-level 0 :type number)
  (tab-space 2 :type number))

;; defnode {{{

(defmacro defnode (phisical-namespace logical-namespace phisical-name logical-name single?)
  `(defmacro ,(mkkey phisical-name) (&optional first &body rest)
     (with-gensyms (attr body)
       (let* ((attr (when (alist? first) first))
              (body (if attr rest (cons first rest))))
         `(let ,attr
            (make-xml-node
              :phisical-namespace ',',(mksym phisical-namespace)
              :logical-namespace ,',logical-namespace
              :phisical-name ',',(mksym phisical-name)
              :logical-name ,',logical-name
              :attrs (list ,@(mapcar (lambda (x)
                                       `(list ',x ,x))
                                     (mapcar #'car attr)))
              :children (mapcar (lambda (node)
                                  (if (stringp node)
                                    (make-xml-node :phisical-name 'text-node :children (list node))
                                    node))
                                (remove nil (list ,@body)))
              :single? ',',single?))))))

;; }}}
;; defnodes {{{

(defmacro defnodes (phisical-namespace logical-namespace phisical-names logical-names single-tags)
  `(progn
     ,@(mapcar (lambda (phisical-name logical-name)
                 `(defnode ,phisical-namespace ,logical-namespace
                           ,phisical-name ,logical-name
                           ,(when (find phisical-name single-tags) t)))
               phisical-names logical-names)))

;; }}}
;; :!DOCTYPE {{{

;; support only html5
(defmacro :!DOCTYPE ()
  `(make-xml-node :phisical-name 'doctype))

;; }}}
;; :!-- {{{

(defmacro :!-- (&rest str)
  `(make-xml-node :phisical-name 'comment :children (list ,@str)))

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

;; get-logical-fullname {{{

(defun get-logical-fullname (node)
  (with-output-to-string (out)
    (unless (empty? (xml-node-logical-namespace node))
      (format out "~A:" (xml-node-logical-namespace node)))
    (format out "~A" (xml-node-logical-name node))))

;; }}}
;; xml-nodes->string {{{

(defun xml-nodes->string (nodes &optional (indent-manager (make-indent-manager)))
  (with-output-to-string (out)
    (let ((indent (indent-manager-indent indent-manager)))
      (cond ((or (null nodes)
                 (and (not (listp nodes))
                      (not (typep nodes 'xml-node))))
             (error "xml-node->string: `~A' unexpected token." nodes))
            ((listp nodes)
             (format out "~{~A~}"
                     (mapcar (lambda (node)
                               (xml-nodes->string node indent-manager))
                             nodes)))
            ((eq (xml-node-phisical-name nodes) 'text-node)
             (format out "~A~{~A~}~%" indent (mapcar #'with-xml-encode (xml-node-children nodes))))
            ((eq (xml-node-phisical-name nodes) 'doctype)
             (format out "~%<!DOCTYPE html>~%"))
            ((eq (xml-node-phisical-name nodes) 'comment)
             (format out "~A<!-- ~{~A~^~%~} -->~%" indent (xml-node-children nodes)))
            (t
              (format out "~A<~A~{ ~{~(~A~)~^=\"~A\"~}~}~A>~%"
                      indent
                      (get-logical-fullname nodes)
                      (xml-node-attrs nodes)
                      (if (xml-node-single? nodes) "/" ""))
              (change-indent-level indent-manager 'inc)
              (awhen (xml-node-children nodes)
                (format out "~A" (xml-nodes->string it indent-manager)))
              (change-indent-level indent-manager 'dec)
              (unless (xml-node-single? nodes)
                (format out "~A</~A>~%"
                        indent
                        (get-logical-fullname nodes))))))))

;; }}}

;; define html tags
(defnodes nil ""
          #.*html-tags* #.(mapcar (lambda (tag)
                                    (string-downcase (mkstr tag)))
                                  *html-tags*)
          #.*single-tags*)

