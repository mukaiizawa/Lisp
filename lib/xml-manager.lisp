
(load "stdlib" :if-does-not-exist nil)

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
;; *no-encode-tags* {{{

(defparameter *no-encode-tags*
  '( :script
     :style))

;; }}}
;; *html-stream* {{{

; default stream is *standard-output*
(defparameter *html-stream* *standard-output*)

;; }}}
;; *indent-manager* {{{

(defparameter *indent-manager* nil)

;; }}}
;; *indent* {{{

; set `nil' to disable indent.
(defparameter *indent* t)

;; }}}

(defstruct (node (:print-object node->string))
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

(defstruct (indent (:print-object indent->string))
  (indent "" :type string)
  (level 0 :type number)
  (tab-space 2 :type number))

(defun indent->string (indent stream)
  (if *indent*
    (format stream (indent-indent indent))
    ""))

;; macro, function
;; defnode {{{

(defmacro defnode (tag single? encode?)
  `(defmacro ,(mkkey tag) (&optional first &body rest)
     (with-gensyms (attr body)
       (let* ((attr (when (alist? first) first))
              (body (if attr rest (cons first rest))))
         `(let ,attr
            (format *html-stream* "~A<~(~A~)~{ ~{~(~A~)~^=\"~A\"~}~}~A>~%"
                    *indent-manager* ',',tag (list ,@(mapcar (lambda (x)
                                                                    `(remove nil (list ',x ,x)))
                                                                  (mapcar #'car attr)))
                    (mkstr (and ',',single? " /")))
            (change-indent-level *indent-manager* 'inc)
            (unless ',',single?
              (parse-inner-tag ,',encode? ,@body))
            (change-indent-level *indent-manager* 'dec)
            (unless ',',single?
              (format *html-stream* "~A</~(~A~)>~%" *indent-manager* ',',tag )))))))

;; }}}
;; defnodes {{{

(defmacro defnodes (tags single-tags no-encode-tags)
  `(progn ,@(mapcar (lambda (tag)
                      `(defnode ,tag
                               ,(find tag single-tags)
                               ,(not (find tag no-encode-tags))))
                    tags)))

;; }}}
;; definition of other tags {{{

(defmacro :!DOCTYPE (str)
  `(format *html-stream* "<!DOCTYPE ~A>~%" ,str))

(defmacro :!-- (str)
  `(format *html-stream* "~A<!-- ~A -->~%" *indent-manager* ,str))

;; }}}
;; parse-inner-tag {{{

(defmacro parse-inner-tag (encode? &body body)
  (with-gensyms (fn)
    (let ((fn (if encode?
                #'with-html-encode
                #'identity)))
      `(progn
         ,@(mapcar (lambda (x)
                     (when x
                       (cond ((stringp x)
                              `(format *html-stream* "~A~A~%"
                                       *indent-manager* (funcall (compose ,fn #'trim-newline) ,x)))
                             ((or (atom x)
                                  (s-expr? x))
                              `(format *html-stream*  "~A~A~%"
                                       *indent-manager* (funcall ,fn ,x)))
                             (t `,x))))
                   body)))))


;; }}}
;; trim-new-line {{{

(defun trim-newline (str)
  (if *indent*
    (let (ignore-space?)
      (with-output-to-string (buf)
        (dostring (c (mkstr str))
          (cond ((char= c #\Newline)
                 (setq ignore-space? t)
                 (princ #\Space buf))
                ((and ignore-space?
                      (char= c #\Space))
                 (values))
                ((and ignore-space?
                      (char/= c #\Space))
                 (setq ignore-space? nil)
                 (princ c buf))
                (t (princ c buf))))))
    str))

;; }}}
;; for indent {{{

(defun change-indent-level (indent direction)
  (when *indent*
    (if (eq direction 'inc)
      (incf (indent-level indent))
      (decf (indent-level indent)))
    (setf (indent-indent indent)
          (make-string
            (* (indent-tab-space indent)
               (indent-level indent))
            :initial-element #\Space)))
  (values))

(setq *indent-manager* (make-indent))

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
;; with-html-output {{{

(defmacro with-html-output ((&optional (stream *standard-output*)) &body body)
  `(let ((*html-stream* ,stream))
     ,@body))

;; }}}

;; define html tags
(defnodes #.*html-tags* #.*single-tags* #.*no-encode-tags*)

