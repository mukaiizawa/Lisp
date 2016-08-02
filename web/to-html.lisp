
(defstruct tag
  (key nil :type symbol)
  (namespace "" :type string)
  (name "" :type string)
  (attr nil :type list)
  (children nil :type list))

;; global variables
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
;; *html-indent-manager* {{{

(defparameter *html-indent-manager* nil)


;; }}}
;; *html-indent* {{{

; set `nil' to disable indent.
(defparameter *html-indent* t)


;; }}}

;; macro, function
;; deftag {{{

(defmacro deftag (tag single? encode?)
  `(defmacro ,(mkkey tag) (&optional first &body rest)
     (with-gensyms (attri body)
       (let* ((attri (when (alist? first) first))
              (body (if attri rest (cons first rest))))
         `(let ,attri
            (format *html-stream* "~A<~(~A~)~{ ~{~(~A~)~^=\"~A\"~}~}~A>~%"
                    *html-indent-manager* ',',tag (list ,@(mapcar (lambda (x)
                                                                    `(remove nil (list ',x ,x)))
                                                                  (mapcar #'car attri)))
                    (mkstr (and ',',single? " /")))
            (indent-level *html-indent-manager* 'inc)
            (unless ',',single?
              (parse-inner-tag ,',encode? ,@body))
            (indent-level *html-indent-manager* 'dec)
            (unless ',',single?
              (format *html-stream* "~A</~(~A~)>~%" *html-indent-manager* ',',tag )))))))


;; }}}
;; deftags {{{

(defmacro deftags (tags single-tags no-encode-tags)
  `(progn ,@(mapcar (lambda (tag)
                      `(deftag ,tag
                               ,(find tag single-tags)
                               ,(not (find tag no-encode-tags))))
                    tags)))


;; }}}
;; definition of other tags {{{

(defmacro :!DOCTYPE (str)
  `(format *html-stream* "<!DOCTYPE ~A>~%" ,str))

(defmacro :!-- (str)
  `(format *html-stream* "~A<!-- ~A -->~%" *html-indent-manager* ,str))

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
                                       *html-indent-manager* (funcall (compose ,fn #'trim-newline) ,x)))
                             ((or (atom x)
                                  (s-expr? x))
                              `(format *html-stream*  "~A~A~%"
                                       *html-indent-manager* (funcall ,fn ,x)))
                             (t `,x))))
                   body)))))


;; }}}
;; trim-new-line {{{

(defun trim-newline (str)
  (if *html-indent*
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
;; for html-indent {{{

(defstruct (html-indent (:print-object get-html-indent))
  (indent "")
  (level 0)
  (tab-space 2))

(defun get-html-indent (html-indent stream)
  (if *html-indent*
    (format stream (html-indent-indent html-indent))
    ""))

(defun indent-level (html-indent direction)
  (when *html-indent*
    (if (eq direction 'inc)
      (incf (html-indent-level html-indent))
      (decf (html-indent-level html-indent)))
    (setf (html-indent-indent html-indent)
          (make-string
            (* (html-indent-tab-space html-indent)
               (html-indent-level html-indent))
            :initial-element #\Space)))
  (values))

(setq *html-indent-manager* (make-html-indent))


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
(deftags #.*html-tags* #.*single-tags* #.*no-encode-tags*)

