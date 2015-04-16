(defparameter *html-output* *standard-output*)

(defmacro deftag (name closingp)
  `(defmacro ,name (attri &body body)
     `(progn
        (format *html-output* "~(<~A~{~{ ~A=\"~A\"~}~}~A>~)~%" ',',name ',attri
                (if ',',closingp "" "/"))
        (text-node ,@body)
        (when ',',closingp
          (format *html-output* "~(</~A>~)~%" ',',name)))))

(defmacro deftags (&rest names)
  `(progn
     ,@(mapcar (lambda (name)
                 `(deftag ,@name))
               names)))

(defmacro text-node (&body body)
  `(progn
     ,@(mapcar (lambda (node)
                 (if (stringp node)
                   `(format *html-output* "~A~%" ,node)
                   node))
               body)))


(deftags (html t)
         (body t)
         (head t)
         (title t)
         (h1 t)
         (h2 t)
         (h3 t)
         (h4 t)
         (h5 t)
         (h6 t)
         (br nil)
         (meta nil)
         (p t)
         (a t)
         (ul t)
         (li t)
         (div t)
         (img nil))


(with-open-file (stream "output.html"
                        :direction :output
                        :if-exists :supersede)
  ; (setf *html-output* stream)
  (html ((lang ja))
        (head ()
              (meta ((charset utf-8)))
              (title ()
                     "html test"))
        (body ()
              (div ((class container) (id container))
                   (p ()
                      "hello world")))))
