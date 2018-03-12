; xc markdown reader

(require :ahead-reader *module-ahead-reader*)
(require :xml-manager *module-xml-manager*)

(defparameter *outline* nil)

(defun trim-left (s)
  (if (char= (char s 0) #\space)
    (trim-left (subseq s 1))
    s))

(defun with-br (nodes)
  (nreverse (reduce (lambda (x y)
                      (cons y (cons `(:br) x)))
                    (cdr nodes)
                    :initial-value (list (car nodes)))))

(defun parse-outline (outline)
  (let ((levels (make-array 6 :initial-element 0)))
    `(list
       (:h1 "目次")
       (:pre ((class "index"))
         ,@(mapcar (lambda (x)
                     (with-output-to-string (out)
                       (let ((level (car x )) (header (cadr x)))
                         (dotimes (i 6)
                           (cond ((= i level) (incf (aref levels i)))
                                 ((> i level) (setf (aref levels i) 0))))
                         (princ (make-string (* 2 level)
                                             :initial-element #\Space)
                                out)
                         (dotimes (i (1+ level))
                           (princ (aref levels i) out)
                           (princ #\. out))
                         (princ #\space out)
                         (princ header out)
                         out)))
                   (nreverse outline))))))

(defmethod read-blank ((ar ahead-reader))
  (read-if (lambda (c) (char= c #\newline)) ar :cache nil)
  ar)

(defmethod get-line ((ar ahead-reader))
  (get-buf (xread-line ar)))

(defmethod parse-header ((ar ahead-reader))
  (let ((line (get-line ar)) (level 0))
    (while (char= (char line 0) #\#)
      (setq line (subseq line 1))
      (incf level))
    (setq line (trim-left line))
    (push (list (decf level) line) *outline*)
    (list (ecase level
            ((0) :h1)
            ((1) :h2)
            ((2) :h3)
            ((3) :h4)
            ((4) :h5)
            ((5) :h6))
          line)))

(defmethod quote-level ((ar ahead-reader) &optional (level 0))
  (if (char= (get-next ar (1+ level)) #\>)
    (quote-level ar (1+ level))
    (1- level)))

(defmethod parse-quote-block ((ar ahead-reader) &optional (level 0))
  (let ((line))
    (push (subseq (get-line ar) (1+ level)) line)
    (while (char= (get-next ar) #\>)
      (let ((curr-level (quote-level ar)))
        (if (= curr-level level)
          (progn (push `(:br) line)
                 (push (subseq (get-line ar) (1+ level)) line))
          (push (parse-quote-block ar curr-level) line))))
    `(:blockquote ,@(nreverse line))))

(defmethod parse-paragraph ((ar ahead-reader))
  `(:p ,(get-line ar)))

(defmethod preformatted-text? ((ar ahead-reader) &optional (col 1))
  (or (> col 4)
      (and (char= (get-next ar col) #\space)
           (preformatted-text? ar (1+ col)))))

(defmethod parse-preformatted_text_block ((ar ahead-reader))
  (let (line)
    (while (preformatted-text? ar)
      (push (subseq (get-line ar) 4) line))
    `(:pre ,@(nreverse line))))

(defmethod table-separator? ((ar ahead-reader))
  (and (char= (get-next ar 1) #\-)
       (char= (get-next ar 2) #\-)
       (char= (get-next ar 3) #\newline)))

(defmethod parse-table-row ((ar ahead-reader) &optional header?)
  `(:tr ,@(mapcar (lambda (x)
                    (if header? `(:th ,x) `(:td ,x)))
                  (string->list #\tab (get-line ar)))))

(defmethod parse-table ((ar ahead-reader))
  (get-line ar)
  (let ((header (parse-table-row ar t))
        (body nil))
    (get-line ar)
    (while (not (table-separator? ar))
      (push (parse-table-row ar) body))
    (get-line ar)
    `(:table (:thead ,header) (:tbody ,@(nreverse body)))))

(defmethod list-line? ((ar ahead-reader))
  (find (get-next ar) '(#\- #\+)))

(defmethod list-level ((ar ahead-reader) &optional (level 0))
  (if (find (get-next ar (1+ level)) '(#\- #\+))
    (list-level ar (1+ level))
    (1- level)))

(defmethod parse-list-block ((ar ahead-reader) &optional (level 0))
  (let ((li-list) (ul? (char= (get-next ar) #\-)))
    (while (list-line? ar)
      (let ((curr-level (list-level ar)))
        (push (if (= curr-level level)
                `(:li ,(subseq (get-line ar) (1+ level)))
                (parse-list-block ar curr-level))
              li-list)))
    `(,(if ul? :ul :ol) ,@(nreverse li-list))))

(defmethod parse-statement ((ar ahead-reader))
  (cond ((char= (get-next (read-blank ar)) #\#) (parse-header ar))
        ((char= (get-next ar) #\>) (parse-quote-block ar))
        ((preformatted-text? ar) (parse-preformatted_text_block ar))
        ((table-separator? ar) (parse-table ar))
        ((list-line? ar) (parse-list-block ar))
        (t (parse-paragraph ar))))

(defmethod parse-title ((ar ahead-reader))
  (get-line ar))

(defmethod parse-xmarkdown((ar ahead-reader))
  (let ((title (parse-title (read-blank ar)))
        (body nil))
    (while (not (reach-eof? (read-blank ar)))
      (push (parse-statement ar) body))
    `((:!DOCTYPE "html")
      (:html ((lang "ja"))
        (:head 
          (:meta ((charset "utf-8")))
          (:link ((type "text/css") (rel "stylesheet") (href "./default.css")))
          (:title ,title))
        (:body
          ,(parse-outline *outline*)
          ,@(nreverse body))))))

(defun read-markdown (stream)
  (with-ahead-reader (ar stream)
    (parse-xmarkdown ar)))

(setq *with-format* t)

(defparameter usage
  (usage :title "mdrd FILE"
         :desc '("Output html from markdown")))

(with-open-file (out "test.html" :direction :output :if-exists :supersede)
  (princ
    (princ
      (with-open-file (in "readme.xmd" :direction :input)
        (DSL->xml (mapcar #'eval (read-markdown in))))
      out)))
