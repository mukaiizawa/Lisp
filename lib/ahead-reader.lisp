
(require "stdlib" *module-stdlib*)
(provide "ahead-reader")

(defconstant +null-character+ (code-char 0))
(defconstant +escape-character+ #\\)

(defstruct ahead-reader
  (stream nil :type stream)
  (buf nil)
  (curr +null-character+ :type character))

(defmacro with-ahead-reader ((reader &optional (stream *standard-input*)) &body body)
  `(let* ((,reader (make-ahead-reader :stream ,stream)))
     ,@body))

(defmacro with-string-ahead-reader ((reader str) &body body)
  (with-gensyms (in)
    `(with-input-from-string (,in ,str)
       (with-ahead-reader (,reader ,in)
         ,@body))))

(defmacro with-open-ahead-reader ((reader pathname &key (if-does-not-exist :error)) &body body)
  (with-gensyms (stream)
    `(let (,stream)
       (unwind-protect
         (progn 
           (setq ,stream (open ,pathname :direction :input :if-does-not-exist ,if-does-not-exist))
           (with-ahead-reader (,reader ,stream)
             ,@body))
         (when (open-stream? ,stream)
           (close ,stream))))))

;; get-curr {{{

(defmethod get-curr ((reader ahead-reader))
  (ahead-reader-curr reader))

;; }}}
;; get-next {{{

(defmethod get-next ((reader ahead-reader))
  (peek-char nil (ahead-reader-stream reader) nil +null-character+))

;; }}}
;; reader-curr-in? {{{

(defmethod reader-curr-in? ((reader ahead-reader) &rest args)
  (find (ahead-reader-curr reader) args))

;; }}}
;; reader-next-in? {{{

(defmethod reader-next-in? ((reader ahead-reader) &rest args)
  (find (get-next reader) args))

;; }}}
;; reach-eof? {{{

(defmethod reach-eof? ((reader ahead-reader))
  (eq (get-next reader) +null-character+))

;; }}}
;; read-next {{{

(defmethod read-next ((reader ahead-reader) &key (cache t))
  (let ((c (read-char (ahead-reader-stream reader) nil +null-character+)))
    (when (char= c #\\)
      (let ((escape-sequence (read-char (ahead-reader-stream reader) nil +null-character+)))
        (setq c (case escape-sequence
                  (#\n #\Newline)
                  (#\t #\Tab)
                  (t escape-sequence)))))
    (setf (ahead-reader-curr reader) c)
    (when cache (add-char reader c)))
  (values reader))

;; }}}
;; read-if {{{

(defmethod read-if ((fn function) (reader ahead-reader) &key (cache t))
  (while (and (not (reach-eof? reader))
              (funcall fn (get-next reader)))
    (read-next reader :cache cache))
  (values reader))

;; }}}
;; read-space {{{

(defmethod read-space ((reader ahead-reader) &key (cache t))
  (read-if (lambda (c)
             (char= c #\Space))
           reader :cache t)
  (values reader))

;; }}}
;; read-number {{{

(defmethod read-number ((reader ahead-reader))
  (read-if #'digit-char-p reader :cache t)
  (when (char= (get-next reader) #\.)
    (read-next reader :cache t)
    (read-if #'digit-char-p reader :cache t))
  (values reader))

;; }}}
;; read-paren {{{

(defmethod read-paren ((reader ahead-reader) &key (cache t))
  (let* ((left-paren (get-curr (read-next reader :cache nil)))
         (right-paren (case left-paren
                        (#\( #\))
                        (#\[ #\])
                        (#\{ #\})
                        (#\< #\>)
                        (t (error "ahead-reader.read-paren: unexpected left-paren `~A'" left-paren)))))
    (read-if (lambda (c)
               (char/= c right-paren))
             reader
             :cache cache))
  (values (read-next reader :cache nil)))

;; }}}
;; add-char {{{

(defmethod add-char ((reader ahead-reader) (c character))
  (push c (ahead-reader-buf reader))
  (values reader))

;; }}}
;; append-str {{{

(defmethod append-str ((reader ahead-reader) (str string))
  (dostring (c str)
    (push c (ahead-reader-buf reader)))
  (values reader))

;; }}}
;; get-buf {{{

(defmethod get-buf ((reader ahead-reader))
  (prog1
    (coerce (nreverse (ahead-reader-buf reader)) 'string)
    (setf (ahead-reader-buf reader) nil)))

;; }}}

