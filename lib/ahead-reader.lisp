(require :stdlib *module-stdlib*)
(provide :ahead-reader)

(defstruct ahead-reader
  (stream nil :type stream)
  (buf nil :type list)
  (linecount 0 :type number)
  (escape-character #\\ :type character)
  (escape-mapper (lambda (c)
                   (case c
                     (#\n #\Newline)
                     (#\t #\Tab)
                     (t c))))
  (linecount 0 :type number)
  (curr +null-character+ :type character)
  (stack nil :type list))

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
         (when (open-stream-p ,stream)
           (close ,stream))))))

;; get-linecount {{{

(defmethod get-linecount ((reader ahead-reader))
  (1+ (ahead-reader-linecount reader)))

;; }}}
;; get-curr {{{

(defmethod get-curr ((reader ahead-reader))
  (ahead-reader-curr reader))

;; }}}
;; get-next {{{

(defmethod get-next ((reader ahead-reader) &optional (n 1))
  (if (not (plusp n))
    (error "get-next: Illegal value `~A'. Optional parameter must be positive integer." n)
    (let* ((stack-size (length (ahead-reader-stack reader)))
           (has-stack? (plusp stack-size)))
      (cond ((and (not has-stack?)
                  (= n 1))
             (peek-char nil (ahead-reader-stream reader) nil +null-character+))
            ((and has-stack?
                  (<= n stack-size))
             (nth (1- n) (ahead-reader-stack reader)))
            (t
              (do* ((acc (reverse (ahead-reader-stack reader)))
                    (cnt 0 (1+ cnt)))
                ((or (>= cnt (- n stack-size))
                     (char= (peek-char nil (ahead-reader-stream reader) nil +null-character+)
                            +null-character+))
                 (progn (setf (ahead-reader-stack reader) (nreverse acc))
                        (if (>= cnt (- n stack-size))
                          (first acc)
                          +null-character+)))
                (push (read-ahead reader) acc)))))))

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
;; read-ahead {{{

(defmethod read-ahead ((reader ahead-reader))
  (if (reach-eof? reader)
    (error "read-ahead: already reach eof. contents of buffer `~A'" (refer-buf reader))
    (let ((c (read-char (ahead-reader-stream reader) nil +null-character+)))
      (when (char= c #\Newline)
        (incf (ahead-reader-linecount reader)))
      (when (char= c (ahead-reader-escape-character reader))
        (setq c (funcall (ahead-reader-escape-mapper reader)
                         (read-char (ahead-reader-stream reader) nil +null-character+))))
      c)))

;; }}}
;; read-next {{{

(defmethod read-next ((reader ahead-reader) &key (cache t))
  (let ((c (if (null (ahead-reader-stack reader))
             (read-ahead reader)
             (pop (ahead-reader-stack reader)))))
    (when cache
      (add-char reader c))
    (setf (ahead-reader-curr reader) c))
  reader)

;; }}}
;; read-n-times {{{

(defmethod read-n-times ((reader ahead-reader) (n number) &key (cache t))
  (dotimes (i n)
    (read-next reader :cache cache))
  reader)

;; }}}
;; read-if {{{

(defmethod read-if ((fn function) (reader ahead-reader) &key (cache t))
  (while (and (not (reach-eof? reader))
              (funcall fn (get-next reader)))
    (read-next reader :cache cache))
  reader)

;; }}}
;; read-space {{{

(defmethod read-space ((reader ahead-reader) &key (cache t))
  (read-if (lambda (c)
             (char= c #\Space))
           reader :cache cache)
  reader)

;; }}}
;; read-number {{{

(defmethod read-number ((reader ahead-reader) &key (cache t))
  (read-if #'digit-char-p reader :cache cache)
  (when (char= (get-next reader) #\.)
    (read-if #'digit-char-p
             (read-next reader :cache cache)))
  reader)

;; }}}
;; read-paren {{{

(defmethod read-paren ((reader ahead-reader) &key (cache t))
  (let* ((left-paren (get-curr (read-next reader :cache nil)))
         (right-paren (case left-paren
                        (#\( #\))
                        (#\[ #\])
                        (#\{ #\})
                        (#\< #\>)
                        (t (error "ahead-reader.read-paren: unexpected left-paren `~A'"
                                  left-paren)))))
    (read-if (lambda (c)
               (char/= c right-paren))
             reader :cache cache))
  (read-next reader :cache nil))

;; }}}
;; read-segment {{{

(defmethod read-segment ((reader ahead-reader) &key (cache t))
  (let ((segment (get-curr (read-next reader :cache nil))))
    (read-next (read-if (lambda (c)
                          (char/= c segment))
                        reader :cache cache)
               :cache nil)))

;; }}}
;; add-char {{{

(defmethod add-char ((reader ahead-reader) (c character))
  (push c (ahead-reader-buf reader))
  reader)

;; }}}
;; append-str {{{

(defmethod append-str ((reader ahead-reader) (str string))
  (dostring (c str)
    (push c (ahead-reader-buf reader)))
  reader)

;; }}}
;; get-buf {{{

(defmethod get-buf ((reader ahead-reader))
  (prog1
    (coerce (nreverse (ahead-reader-buf reader)) 'string)
    (setf (ahead-reader-buf reader) nil)))

;; }}}
;; refer-buf {{{

(defmethod refer-buf ((reader ahead-reader))
  (coerce (reverse (ahead-reader-buf reader)) 'string))

;; }}}
;; set-escape-character {{{

(defmethod set-escape-character ((reader ahead-reader) (c character))
  (setf (ahead-reader-escape-character reader) c)
  reader)

;; }}}
;; set-escape-mapper {{{

(defmethod set-escape-mapper ((reader ahead-reader) (fn function))
  (setf (ahead-reader-escape-mapper reader) fn)
  reader)

;; }}}
