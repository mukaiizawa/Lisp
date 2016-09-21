
(require "ahead-reader" *module-ahead-reader*)
(provide "csv-reader")

(defstruct (csv-reader (:include ahead-reader))
  (lines nil :type list)
  (wrap? t :type boolean)
  (wrapper #\" :type character)
  (segment-character #\, :type character))

(defstruct csv-writer
  (lines nil :type list)
  (wrap? t :type boolean)
  (wrapper #\")
  (segment-character #\, :type character))

;; todo
(defmacro with-csv-reader ((reader &optional (stream *standard-input*) (segment-character #\,) &body body)
  `(let* ((,reader (make-csv-reader :stream ,stream :)))
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
