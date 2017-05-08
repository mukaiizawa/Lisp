(require :ahead-reader *module-ahead-reader*)
(provide :csv-reader)

(defstruct (csv-reader (:include ahead-reader))
  (wrapper-character #\" :type character)
  (segment-character #\, :type character))

(defmacro with-csv-reader
  ((reader &optional (stream *standard-input*) (segment-character #\,) (wrapper-character #\")) &body body)
  `(let* ((,reader
            (make-csv-reader :stream ,stream :segment-character ,segment-character :wrapper-character ,wrapper-character)))
     ,@body))

(defmethod read-csv-line ((reader csv-reader))
  (let ((fields nil))
    (while (not (reader-next-in? reader +null-character+ #\Newline))
      (push
        (get-buf (if (not (reader-next-in? reader (csv-reader-wrapper-character reader)))
                   (read-if (lambda (c)
                              (not (find c (list #\Newline (csv-reader-segment-character reader)))))
                            reader)
                   (progn
                     (read-next reader :cache nil)    ; skip open quote
                     (while (not (and (char= (get-next reader) (csv-reader-wrapper-character reader))
                                      (char/= (get-next reader 2) (csv-reader-wrapper-character reader))))
                       (when (reader-next-in? reader (csv-reader-wrapper-character reader))
                         (read-next reader :cache nil))    ; read `""' as `"'
                       (read-next reader))
                     (read-next reader :cache nil))))    ; skip close quote
        fields)
      (when (reader-next-in? reader #\,)    ; skip `,'
        (read-next reader :cache nil)))
    (nreverse fields)))

(defmethod read-csv-lines ((reader csv-reader))
  (let ((lines nil))
    (while (not (reach-eof? reader))
      (push (read-csv-line reader) lines))
    (nreverse lines)))
