(load "../lib/stdlib")

(defun map-char (str index)
  (char str (1- index)))

(defun get-index (from c allbut? lastto)
  (labels ((normal-case ()
                        (aif (position c from)
                          (1+ it)    ; index
                          0))    ; not found
           (allbut-case ()
                        (cond ((eq c 'EOF) 0)
                              ((not allbut?) (normal-case))
                              ((> (normal-case) 0) 0)    ; allbut? is true and found with index => return not-found.
                              (t (1+ lastto)))))    ; allbut? is true and collapse.
    (allbut-case)))

(defun translit-main (from to lastto)
  (let* ((i)
         (allbut? (char= #\! (char from 0)))
         ;; when `allbut?' is true, remove a first `!' from the `from'.
         (from (if allbut? (subseq from 1) from))
         (collapse? (or allbut?
                        (> (length from) lastto))))
    (do ((c (read-char *standard-input* nil 'EOF)
            (read-char *standard-input* nil 'EOF)))
      ((eq c 'EOF))
      (setq i (get-index from c allbut? lastto))
      ;; collapse
      (when (and collapse?
                 (>= i lastto)
                 (> lastto 0))
        (write-char (map-char to lastto))
        (do ()
          ((or (eq c 'EOF) (< i lastto)))
          (setq c (read-char *standard-input* nil 'EOF)
                i (get-index from c allbut? lastto))))
      (unless (eq c 'EOF)
        (cond
          ;; translate
          ((and (> i 0)
                (> lastto 0))
           (write-char (map-char to i)))
          ;; copy
          ((= i 0)
           (write-char c))
          ;; delete
          (t
            'do-nothing))))))

(defexe translit (--help)
  "help"
  (let* ((usage (usage :title "translit [OPTION] FROM [TO]"
                       :desc  '("Translate, squeeze, and/or delete characters from standard input,"
                                "writing to standard output.")))
         (from (expand-hyphen (first args)))
         (to (expand-hyphen (second args)))
         (lastto (length to)))
    (cond ((or --help
               errors
               (empty? from))
           (funcall usage))
          ((< (length from) lastto)
           (format *error-output* "translit: `~A' too large~%" from)
           (funcall usage))
          (t
            (translit-main from to lastto)))))
