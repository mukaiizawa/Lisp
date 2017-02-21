(require :stdlib *module-stdlib*)

(let ((index 0))
  (defun reset-index ()
    (setq index 0))
  (defun output (pathname lis)
    (when lis
      (let ((fname (mkstr (file-namestring pathname) "_" (mkindex (incf index)) ".dat")))
        (echo "  output: " fname)
        (write-to lis fname)))))

(defun mkindex (num)
  (cond ((< num 10)
         (mkstr 0 0 num))
        ((< num 100)
         (mkstr 0 num))
        (t num)))

(defexe diverge ((-n --split-num) (-e --extension) -r --help)
  "n:split-num:e:extension:r;help"
  (let ((usage (usage :title "diverge [OPTION}..."
                      :desc  "Split file utility."
                      :opts '("-n, --split-num=" "number of split line"
                              "-e, --extension=" "target file extension. default 'csv'"
                              "-r" "split file recursive")))
        (split-line-num (aif (or -n --split-num) (parse-integer it) 10000))
        (extension (aif (or -e --extension) (intern it) 'csv)))
    (when (or --help errors)
      (funcall usage))
    (mapfile (lambda (pathname)
               (echo "start: " pathname)
               (reset-index)
               (with-encoding (guess-encoding pathname)
                 (with-open-file (in pathname :direction :input :if-does-not-exist nil)
                   (let ((output-lines nil))
                     (do ((line (read-line in nil nil) (read-line in nil nil))
                          (line-num 0 (1+ line-num)))
                       ((null line))
                       (push line output-lines)
                       (when (= (mod (1+ line-num) split-line-num) 0)
                         (output pathname (nreverse output-lines))
                         (setq output-lines nil)))
                     (output pathname (nreverse output-lines))
                     (echo "end: " pathname)))))
             :extension extension
             :recursive -r))
  (echo "finished."))
