
(load "../lib/stdlib")
(load "to-html")



(defexe to-html (--help -r)
  "r;help"
  (with-encoding (:utf-8 :unix)
    (let ((usage (usage :title "to-html [OPTION}"
                        :desc  "Convert from lisp to html."
                        :opts  '("-r" "only output tabs file"))))
      (when (or --help
                errors)
        (funcall usage))
      (mapfile (lambda (pathname)
                 (with-open-file (in pathname :direction :input)
                   (with-open-file (out (mkstr (pathname-name pathname) ".html")
                                        :direction :output
                                        :if-exists :supersede)
                     (with-html-output (out)
                       (awhile (read in nil nil)
                         (unless (eq 'load (first it))
                           (eval it)))))))
               :recursive -r
               :extension 'lisp))))



