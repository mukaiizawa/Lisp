
(load "../../lib/stdlib")

;; exeのあるフォルダ内のdatファイルが対象
(defexe fmerge (--help)
  "help"
  (let ((usage (usage :title "fmerge [OPTION]"
                      :desc  "Merge file utility."
                      :opts  '())))
    (when (or --help errors)
      (echo (pop errors))
      (funcall usage))
    (echo "start")
    (mapfile (lambda (pathname)
               (let* ((file-from (file-namestring pathname))
                      (file-to (before #\_ file-from)))
                 (echo "  merge: " file-from " => " file-to)
                 (write-to (read-from pathname) file-to
                           :enc :cp932
                           :ff :windows)))
             :extension 'dat)
    (echo "finished.")))


