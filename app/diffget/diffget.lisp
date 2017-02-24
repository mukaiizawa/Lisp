(require :stdlib *module-stdlib*)

; 元になるファイルと、それと比較するファイルを用意する。
; 元になるファイル群                              ：DIR1
; 比較するファイル群                              ：DIR2
; 元のファイルにあり、
; 比較するファイルにない行を吐き出したファイル群  ：DIR1-DIR2

(defexe diffget  (-e --help)
  "e:help"
  (echo "diffget start")
  (let* ((usage (usage :title "diffget [DIR1] [DIR2] [OPTION]..."
                       :desc  "Get different line with the same name file of [DIR1] and [DIR2]."
                       :opts  '("-e" "extension of terget file (default value `dat'")))
         (origin-dir (first args))
         (test-dir (second args))
         (result-dir (mkstr origin-dir "-" test-dir)))
    (when (or --help
              errors
              (null origin-dir)
              (null test-dir))
      (funcall usage))
    (with-encoding (:cp932 :windows)
      (mapfile (lambda (pathname)
                 (let* ((fname (file-namestring pathname))
                        (ori-path (mkstr origin-dir "/" fname))
                        (test-path (mkstr test-dir "/" fname))
                        (result-path (mkstr result-dir "/" fname)))
                   (echo "  start" fname)
                   (when (file-exists? test-path)
                     (write-to! (nset-difference
                                  (read-from ori-path)
                                  (read-from test-path)
                                  :test #'string=)
                                result-path)
                     (echo "  finish" fname))))
               :directory origin-dir
               :extension (or -e 'dat))))
  (echo "finished"))
