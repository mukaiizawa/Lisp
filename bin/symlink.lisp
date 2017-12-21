#|
 | windows symbolic link maker
 |#

(require :stdlib *module-stdlib*)

(defvar *batch-file* "./symlink.bat")

(defun symlink ()
  (with-encoding (:cp932 :windows)
    (with-open-file (out *batch-file* :direction :output :if-exists :supersede)
      (princln "@ECHO off" out)
      (princln "REM following command generated automatically" out)
      (mapfile (lambda (pathname)
                 (format out "DEL ~A 2> nul~%MKLINK ~1:*~A \"~A\"~%"
                         (mkstr "\"C:\\xbin\\" (file-namestring pathname) "\"")
                         pathname))
               :recursive t :directory "./" :extension 'exe)
      (format out "pause~%"))))

(symlink)
