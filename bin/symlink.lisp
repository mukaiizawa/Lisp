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
                 (let ((dest (mkstr "\"C:\\xbin\\" (file-namestring pathname) "\"")))
                   (format out "DEL ~A 2> nul~%MKLINK ~1:*~A \"~A\"~%"
                           dest pathname)))
               :recursive t :directory "./" :extension 'exe)
      (format out "pause~%"))))

(symlink)
