(require :vba *module-vba*)
(require :test-utils *module-test-utils*)

(defparameter sheet-names '("sheetA" "sheetB" "sheetC"))

(with-vba-main
  (dolist (sheet-name sheet-names)
    (let ((sheet (sheet sheet-name)))
      (.add-last sheet)
      (dotimes (i 3)
        (dotimes (j 3)
          (.put sheet (cell (1+ i) (1+ j)) (* i j)))))))
