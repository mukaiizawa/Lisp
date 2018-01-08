;; vba utility

(require :stdlib *module-stdlib*)
(provide :vba)

(defun refer-sheet (ws)
  (format nil "Worksheets(\"~A\")" ws))

(defmacro with-vba-main (&rest body)
  `(progn (format t "sub main()~%")
          ,@body
          (format t "end sub~%")))

(defmacro with-new-sheet ((ws) &rest body)
  `(progn (format t "Worksheets.Add after:=Worksheets(Worksheets.Count)~%")
          (format t "ActiveSheet.Name = \"~A\"~%" ,ws)
          ,@body))

(defmacro put-cell (ws range val)
  `(format t "~A.Range(\"~A\").Value = \"~A\"~%"
           (refer-sheet ,ws) ,range ,val)) 

(defmacro select-cell (ws range)
  `(let ((ws (refer-sheet ,ws)))
     (format t "~A.Activate~%~A.Range(\"~A\").Select~%" ws ws ,range)))

(defmacro select-a1-cell (ws)
  `(select-cell ,ws "A1"))
