;; vba utility

(require :stdlib *module-stdlib*)
(provide :vba)

(defmacro with-vba-main (&rest body)
  `(progn (format t "sub main()~%")
          ,@body
          (format t "end sub~%")))

(defmacro with-new-sheet ((ws) &rest body)
  `(progn (format t "Worksheets.Add after:=Worksheets(Worksheets.Count)~%")
          (format t "ActiveSheet.Name = \"~A\"~%" ,ws)
          ,@body))

(defmacro put-cell (ws range val)
  `(format t "Worksheets(\"~A\").Range(\"~A\").Value = \"~A\"~%"
           ,ws ,range ,val)) 
