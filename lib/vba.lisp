;; vba utility

(provide :vba)

(defparameter +last-sheet+ nil)
(defparameter +active-sheet+ nil)
(defparameter +last-sheet+ nil)

(defclass VBARange ()
  ((expr :accessor .range)))

(defclass VBACell (VBARange)
  ((row :accessor .row)
   (col :accessor .col)))

(defclass VBASheet ()
  ((name :accessor .name)))

(defclass VBALastSheet (VBASheet) ())
(defclass VBAActiveSheet (VBASheet) ())

; cell (0 origin)
(defmethod .init ((c VBACell) &key row col)
  (setf (.row c) row
        (.col c) col)
  c)

(defmethod .map ((c VBACell))
  (format nil "Cells(~A, ~A)" (1+ (.row c)) (1+ (.col c))))    ; 1 origin

; range
(defmethod .init ((r VBARange) &key expr)
  (setf (.range r) expr)
  r)

(defmethod .map ((r VBARange))
  (format nil "Range(~A)" (.range r)))

(defmethod .put ((s VBASheet) (r VBARange) val)
  (format t "~A.~A.Value = \"~A\"~%" (.map s) (.map r) val))

; sheet
; note: to get excel worksheets, write bellow command in 'imidiate windows' and press enter.
;     For Each i In ThisWorkbook.Sheets: debug.print i.name : next i
(defmethod .init ((s VBASheet) &key name)
  (setf (.name s) name)
  s)

(defmethod .map ((s VBASheet))
  (format nil "Worksheets(\"~A\")" (.name s)))

(defmethod .map ((s VBALastSheet))
  (format nil "Worksheets(Worksheets.Count)"))

(defmethod .map ((s VBAActiveSheet))
  (.name s))

(defmethod .select-range ((s VBASheet) (r VBARange))
  (format t "~A.Activate~%~1:*~A.~A.Select~%" (.map s) (.map r)))

(defmethod .select-A1 ((s VBASheet))
  (.select-range s (cell 1 1)))

(defmethod .add-last ((s VBASheet))
  (format t "Worksheets.Add after:=~A~%" (.map +last-sheet+))
  (format t "~A.Name = \"~A\"~%" (.map +active-sheet+) (.name s)))

(defmethod .copy-to-last ((from VBASheet) (to VBASheet))
  (format t "~A.Copy after:=~A~%" (.map from) (.map +last-sheet+))
  (format t "~A.Name = \"~A\"~%" (.map +last-sheet+) (.name to)))

; utility
(defun cell (row col)
  (.init (make-instance 'VBACell) :row row :col col))

(defun range (expr)
  (.init (make-instance 'VBARange) :expr expr))

(defun sheet (name)
  (.init (make-instance 'VBASheet) :name name))

(setf +last-sheet+ (.init (make-instance 'VBALastSheet) :name "LastSheet")
      +active-sheet+ (.init (make-instance 'VBAActiveSheet) :name "ActiveSheet"))

; enter utility
(defmacro with-vba-main (&rest body)
  `(progn (format t "Option Explicit~%")
          (format t "Sub main()~%")
          (format t "Application.ScreenUpdating = False~%")
          (format t "Application.DisplayAlerts = False~%")
          ,@body
          (format t "Application.DisplayAlerts = True~%")
          (format t "Application.ScreenUpdating = True~%")
          (format t "End Sub~%")))
