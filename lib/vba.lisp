;; vba utility

(provide :vba)

(defparameter +last-sheet+ nil)
(defparameter +active-sheet+ nil)
(defparameter +last-sheet+ nil)

(defparameter *sym-index* 0)
(defparameter *subroutine-index* 0)

(defclass VBARange ()
  ((expr :accessor .range)))

(defclass VBACell (VBARange)
  ((row :accessor .row)
   (col :accessor .col)))

(defclass VBASheet ()
  ((name :accessor .name)))

(defclass VBALastSheet (VBASheet) ())
(defclass VBAActiveSheet (VBASheet) ())

(defun gen-vba-sym ()
  (with-output-to-string (out)
    (princ "G" out) (princ (incf *sym-index*) out)))

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
  (format nil "Range(\"~A\")" (.range r)))

(defmethod .put ((s VBASheet) (r VBARange) val)
  (let ((val (if (listp val) val (list val))))
    (format t "~A.~A.Value = \"~A\"~%" (.map s) (.map r) (car val))
    (dolist (x (cdr val))
      (format t "~A.~A.Value = ~A.~A.Value & vbLf & \"~A\"~%"
              (.map s) (.map r) (.map s) (.map r) x))))

(defmethod .copy ((s1 VBASheet) (r1 VBARange) (s2 VBASheet) (r2 VBARange))
  (format t "~A.~A.Copy~%" (.map s1) (.map r1))
  (format t "~A.~A.Select~%" (.map s2) (.map r2))
  (format t "~A.Paste~%" (.map +active-sheet+)))

(defmethod .border ((s VBASheet) (r VBARange))
  (format t "~A.~A.Borders.LineStyle = True~%" (.map s) (.map r)))

(defmethod .print-area ((s VBASheet) (r VBARange))
  (format t "~A.PageSetup.PrintArea = ~A.Address~%" (.map s) (.map r)))

(defmethod .auto-fit-row ((s VBASheet) (r VBARange))
  (format t "~A.~A.EntireRow.AutoFit~%" (.map s) (.map r)))

(defmethod .wrap-text ((s VBASheet) (r VBARange))
  (format t "~A.~A.WrapText = True~%" (.map s) (.map r)))

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

(defmethod .remove-if-exist ((s VBASheet))
  (let ((sym (gen-vba-sym)))
    (format t (concatenate
                'string
                "Dim ~A~%"
                "For Each ~A In Worksheets~%"
                "  If ~A.Name = \"~A\" Then~%"
                "    ~A.Delete~%"
                "    Exit For~%"
                "  End If~%"
                "Next~%")
            sym sym sym (.name s) sym)))

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
(defmacro with-subroutine (&rest body)
  `(progn (format t "Sub sub_~A()~%" (incf *subroutine-index*))
          ,@body
          (format t "End Sub~%")))

(defmacro with-vba (&rest body)
  `(progn (format t "Option Explicit~%")
          ,@body
          (format t "Sub main()~%")
          (format t "Application.ScreenUpdating = False~%")
          (format t "Application.DisplayAlerts = False~%")
          (dotimes (i *subroutine-index*)
            (format t "Call sub_~A~%" (1+ i)))
          (format t "Application.DisplayAlerts = True~%")
          (format t "Application.ScreenUpdating = True~%")
          (format t "End Sub~%")))
