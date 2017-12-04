; knight tour problem.

(defparameter *size* 5)

(defclass KnightTour ()
  ((size :accessor .size)
   (board :accessor .board)
   (visit-count :accessor .visit-count)
   (curr-row :accessor .curr-row)
   (curr-col :accessor .curr-col)))

(defmethod .init ((kt KnightTour) size row col)
  (setf (.size kt) size
        (.curr-row kt) row
        (.curr-col kt) col
        (.board kt) (make-array (list (.size kt) (.size kt))
                                :initial-element nil)
        (.visit-count kt) 0
        (aref (.board kt) row col) (.visit-count kt))
  kt)

(defmethod .in-board? ((kt KnightTour) row col)
  (every (lambda (x) (<= 0 x (1- (.size kt)))) (list row col)))

(defmethod .doboard ((kt KnightTour) fn)
  (do ((row 0 (1+ row))) ((>= row (.size kt)))
    (do ((col 0 (1+ col))) ((>= col (.size kt)))
      (funcall fn row col))))

(defmethod .finish? ((kt KnightTour))
  (= (1+ (.visit-count kt)) (expt (.size kt) 2)))

(defmethod .print-board ((kt KnightTour))
  (princ #\Newline)
  (.doboard kt (lambda (row col)
                 (if (= (mod col (.size kt)) 0) (fresh-line)) 
                 (format t "~3A" (aref (.board kt) row col)))))

(defmethod .back ((kt KnightTour))
  (decf (.visit-count kt))
  (setf (aref (.board kt) (.curr-row kt) (.curr-col kt)) nil)
  (.doboard kt (lambda (row col)
                 (when (equal (aref (.board kt) row col) (.visit-count kt))
                   (setf (.curr-row kt) row (.curr-col kt) col)))))

(defmethod .movable? ((kt KnightTour) row col)
  (and (.in-board? kt row col) (not (aref (.board  kt) row col))))

(defmethod .move ((kt KnightTour) row col)
  (incf (.visit-count kt))
  (setf (.curr-row kt) row
        (.curr-col kt) col
        (aref (.board kt) row col) (.visit-count kt))
  (.try-move kt))

(defun combination (&rest args)
  (if (null (car args))
    '(())
    (mapcan (lambda (x)
              (mapcar (lambda (y) (cons y x))
                      (car args)))
            (apply #'combination (cdr args)))))

(defmethod .try-move ((kt KnightTour))
  (macrolet
    ((expand-candidate ()
      (let ((sign '(+ -))
            (pos '(1 2)))
        `(progn
           ,@(mapcar (lambda (args)
                       `(let ((row (funcall ',(cadr args) (.curr-row kt) 
                                            ,(caar args)))
                              (col (funcall ',(caddr args) (.curr-col kt)
                                            ,(cadar args))))
                          (if (.movable? kt row col) (.move kt row col))))
                     (combination (list pos (reverse pos)) sign sign))))))
    (if (.finish? kt)
      (.print-board kt)
      (expand-candidate))
    (.back kt)))

(defun main ()
  (let ((kt (make-instance 'KnightTour)))
    (do ((row 0 (1+ row))) ((>= row (ash *size* -1)))
      (do ((col 0 (1+ col))) ((>= col (ash *size* -1)))
        (.try-move (.init kt *size* row col))))))

(main)
