; n queen problem

(defparameter *board-size* 8)
(defparameter *board* (make-array (list *board-size*  *board-size*)
                                  :initial-element nil))

(defun print-board ()
  (dotimes (row *board-size*)
    (dotimes (col *board-size*)
      (princ (if (aref *board* row col) " Q" " -")))
    (fresh-line))
  (princ #\newline))

(defun in-board? (row col)
  (and (<= 0 row (1- *board-size*)) (<= 0 col (1- *board-size*))))

(defun putable? (row col)
  (dotimes (row row)
    (if (aref *board* row col) (return-from putable? nil)))
  (do ((row row (1- row)) (col col (1- col)))
    ((not (in-board? row col)))
    (if (aref *board* row col) (return-from putable? nil)))
  (do ((row row (1- row)) (col col (1+ col)))
    ((not (in-board? row col)))
    (if (aref *board* row col) (return-from putable? nil)))
  t)

(defun put-queen (row)
  (if (= row *board-size*)
    (print-board)
    (dotimes (col *board-size*)
      (when (putable? row col)
        (setf (aref *board* row col) t)
        (put-queen (1+ row))
        (setf (aref *board* row col) nil)))))

(defun main ()
  (put-queen 0))

(main)
