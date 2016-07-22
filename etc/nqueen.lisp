
(load "../lib/stdlib")

(defvar *board-size* 8)
(defvar *queen* 1)
(defvar *solution-num* 0)

(defun main ()
  (try-put-queen (mkboard) 0))

(defun mkboard ()
  (make-array (list *board-size* *board-size*) :initial-element 0))


;; tryPut is the function that try put Queen line `row' on a `board'.
(defun try-put-queen (board row)
  (cond ((= row *board-size*)
         (incf *solution-num*)
         (print-board board))    ; finish
        (t
          (dorange (col 0 (1- *board-size*))
            (when (put-queen? board row col)
              (try-put-queen (put-queen board row col)
                             (1+ row)))))))

(defmacro queen? (board row col)
  `(and (<= 0 ,row) (< ,row *board-size*)
        (<= 0 ,col) (< ,col *board-size*)
        (= (aref ,board ,row ,col) *queen*)))

(defun put-queen? (board row col)
  (do ((up (1- row) (1- up))
       (right (1+ col) (1+ right))
       (left (1- col) (1- left)))
    ((and (< (max up left) 0)
          (>= right *board-size*)) t)
    (when (or (queen? board up col)
              (queen? board up right)
              (queen? board up left))
      (return-from put-queen? nil))))

(defun put-queen (board row col)
  (let ((new-board (mkboard)))
    (dotimes (i *board-size*)
      (dotimes (j *board-size*)
        (setf (aref new-board i j)
              (aref board i j))))
    (setf (aref new-board row col) *queen*)
    (values new-board)))

(defun print-board (board)
  (format t "~%~A~%~A " *solution-num* (make-string (* *board-size* 2) :initial-element #\_))
  (dotimes (i *board-size*)
    (format t "~%")
    (format t "|")
    (dotimes (j *board-size*)
      (format t "~A|" (aif (= 0 (aref board i j)) #\_ #\Q))))
  (fresh-line))

(main)


