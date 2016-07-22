
(load "../lib/stdlib" :if-does-not-exist nil)

(defparameter *board* nil)
(defparameter *default-board* nil)
(defparameter *box-row-size* nil)
(defparameter *box-col-size* nil)
(defparameter *board-size* nil)
(defparameter *candidate*  nil)

(eval-when (:compile-toplevel)
  (proclaim '(optimize speed)))

(defun main ()
  (declaim (optimize (speed 3) (debug 0) (safety 0)))
  (setq *board-size* (* *box-row-size* *box-col-size*)
        *default-board* (copy-board)
        *candidate* (iota 1 *board-size*))
  (try-set-number 0 0 *candidate*))

;; try set number on board at line `row' and column `col'
(defun try-set-number (row col candidate)
  (declaim (optimize (speed 3) (debug 0) (safety 0)))
  (if (null candidate)
    (progn (unless (default-value? row col)
             (setf (aref *board* row col) 0))
           (return-from try-set-number nil))
    (cond ((>= row *board-size*)
           (print-board)
           (return-from try-set-number t))
          ((default-value? row col)
           (try-set-number (next-row row col) (next-col col) *candidate*))
          (t
            (or 
              (set-number row col (first candidate))
              (try-set-number row col (rest candidate)))))))

(defun default-value? (row col)
  (declaim (optimize (speed 3) (debug 0) (safety 0)))
  (plusp (aref *default-board* row col)))

(defun next-row (row col)
  (declaim (optimize (speed 3) (debug 0) (safety 0)))
  (+ row (truncate (1+ col) *board-size*)))

(defun next-col (col)
  (declaim (optimize (speed 3) (debug 0) (safety 0)))
  (mod (1+ col) *board-size*))

(defun set-number (row col num)
  (declaim (optimize (speed 3) (debug 0) (safety 0)))
  ; check row and col
  (dotimes (i *board-size*)
    (when (or (= (aref *board* row i) num)
              (= (aref *board* i col) num))
      (return-from set-number nil)))
  ; check box
  (let ((offset-row (- row (mod row *box-row-size*)))
        (offset-col (- col (mod col *box-col-size*))))
    (dorange (i offset-row (+ offset-row *box-row-size* -1))
      (dorange (j offset-col (+ offset-col *box-col-size* -1))
        (when (= (aref *board* i j) num)
          (return-from set-number nil)))))
  (setf (aref *board* row col) num)    ; update board
  (try-set-number (next-row row col) (next-col col) *candidate*))

(defun print-board ()
  (declaim (optimize (speed 3) (debug 0) (safety 0)))
  (let ((horizontal-bar (make-string (+ (* 3 *board-size*) (* *box-row-size* *box-col-size*) 1)
                                     :initial-element #\-)))
    (format t "~%~A" horizontal-bar)
    (dotimes (i *board-size*)
      (format t "~%|")
      (dotimes (j *board-size*)
        (format t "~3@A" (aref *board* i j))
        (when (= (mod j *box-col-size*) (1- *box-col-size*))
          (format t " |")))
      (when (= (mod i *box-row-size*) (1- *box-row-size*))
        (format t "~%~A" horizontal-bar)))))

(defun mkboard (lis)
  (declaim (optimize (speed 3) (debug 0) (safety 0)))
  (let ((row-size (length lis))
        (col-size (length (first lis))))
    (make-array (list row-size col-size)
                :initial-contents 
                lis)))

(defun copy-board ()
  (declaim (optimize (speed 3) (debug 0) (safety 0)))
  (let ((new-board (make-array (list *board-size* *board-size*))))
    (dotimes (i *board-size*)
      (dotimes (j *board-size*)
        (setf (aref new-board i j) (aref *board* i j))))
    (values new-board)))


;; solve puzle
(setq *box-row-size* 3
      *box-col-size* 3
      *board* (mkboard
                '((9  1  7  2  8  4  3  5 -6)
                  (8 -2  6 -9 -5  3  4  1 -7)
                  (4 -5  3  1 -7  6  8  9 -2)
                  (7  9  8 -6 -1  5  2 -4 -3)
                  (6  3  2  4 -9  7  1  8 -5)
                  (5 -4 -1 -3 -2  8  6  7 -9)
                  (2  6  9  7  4  1  5 -3 -8)
                  (1  7  5  8  3  2  9  6 -4)
                  (3  8  4  5  6  9  7 -2 -1))))
(main)




