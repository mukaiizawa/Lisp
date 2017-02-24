(require :stdlib *module-stdlib*)

(defvar *box-size* 3)
(defparameter *board-size* nil)
(defparameter *exit-flg* nil)

(defun main (board)
  (setq *board-size* (expt *box-size* 2))
  (setq *exit-flg* nil)
  (try-set-number board 0 0))

;; try set number on board at line `row' and column `col'
(defun try-set-number (board row col)
  (cond (*exit-flg*
          (return-from try-set-number 'solved))
        ((>= row *board-size*)
         (setq *exit-flg* t)
         (print-board board))
        (t
          (let* ((next-row (+ row (truncate (1+ col) *board-size*)))
                 (next-col (mod (1+ col) *board-size*)))
            (if (> (aref board row col) 0)
              (try-set-number board next-row next-col)    ; aleady put
              (dolist (num (to-randlist (iota 1 *board-size*)))
                (when (set-number? board row col num)
                  (try-set-number (set-number board row col num)
                                  next-row
                                  next-col))))))))

(defun to-randlist (lis)
  (sort lis #'< :key (lambda (x) (declare (ignore x)) (random 1.0 (make-random-state t)))))

(defun set-number (board row col num)
  (let ((new-board (make-array (list *board-size* *board-size*))))
    (dotimes (i *board-size*)
      (dotimes (j *board-size*)
        (setf (aref new-board i j)
              (aref board i j))))
    (setf (aref new-board row col) num)
    (values new-board)))

(defun set-number? (board row col num)
  ; check row and col
  (dotimes (i *board-size*)
    (when (or (= (aref board row i) num)
              (= (aref board i col) num))
      (return-from set-number? nil)))
  ; check box (3*3)
  (let ((offset-row (- row (mod row *box-size*)))
        (offset-col (- col (mod col *box-size*))))
    (dorange (i offset-row (+ offset-row *box-size* -1))
      (dorange (j offset-col (+ offset-col *box-size* -1))
        (when (= (aref board i j) num)
          (return-from set-number? nil)))))
  (values t))

(defun print-board (board)
  (let ((horizontal-bar (make-string (+ (* 3 *board-size*) (* *box-size* 2) 1)
                                     :initial-element #\-)))
    (format t "~%~A" horizontal-bar)
    (dotimes (i *board-size*)
      (format t "~%|")
      (dotimes (j *board-size*)
        (format t "~3@A" (aref board i j))
        (when (= (mod j *box-size*) (1- *box-size*))
          (format t " |")))
      (when (= (mod i *box-size*) (1- *box-size*))
        (format t "~%~A" horizontal-bar)))))

(defun mkarr (lis)
  (let ((row-size (length lis))
        (col-size (length (first lis))))
    (make-array (list row-size col-size)
                :initial-contents 
                lis)))


;; create new puzle
(main (make-array '(9 9) :initial-element -1))

;; solve puzle
(setq *box-size* 3)
(main (mkarr 
        '((9  1  7  2  8  4  3  5 -6)
          (8 -2  6 -9 -5  3  4  1 -7)
          (4 -5  3  1 -7  6  8  9 -2)
          (7  9  8 -6 -1  5  2 -4 -3)
          (6  3  2  4 -9  7  1  8 -5)
          (5 -4 -1 -3 -2  8  6  7 -9)
          (2  6  9  7  4  1  5 -3 -8)
          (1  7  5  8  3  2  9  6 -4)
          (3  8  4  5  6  9  7 -2 -1))))
