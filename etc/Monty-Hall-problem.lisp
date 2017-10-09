#|
 | montie hole problem.
 |#

(defparameter *test-times* 100000)

(defun do-game (change-choice?)
  (let* ((master-choice (random 3))
         (user-choice (random 3))
         (openable (remove-if (lambda (x) 
                                (or (= x master-choice) (= x user-choice)))
                              (loop for i from 0 to 2 collect i))))
    (when change-choice?
      (let ((open-choice (nth (random (length openable)) openable)))
        (setf user-choice (- (+ 0 1 2) user-choice open-choice))))
    (= master-choice user-choice)))

(defun montie-hole-problem ()
  (let ((win-count-change 0)
        (win-count-not-change 0))
    (dotimes (i *test-times*)
      (if (do-game t) (incf win-count-change))
      (if (do-game nil) (incf win-count-not-change)))
    (format t "change: ~A~%fix: ~A"
            (/ win-count-change 1.0 *test-times*)
            (/ win-count-not-change 1.0 *test-times*))))

(montie-hole-problem)
