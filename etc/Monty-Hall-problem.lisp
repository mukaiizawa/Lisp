#|
 | montie hole problem.
 |#

(defparameter *test-times* 100000)

(defun try-game (change-choice?)
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
  (let ((win-change 0)
        (win-fix 0))
    (dotimes (i *test-times*)
      (if (try-game t) (incf win-change))
      (if (try-game nil) (incf win-fix)))
    (format t "change: ~A~%fix: ~A"
            (/ win-change 1.0 *test-times*) (/ win-fix 1.0 *test-times*))))

(montie-hole-problem)

