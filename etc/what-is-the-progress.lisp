(defvar dic '(進捗 どう です か ???))

(defun main (lis)
  (if lis
    (let ((sym (nth (random (length dic)) dic)))
      (princ sym)
      (if (eq sym (first lis))
        (main (rest lis))
        (main dic)))))

(main dic)
