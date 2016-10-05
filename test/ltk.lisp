
(require :ltk *module-ltk*)
(require :stdlib *module-stdlib*)
(require :test-utils *module-test-utils*)

(with-ltk ()
  (bind *tk* "<Alt-q>" (ilambda (event) (setf *exit-mainloop* t)))
  (let* ((canvas (make-instance 'canvas))
         (key-down nil))
    (pack canvas)
    (bind canvas "<ButtonPress-1>"
          (lambda (evt)
            (setf key-down t)
            (create-oval canvas
                         (- (event-x evt) 10) (- (event-y evt) 10)
                         (+ (event-x evt) 10) (+ (event-y evt) 10))))
    (bind canvas "<ButtonRelease-1>" (lambda (evt) 
                                       (declare (ignore evt))
                                       (setf key-down nil)))
    (bind canvas "<Motion>"
          (lambda (evt)
            (when key-down
              (create-oval canvas
                           (- (event-x evt) 10) (- (event-y evt) 10)
                           (+ (event-x evt) 10) (+ (event-y evt) 10)))))))

(with-ltk ()
  (bind *tk* "<Alt-q>" (ilambda (event) (setf *exit-mainloop* t)))
  (dorange (i 0 5)
    (dorange (j 0 5)
      (grid (make-instance 'button :text (format nil "~d" (+ i j))) i j))))


(with-ltk ()
  (bind *tk* "<Alt-q>" (ilambda (event) (setf *exit-mainloop* t)))
  (let* ((fr1 (make-instance 'labelframe :text "a"))
         (fr2 (make-instance 'labelframe :text "b"))
         (btns (mapcar #'(lambda (n)
                           (make-instance 'button
                                          :master (if (< n 5) fr1 fr2)
                                          :text (format nil "Nm~d" n)))
                       '(0 1 2 3 4 5 6 7 8 9))))
    (pack (list fr1 fr2) :fill :both :side :left)
    (pack btns :fill :both :side :top)))


