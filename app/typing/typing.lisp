(load "../../lib/stdlib")

(defvar *default-dic-path* "lisp.word")
(defvar *default-question-num* 30)
(defparameter *dic* (make-hash-table))
(defparameter *dic-length* -1)
(defparameter *total-key-stroke* 0)

(defun import-dic (dic-path)
  (mapcan (lambda (word)
            (setf (gethash (incf *dic-length*) *dic*) word))
          (read-from dic-path)))

(defun get-word ()
  (gethash (random *dic-length* (make-random-state t)) *dic*))

(defun summary (start-time)
  (let ((time (- (get-universal-time) start-time 0.0)))
    (surround (echo (make-sequence 'string 50 :initial-element #\-))
      (echo (format-string "Total key stroke" 20 :float :left)
            (format-string (mkstr *total-key-stroke* "   times") 20 :float :right))
      (echo (format-string "Time per key stroke" 20 :float :left)
            (format-string (mkstr (subseq (mkstr (/ *total-key-stroke* time 1.0)) 0 5) " key/sec") 20 :float :right))
      (echo (format-string "Total time" 20 :float :left)
            (format-string (mkstr time "     sec") 20 :float :right)))))

(defexe typing (-d -n --help)
  "d:n:help"
  (let ((usage (usage :title "typing"
                      :desc  "Symple typing game."
                      :opts '("-n" "a number of word. (default 60)"
                              "-d" "dictionary for typing.")))
        (dic-path (aif -d it *default-dic-path*))
        (question-num (aif -n (parse-integer it) *default-question-num*))
        (start-time (get-universal-time)))
    (when (or --help errors)
      (funcall usage))
    (import-dic dic-path)
    (while (>= (decf question-num) 0)
      (let ((word (get-word)))
        (echo word)
        (incf *total-key-stroke* (length word)) 
        (while (not (string= word (read-line))))))
    (summary start-time)))
