(load "../../lib/stdlib")

(defparameter *words* (mkhash :size 10000))
(defconstant *maxword* 100)


(defmacro read-common-part (var)
  `(if (or (alpha-char-p ,var)
           (char= ,var #\'))
     (progn
       (setf (aref buffer pos) ,var)
       (incf pos))
     (progn
       (unless (zerop pos)
         (see (intern (string-downcase
                        (subseq buffer 0 pos))))
         (setf pos 0))
       (aif (punc ,var) (see ,var)))))

(defun read-text (pathname)
  (with-open-file (s pathname :direction :input)
    (let ((buffer (make-string *maxword*))
          (pos 0))
      (do ((c (read-char s nil :eof)
              (read-char s nil :eof)))
        ((eql c :eof))
        (read-common-part c)))))

(defun read-stdin ()
  (let ((buffer (make-string *maxword*))
        (pos 0))
    (awhile (read-char *standard-input* nil nil)
      (read-common-part it))))

(defun random-next (prev)
  (let* ((choices (gethash prev *words*))
         (i (random (reduce #'+ choices :key #'cdr)
                    (make-random-state t))))
    (dolist (pair choices)
      (if (minusp (decf i (cdr pair)))
        (return (car pair))))))

(let ((prev '|.|))
  (defun see (symb)
    (let ((pair (assoc symb (gethash prev *words*))))
      (if (null pair)
        (push (cons symb 1) (gethash prev *words*))
        (incf (cdr pair)))
      (setf prev symb))))

(defun punc (c)
  (case c
    (#\. '|.|) (#\, '|,|) (#\! '|!|)
    (#\? '|?|) (#\; '|;|)  ))

(defun generate-text (n &optional (prev '|.|))
  (if (zerop n)
    (terpri)
    (let ((next (random-next prev)))
      (princ next)
      (princ #\Space)
      (generate-text (1- n) next))))


(defexe poem ((-n --words) --help)
  "n:words:help"
  (let ((files args)
        (usage (usage :title "poem [FILE]..."
                      :desc  "Create the random text from FILE."
                      :opts  '("-n, --words=[N]" "print the N words random text(default 100)"))))
    (when (or --help
              (find-if (compose #'not #'file-exists?) files))
      (funcall usage))
    (if (null args)
      (read-stdin)
      (dolist (i files)
        (read-text i)))
    (generate-text (aif (or -n --words) (parse-integer it) 100))))
