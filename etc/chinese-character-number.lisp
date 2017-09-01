; convert numeric number into chinese character number.

(defparameter *units*
  '(""
    "万"
    "億"
    "兆"
    "京"
    "垓"
    "秭"
    "穣"
    "溝"
    "澗"
    "正"
    "載"
    "極"
    "恒河沙"
    "阿僧祇"
    "那由他"
    "不可思議"
    "無量大数"))

(defparameter *digits*
  '("" "十" "百" "千"))

(defparameter *chinese-char-nums*
  '("〇" "一" "二" "三" "四" "五" "六" "七" "八" "九"))

(defparameter *zero* (car *chinese-char-nums*))

(defun ctoi (c)
  (abs (- (char-code #\0) (char-code c))))

(defun range (n)
  (loop for i from 0 to (1- n) collect i))

(defun group4 (lis &optional acc)
  (if (null lis)
    (reverse acc)
    (group4 (nthcdr 4 lis)
            (push (subseq lis 0 (min 4 (length lis))) acc))))

(defun parse-unit (unit)
  (reverse
    (remove nil
            (mapcar (lambda (num digit)
                      (if (/= num 0) (list num digit)))
                    unit (range (length unit))))))

(defun parse-units (units)
  (reverse (remove nil
                   (mapcar (lambda (unit unit-index)
                             (let ((digits (parse-unit unit)))
                               (if digits (list digits unit-index))))
                           units (range (length units))))))

(defun print-units (units)
  (labels ((traverse (tree)
                     (mapcan (lambda (unit/unit-index)
                               (mapcan (lambda (digit/digit-index)
                                         (let ((digit (car digit/digit-index))
                                               (index (cadr digit/digit-index)))
                                           (if (or (/= digit 1) (= index 0))
                                             (princ (nth digit *chinese-char-nums*)))
                                           (princ (nth index *digits*))))
                                       (car unit/unit-index))
                               (princ (nth (cadr unit/unit-index) *units*)))
                             tree)))
    (if (null units)
      (princ *zero*)
      (traverse units))))

(defun main (num)
  (fresh-line)
  (print-units
    (parse-units
      (group4 (mapcar #'ctoi
                      (reverse (coerce (write-to-string num) 'list)))))))

(main 0)
(main 1)
(main 10)
(main 100)
(main 1000)
(main 1001)
(main 1234567)
(main 1000000000)
