; convert numeric number into chinese character number.

(defparameter *digits*
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

(defparameter *units*
  '("" "十" "百" "千"))

(defparameter *chinese-char-nums*
  '("〇" "一" "二" "三" "四" "五" "六" "七" "八" "九"))

(defun ctoi (c)
  (abs (- (char-code #\0) (char-code c))))

(defun fold-zero (chinese-char-nums)
  (let* ((zero (car *chinese-char-nums*))
         (result (remove zero chinese-char-nums)))
    (if result result (list zero))))

(defun to-chinese-char-num (num-chars)
  (do ((acc)
       (i 0 (1+ i))
       (rest-chars (reverse num-chars) (cdr rest-chars)))
    ((null rest-chars) (fold-zero acc))
    (let* ((num (ctoi (car rest-chars)))
           (chinese-char-num (nth num *chinese-char-nums*))
           (unit (nth i *units*)))
      (push (cond ((= num 0) chinese-char-num)
                  ((and (= num 1) (/= i 0)) unit)
                  (t (concatenate 'string chinese-char-num unit)))
            acc))))

(defun main (num)
  (do* ((acc)
        (i 0 (1+ i))
        (nums (coerce (write-to-string num) 'list) (butlast nums 4))
        (last4 (last nums 4) (last nums 4)))
    ((null nums) (reduce (lambda (x &optional (y ""))
                           (concatenate 'string x y))
                         (apply #'append acc)))
    (push (append (to-chinese-char-num last4) (list (nth i *digits*)))
          acc)))

(main 0)
(main 1)
(main 10)
(main 100)
(main 1000)
(main 1001)
(main 1234567)
(main 120001000)
