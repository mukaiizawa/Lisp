(require :stdlib *module-stdlib*)

(defparameter chinese-char-nums
  '("〇" "一" "二" "三" "四" "五" "六" "七" "八" "九"))

(defparameter digits
  '("千" "万" "億" "兆" "景"))

(defun ctoi (c)
  (if (null c) 10
    (abs (- (char-code #\0) (char-code c)))))

(defun main (i)
  (do* ((nums (coerce (mkstr i) 'list) (butlast nums 4))
        (last4 (last nums 4) (last nums 4))
        (count 0 (1+ count))
        (acc ""))
    ((null nums) acc)
    (setf acc (mkstr 
                (nth (ctoi (car last4)) chinese-char-nums)
                (when (>= (length nums) 4)
                  (nth count digits))
                (nth (ctoi (cadr last4)) chinese-char-nums)
                (nth (ctoi (caddr last4)) chinese-char-nums)
                (nth (ctoi (cadddr last4)) chinese-char-nums)
                acc))))

(print (main 1))
(print (main 2))
(print (main 10))
(print (main 100))
(print (main 1000))
(print (main 1001))
(print (main 1234567))
(print (main 120001000))
