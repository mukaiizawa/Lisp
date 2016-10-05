
(require :stdlib *module-stdlib*)

(defun ascii ()
  ;; header
  (format t "~3A" #\Space)
  (for (i #x0 (<= i #xF))
    (format t "~2X" i))
  ;; body
  (for (i #x20 (< i #x7F))
    (when (= (mod i 16) 0)
      (format t "~%~2X-" (/ i 16)))
    (when (graphic-char-p (code-char i))
      (format t " ~A" (code-char i)))))

