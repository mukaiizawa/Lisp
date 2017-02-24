(require :stdlib *module-stdlib*)

;; Boyer-Moore String Search Algorithm
(defun boyer-moore (pat str &key (ignore-case nil))
  (labels ((make-table (pat)
                       (let* ((pat-length (length pat))
                              (table (make-hash-table :size pat-length)))
                         (dotimes (i (1- pat-length))
                           (setf (gethash (char pat i) table) (- pat-length i 1)))
                         (setf (gethash t table) pat-length)
                         table)))
    (let ((table (make-table pat))
          (pat-length (length pat))
          (str-length (length str))
          (test (if ignore-case #'string-equal #'string=))
          (i 0))
      (while (<= i (- str-length pat-length))
        (if (funcall test pat (subseq str i (+ i pat-length)))
          (return-from boyer-moore i)
          (incf i (aif (or (gethash (char str (+ i pat-length -1)) table)
                           (and ignore-case
                                (if (upper-case-p (char str (+ i pat-length -1)))
                                  (gethash (char-downcase (char str (+ i pat-length -1))) table)
                                  (gethash (char-upcase (char str (+ i pat-length -1))) table))))
                    it
                    (gethash t table)))))
      nil)))

;; Examples:
(boyer-moore "pep" "picled_pepper")
;; => 7
(boyer-moore  "ba" "ba")
;; => 0
(boyer-moore  "icled_pepper" "picled_pepper")
;; => 1
