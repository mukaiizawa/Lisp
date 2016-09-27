
(require "stdlib" *module-stdlib*)

(defun merge-sort (lis)
  (funcall (alambda (lis)
             (if (single? lis)
               lis
               (let1 (segment (floor (/ (length lis) 2)))
                 (merge 'list
                        (self (subseq lis 0 segment))
                        (self (subseq lis segment))
                        #'<))))
           lis))

#o(merge-sort (list 1 7 2 3 4 4 3 4 8 6 7))

