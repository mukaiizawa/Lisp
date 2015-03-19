; list utils {{{
; last1, singlep, append1, conc1, mklist
; longerp, filter, group, flatten, prune

(proclaim'(inline last1 singlep append1 conc1 mklist))

(defun last1 (lst)
  (car (last lst)))

(defun singlep (lst)
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun conc1 (lst obj)
  (nconc lst (list obj)))

(defun mklist (obj)
  (if (listp obj)
    obj
    (list obj)))

(defun longerp (x y)
  (labels ((compare (x y)
                    (and (consp x)
                         (or (null y)
                             (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
      (compare x y)
      (> (length x) (length y)))))
; (print (longerp '(1 2) '(1)))
; (print (longerp '(1) '(1 2)))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))
; (print (filter (lambda (x)
;                  (if (oddp x)
;                    (+ x 10)))
;                '(1 2 3 4 5)))

(defun group (lst n)
  (if (zerop n) (error "function 'group' -> zero length"))
  (labels ((rec (lst acc)
                (let ((rest (nthcdr n lst)))
                  (if (consp rest)
                    (rec rest (cons (subseq lst 0 n) acc))
                    (nreverse (cons lst acc))))))
    (if lst (rec lst nil) nil)))
; (print (group '(1 2 3 4 5) 2))

(defun flatten (x)
  (labels ((rec (x acc)
                (cond ((null x) acc)
                      ((atom x) (cons x acc))
                      (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))
; (print (flatten '(a (b c) ((d e) f))))

(defun prune (fn lst)
  (labels ((rec (lst acc)
                (cond ((null lst) (nreverse acc))
                      ((consp (car lst))
                       (rec (cdr lst)
                            (cons (rec (car lst) nil) acc)))
                      (t (rec (cdr lst)
                              (if (funcall fn (car lst))
                                acc
                                (cons (car lst) acc)))))))
    (rec lst nil)))
; (print (prune #'oddp '(1 ((2 3) 4) 5 (6 7) 8 9)))

; }}}
; search utils {{{
; find2, before, after, last-member

(defun find2 (fn lst)
  (if (null lst)
    nil
    (let ((val (funcall fn (car lst))))
      (if val
        (values (car lst) val)
        (find2 fn (cdr lst))))))
; (print (find2 #'oddp  '(2 2 3 4 5)))

(defun before (x y lst &key (test #'eql))
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))
; (print (before 'a 'b '(a b c d)))

(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))
; (print (after "d" 'a '(a b c "d") :test #'equal))

(defun last-member (tar lst &key (test #'eql))
  (labels ((rec (lst last-match)
                (let ((matched (member tar lst :test test)))
                  (if matched
                    (rec (member tar (cdr matched) :test test) matched)
                    last-match))))
    (rec lst nil)))
; (print (last-member 'a '(a 1 a 2 a 3)))




; }}}








