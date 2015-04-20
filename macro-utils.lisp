
; loop {{{
(defmacro while (test &body body)
  `(do ()
     ((not ,test))
     ,@body))

(defmacro for ((var start stop) &body body)
  (with-gensyms (gstop)
                `(do ((,var ,start (1+ ,var))
                      (gstop ,stop))
                   ((> ,var gstop))
                   ,@body)))
; (for (i 0 10)
;      (print i))

; }}}
; anaphoric macros {{{

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it
       ,then-form
       ,else-form)))
; (aif (* 2 3)
;      (print it))
; (aif nil
;      (print 't)
;      (print it))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))
; (awhen (* 5 5)
;        (print (* it 1))
;        (print (* it 2)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
     ((not it))
     ,@body))
; (let ((i 0))
;   (awhile (< i 10)
;           (print (incf i))))

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro acond (&rest clauses)
  (if (null clauses)
    nil
    (let ((cl1 (car clauses))
          (sym (gensym)))
      `(let ((,sym ,(car cl1)))
         (if ,sym
           (let ((it ,sym)) ,@(cdr cl1))
           (acond ,@(cdr clauses)))))))
; (acond ((= 1 0)
;         (print (/ 1 0)))
;        ("it's me"
;         (print it))
;        (t
;          (princ it)))


; }}}
; macro-defining macros {{{

(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(defmacro abbrevs (&rest pairs)
  `(progn
     ,@(mapcar (lambda (pair)
                 `(abbrev ,@pair))
               pairs)))

; (abbrevs (pri princ)
;          (pr  print))


; }}}

