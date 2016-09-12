
(provide "stdlib")

(defconstant +null-character+ (code-char 0))
(defconstant +empty-string+ "")

;; read macro
;; #m {{{

(set-dispatch-macro-character #\# #\m
  (lambda (stream char1 char2)
    (declare (ignore char1 char2))
    (print (macroexpand-1 (read stream t nil t)))))

;; }}}
;; #a {{{

#+ccl
(set-dispatch-macro-character #\# #\n
  (lambda (stream char1 char2)
    (declare (ignore char1 char2))
    (println (ccl:macroexpand-all (read stream t nil t)))))

;; }}}
;; #o {{{

(set-dispatch-macro-character #\# #\o
  (lambda (stream char1 char2)
    (declare (ignore char1 char2))
    `(pprint ,(read stream t nil t))))

;; }}}
;; #<< {{{

(set-dispatch-macro-character #\# #\<
  (lambda (stream sub-char numarg)
    (declare (ignore sub-char numarg))
    (let (chars)
      (do ((curr (read-char stream nil +null-character+)
                 (read-char stream nil +null-character+)))
        ((char= curr #\Newline))
        (unless (or (char= curr #\Space)
                    (char= curr #\<))
          (push curr chars)))
      (let* ((pattern (nreverse chars))
             (acc))
        (do* ((curr (read-char stream nil +null-character+)
                    (read-char stream nil +null-character+))
              (pointer pattern
                       (if (char= (first pointer) curr)
                         (rest pointer)
                         pattern)))
          ((null pointer)
           (coerce
             (nreverse
               (nthcdr (length pattern) acc))
             'string))
          (if (char= curr +null-character+)
            (error "Read macro `#<<': reached eof.")
            (push curr acc)))))))

;; }}}

;; macro utils
;; mkstr {{{

(defun mkstr (&rest args)
  (with-output-to-string (str)
    (dolist (i args)
      (if i (princ i str)))))

;; }}}
;; mksym {{{

(defun mksym (&rest args)
  (values (intern (apply #'mkstr args))))

;; }}}
;; mkkey {{{

(defun mkkey (&rest args)
  (values (intern (apply #'mkstr args) :keyword)))

;; }}}
;; with-gensyms {{{

#-clisp
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar (lambda (x)
                   `(,x (gensym)))
                 syms)
     ,@(mapcar (lambda (x)
                 `(declare (ignorable ,x)))
               syms)
     ,@body))

;; }}}
;; with-encoding {{{

(defmacro with-encoding (expr &body body)
  (with-gensyms (enc ff)
    `(multiple-value-bind (,enc ,ff)
       ,(if (keywordp (first expr))
          `(values ,@expr)
          `,expr)
       (let (#+ccl(ccl:*default-external-format*
                    (ccl:make-external-format :character-encoding ,enc
                                              :line-termination ,ff))
             #+ccl(ccl:*default-file-character-encoding* ,enc)
             #+ccl(ccl:*default-socket-character-encoding* ,enc))
         ,@body))))

;; }}}
;; canonical-letargs {{{

(defun canonical-letargs (lis)
  (if lis
    (cons
      (cond ((symbolp (car lis))
             (list (car lis)))
            ((consp (car lis))
             (car lis))
            (t
              (error "canonical-letargs: Bat let bindings")))
      (canonical-letargs (cdr lis)))))

;; }}}
;; ilambda {{{

(defmacro ilambda (params &body body)
  `(lambda ,params
     ,@(mapcar (lambda (x)
                 `(declare (ignorable ,x)))
               params)
     ,@body))

;; }}}
;; alambda {{{

(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

;; }}}
;; surround {{{

(defmacro surround (wrapper &body body)
  `(progn ,wrapper
          ,@body
          ,wrapper))

;; }}}
;; while {{{

(defmacro while (test &body body)
  `(do ()
     ((not ,test))
     ,@body))

;; }}}
;; dowhile {{{

(defmacro dowhile (test &body body)
  `(progn
     ,@body
     (while ,test
       ,@body)))

;; }}}
;; for {{{

(defmacro for ((var start stop &key (step 1)) &body body)
  (with-gensyms (gstop)
    `(do* ((,var ,start (+ ,step ,var))
           (gstop ,stop ,stop))
       ((not gstop))
       ,@body)))

;; }}}
;; dostring {{{

(defmacro dostring ((i str) &body body)
  (with-gensyms (index)
    `(dotimes (,index (length ,str))
       (let ((,i (char ,str ,index)))
         (declare (ignorable ,i))
         ,@body))))

;; }}}
;; dorange {{{

(defmacro dorange ((var start stop) &body body)
  (with-gensyms (gstop step)
    `(let* ((,step (if (<= ,start ,stop) 1 -1)))
       (do ((,var ,start (+ ,var ,step))
            (,gstop ,stop))
         ((or (and (plusp ,step)
                   (> ,var ,gstop))
              (and (minusp ,step) 
                   (< ,var ,gstop))))
         ,@body))))

;; (dorange (i 0 9)
;;   (princ i))
;; => 0123456789

;; (dorange (i 9 0)
;;   (princ i))
;; => 9876543210

;; }}}
;; aif {{{

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it
       ,then-form
       ,else-form)))

;; }}}
;; awhen {{{

(defmacro awhen (test-form &body body)
  `(aif ,test-form
     (progn ,@body)))

;; }}}
;; awhile {{{

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
     ((not it))
     ,@body))

;; }}}
;; doawhile {{{

(defmacro doawhile (expr &body body)
  (with-gensyms (at-first?)
    `(do ((it ,expr ,expr)
          (,at-first? t nil))
       ((and (not ,at-first?) (not it)))
       ,@body)))

;; }}}
;; aand {{{

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

;; }}}
;; acond {{{

(defmacro acond (&rest clauses)
  (if (null clauses)
    nil
    (let ((cl1 (car clauses))
          (sym (gensym)))
      `(let ((,sym ,(car cl1)))
         (if ,sym
           (let ((it ,sym)) ,@(cdr cl1))
           (acond ,@(cdr clauses)))))))

;; }}}
;; dcond {{{

;; cond for debug.
(defmacro dcond (&rest clauses)
  (if (null clauses)
    nil
    (let ((cl1 (car clauses))
          (sym (gensym)))
      `(let ((,sym ,(car cl1)))
         (if ,sym
           (let ((it ,sym))
             (declare (ignorable it))
             (echo "CASE:" ',(car cl1))
             ,@(cdr cl1))
           (dcond ,@(cdr clauses)))))))

;; }}}

;; sequential
;; before {{{

(defun before (elt seq &key (from-end nil) (test #'eql))
  (awhen (position elt seq :from-end from-end :test test)
    (subseq seq 0 it)))

;; }}}
;; after {{{

(defun after (elt seq &key (from-end nil) (test #'eql))
  (awhen (position elt seq :from-end from-end :test test)
    (subseq seq (1+ it))))

;; }}}

;; list utils
(proclaim'(inline last1 single? append1 conc1 mklist))
;; last1 {{{

(defun last1 (lst)
  (car (last lst)))

;; }}}
;; single? {{{

(defun single? (lst)
  (and (consp lst) (not (cdr lst))))

;; }}}
;; alist? {{{

(defun alist? (lis)
  (ignore-errors (not (find-if (compose #'not #'listp) lis))))

;; }}}
;; append1 {{{

(defun append1 (lst obj)
  (append lst (list obj)))

;; }}}
;; conc1 {{{

(defun conc1 (lst obj)
  (nconc lst (list obj)))

;; }}}
;; mklist {{{

(defun mklist (obj)
  (if (listp obj)
    obj
    (list obj)))

;; }}}
;; mkalist {{{

(defun mkalist (obj)
  (labels ((rec (lis)
                (when lis
                  (cons
                    (mklist (car lis))
                    (mkalist (cdr lis))))))
    (rec (mklist obj))))

;; }}}
;; iota {{{

(defun iota (from to &optional step)
  (labels ((rec (from to step acc)
                (if (> from to)
                  (nreverse acc)
                  (rec (+ from step)
                       to 
                       step
                       (push from acc)))))
    (rec from to (or step 1) nil)))

;; }}}
;; longer? {{{

(defun longer? (x y)
  (labels ((compare (x y)
                    (and (consp x)
                         (or (null y)
                             (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
      (compare x y)
      (> (length x) (length y)))))

;; }}}
;; filter {{{

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

;; }}}
;; group {{{

(defun group (lis n)
  (when (zerop n)
    (error "group: The argument n required non-zero value."))
  (when lis
    (funcall (alambda (lis &optional acc)
               (let ((rest (nthcdr n lis)))
                 (if (consp rest)
                   (self rest (cons (subseq lis 0 n) acc))
                   (nreverse (cons lis acc)))))
             lis)))

;; }}}
;; flatten {{{

(defun flatten (obj)
  (when obj
    (do* ((result (list obj))
          (node result))
      ((null node) result)
      (cond ((consp (car node))
             (when (cdar node) (push (cdar node) (cdr node)))
             (setf (car node) (caar node)))
            (t (setf node (cdr node)))))))

;; }}}
;; prune {{{

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

;; }}}
;; before? {{{

(defun before? (x y lst &key (test #'eql))
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before? x y (cdr lst) :test test))))))

;; }}}
;; after? {{{

(defun after? (x y lst &key (test #'eql))
  (let ((rest (before? y x lst :test test)))
    (and rest (member x rest :test test))))

;; }}}
;; last-member {{{

(defun last-member (tar lst &key (test #'eql))
  (labels ((rec (lst last-match)
                (let ((matched (member tar lst :test test)))
                  (if matched
                    (rec (member tar (cdr matched) :test test) matched)
                    last-match))))
    (rec lst nil)))

;; }}}
;; most {{{

(defun most (fn lst)
  (if (null lst)
    (values nil nil)
    (let* ((wins (car lst))
           (max (funcall fn wins)))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (when (> score max)
            (setq wins obj
                  max score))))
      (values wins max))))

;; }}}

;; functional utils
;; maptree {{{

(defun maptree (fn &rest args)
  (if (some #'atom args)
    (apply fn args)
    (apply #'mapcar
           (lambda (&rest args)
             (apply #'maptree fn args))
           args)))

;; }}}
;; memoize {{{

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    (lambda (&rest args)
      (multiple-value-bind (val win) (gethash args cache)
        (if win
          val
          (setf (gethash args cache)
                (apply fn args)))))))

;; }}}
;; compose {{{

(defun compose (&rest fns)
  (if fns
    (let ((fn1 (last1 fns))
          (fns (butlast fns)))
      (lambda (&rest args)
        (reduce #'funcall fns
                :from-end t
                :initial-value (apply fn1 args))))
    #'identity))

;; }}}

;; string utils
(proclaim'(inline empty? blank?))
;; empty? {{{

(defun empty? (str)
  (or (null str)
      (string= "" str)))

;; }}}
;; blank? {{{

(defun blank? (str)
  (or (empty? str)
      (empty? (string-trim " " str))))

;; }}}
;; explode {{{

(defun explode (sym)
  (map 'list (lambda (x)
               (intern (make-string 1 :initial-element x)))
       (symbol-name sym)))

;; @see On Lisp
;; (explode 'bomb)
;; => (B O M B)

;; }}}
;; string->list {{{

(defun string->list (char str)
  (labels ((rec (str acc)
                (aif (after char str)
                  (rec it (cons (before char str) acc))
                  (reverse (cons str acc)))))
    (rec str nil)))

;;(string->list #\, "abcd,e,f,,g,hi,j,klmnop,qr,")
;; => ("abcd" "e" "f" "" "g" "hi" "j" "klmnop" "qr" "") 

;; }}}
;; list->string {{{

(defun list->string (lis &key (char nil))
  (if lis
    (let ((*print-case* :downcase))
      (reduce (lambda (x y)
                (mkstr x (aif char it #\Space) y))
              lis))
    ""))

;; Examples: {{{
;;
;; (echo (list->string '(a b c d)))
;; => a b c d
;;
;; (echo (list->string '(a b c d) :char #\/))
;; => a/b/c/d
;;
;; }}}

;; }}}
;; string->byte {{{

(defun string->byte (str &key (encoding :default))
  #+ccl
  (ccl:encode-string-to-octets str :external-format encoding)
  #+sbcl
  (sb-ext:string-to-octets str :external-format encoding :null-terminate nil)
  #-(or sbcl ccl)
  (error "string->byte: not implemented."))

;; }}}
;; byte->string {{{

(defun byte->string (byte &key (encoding :default))
  #+ccl
  (ccl:decode-string-from-octets byte :external-format encoding)
  #+sbcl
  (sb-ext:octets-to-string byte :external-format encoding)
  #-(or sbcl ccl)
  (error "byte-string: not implemented."))

;; }}}
;; format-string {{{

(defun format-string (str size &key (float :right) (padding 0))
  (let ((padding (make-string padding :initial-element #\Space))
        (space-num (- size (length str))))
    (mkstr (if (eq float :left) (mkstr padding str))
           (make-string (if (< 0 space-num) space-num 0) :initial-element #\Space)
           (if (eq float :right) (mkstr str padding)))))

;; (format-string "test" 8)
;; => "    test"
;; (format-string "test" 2 :float :right)
;; => "test"
;; (format-string "test" 6 :float :left)
;; => "test  "
;; (format-string "test" 10 :float :right :padding 2)
;; => "      test  "
;; (format-string "test" 10 :float :left :padding 2)
;; => "  test      "

;; }}}

;; io utils
;; guess-encoding {{{

(defun guess-encoding (pathname)
  (labels ((partial-read ()
                         (let ((buf (make-array (expt 2 12)
                                                :element-type '(unsigned-byte 8)
                                                :initial-element 0)))
                           (unless (file-exists? pathname)
                             (error "guess-enoding: no such file or directory `~A'" pathname))
                           (with-open-file (in pathname :direction :input :element-type '(unsigned-byte 8))
                             (read-sequence buf in))
                           buf))
           (guess ()
                  (let ((buf (partial-read)))
                    (aif (find-if (lambda (encoding)
                                    (equalp buf
                                            (ignore-errors
                                              (string->byte (byte->string buf :encoding encoding)
                                                            :encoding encoding))))
                                  '( :Windows-31j
                                     :EUC-JP 
                                     :UTF-8
                                     :UTF-16
                                     :UTF-16BE
                                     :UTF-16LE
                                     :UTF-32
                                     :UTF-32BE
                                     :UTF-32LE
                                     :CP936
                                     :ISO-8859-1
                                     :ISO-8859-2
                                     :ISO-8859-3
                                     :ISO-8859-4
                                     :ISO-8859-5
                                     :ISO-8859-6
                                     :ISO-8859-7
                                     :ISO-8859-8
                                     :ISO-8859-9
                                     :ISO-8859-10
                                     :ISO-8859-11 
                                     :ISO-8859-13 
                                     :ISO-8859-14 
                                     :ISO-8859-15 
                                     :ISO-8859-16 
                                     :MACINTOSH
                                     :UCS-2
                                     :UCS-2BE
                                     :UCS-2LE
                                     :US-ASCII
                                     :GB2312))
                      (values it (if (find 13 buf) :windows :unix))    ; CR
                      (error "guess-encoding: can't guess encoding of `~A'." pathname)))))
    (guess)))

;; }}}
;; stdin {{{

(defun stdin ()
  (let ((buf nil))
    (awhile (read-line *standard-input* nil nil)
      (push it buf))
    (nreverse buf)))

;; }}}
;; stdout {{{

(defun stdout (lis)
  (dolist (i (mklist lis))
    (princln i)))

;; }}}
;; show-hash {{{

;; for debug util
(defun show-hash (hash)
  (maphash (lambda (k v)
             (print (mkstr k ":" v)))
           hash))

;; }}}
;; princln {{{

(defun princln (str &optional (stream *standard-output*))
  (princ str stream)
  (fresh-line stream))

;; }}}
;; println {{{

(defun println (str &optional (stream *standard-output*))
  (print str stream)
  (fresh-line stream))

;; }}}
;; echo {{{

(defun echo (&rest args)
  (princln (apply #'mkstr args)))

;; }}}
;; read-as-list {{{

(defun read-as-list (&optional args)
  (values (read-from-string
            (concatenate 'string "("
                         (apply #'read-line args)
                         ")"))))

;; }}}

;; file utils
;; dir-pathname? {{{

(defun dir-pathname? (pathname)
  (flet ((component-present-p (val)
                              (and val (not (eql val :unspecific)))))
    (and (not (component-present-p (pathname-name pathname)))
         (not (component-present-p (pathname-type pathname)))
         pathname)))

;; }}}
;; pathname-as-file {{{

(defun pathname-as-file (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "pathname-as-file: Can't reliably convert wild pathnames."))
    (if (dir-pathname? name)
      (let* ((directory (pathname-directory pathname))
             (name-and-type (pathname (first (last directory)))))
        (make-pathname
          :directory (butlast directory)
          :name (pathname-name name-and-type)
          :type (pathname-type name-and-type)
          :defaults pathname))
      pathname)))

;;}}}
;; pathname-as-directory {{{

(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "pathname-as-directory: Can't reliably convert wild pathnames."))
    (if (not (dir-pathname? name))
      (make-pathname
        :directory (append (or (pathname-directory pathname) (list :relative))
                           (list (file-namestring pathname)))
        :name nil
        :type nil
        :defaults pathname)
      pathname)))

;; }}}
;; directory-wildcard {{{

(defun directory-wildcard (dirname)
  (make-pathname
    :name :wild
    :type #-clisp :wild #+clisp nil
    :defaults (pathname-as-directory dirname)))

;; }}}
;; file-exists? {{{

;; like a probe-file. (for potable)
(defun file-exists? (pathname)
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)
  #+(or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))
  #+clisp
  (or (ignore-errors
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        (let ((directory-form (pathname-as-directory pathname)))
          (when (ext:probe-directory directory-form)
            directory-form))))
  #-(or sbcl cmu lispworks openmcl allegro clisp)
  (error "file-exists? not implemented"))

; }}}
;; parent-directory {{{

(defun parent-directory (pathname)
  (last1 (pathname-directory pathname)))

;; }}}
;; read-from {{{

(defun read-from (pathname)
  (let (buf)
    (with-encoding (guess-encoding pathname)
      (with-open-file (in pathname
                          :direction :input
                          :if-does-not-exist :error)
        (awhile (read-line in nil nil)
          (push it buf))))
    (nreverse buf)))

;; }}}
;; write-to, write-to! {{{

(defun write-to (lis pathname &key (if-exists :append))
  (with-open-file (out pathname
                       :direction :output
                       :if-exists if-exists
                       :if-does-not-exist :create)
    (dolist (i (mklist lis))
      (princln i out))))

;; syntax sugar
;; (write-to lis pathname :if-exists :supersede)
;; <=> (write-to! lis  pahtname)
(defun write-to! (lis pathname)
  (write-to lis pathname :if-exists :supersede))

;; }}}
;; mkdir {{{

(defun mkdir (pathname)
  (ensure-directories-exist (mkstr pathname "/")))

;; Examples:
;; (mkdir "foo") => foo
;; (mkdir "foo/bar") => foo/bar/
;; (mkdir "foo/bar/baz.txt") => foo/bar/

;; }}}
;; mkfile, mkfile! {{{

;; create a file when it does not exist.
(defun mkfile (pathname &key (if-exists :append))
  (with-open-file (out pathname
                       :direction :output
                       :if-exists if-exists
                       :if-does-not-exist :create)
    (declare (ignore out))))

;; syntax sugar
;; (mkfile pathname :if-exists :supersede)
;; <=> (mkfile! pahtname)
(defun mkfile! (pathname)
  (mkfile pathname :if-exists :supersede))

;; }}}
;; ls {{{

;; like 'ls' command.
(defun ls (dirname &key (file t) (dir t))
  (labels ((ls (dirname)
               (when (wild-pathname-p dirname)
                 (error "ls: Can only lisp concrete directory names."))
               (let ((wildcard (directory-wildcard dirname)))
                 #+(or sbcl cmu lispworks) (directory wildcard)
                 #+openmcl (directory wildcard :directories t)
                 #+allegro (directory wildcard :directories-are-files nil)
                 #+clisp (nconc (directory wildcard)
                                (directory (make-pathname
                                             :name nil
                                             :type nil
                                             :defaults wildcard
                                             :directory (append (pathname-directory wildcard)
                                                                (list :wild)))))
                 #-(or sbcl cmu lispworks openmcl allegro clisp)
                 (error "ls not implemented."))))
    (cond ((and file dir)
           (ls dirname))
          ((or file dir)
           (delete-if (if file
                        #'dir-pathname?
                        (compose #'not #'dir-pathname?))
                      (ls (pathname-as-directory dirname))))
          (t (error "ls: Please chose file or dir.")))))

;; }}}
;; mapfile {{{

(defun mapfile (fn &key (extension nil) (directory "./") (recursive nil))
  (let ((files (ls directory :dir nil))
        (extensions (mapcar #'symbol-name (mklist extension))))
    (when extensions
      (setq files (remove-if-not (lambda (pathname)
                                   (member (pathname-type pathname) extensions :test #'string-equal))
                                 files)))
    (dolist (file files)
      (funcall fn file))
    (when recursive (dolist (i (ls directory))
                      (mapfile fn
                               :extension extension
                               :directory i
                               :recursive t)))))

;; Examples: {{{
;;
;; (mapfile #'print)
;; => Printing files in your current working directory.
;; (mapfile #'print 'txt)
;; => Printing files (text files)
;; (mapfile #'print '(lisp txt))
;; => Printing files (text files and lisp files)
;;
;;
;; }}}

;; }}}

;; for application
(proclaim'(inline run-on? parse-int parse-sym))
;; run-on? {{{

(defun run-on? (key)
  (case key
    (:windows (or (find :WIN32 *features*)
                  (find :windows *features*)))))

;; }}}
;; parse-int {{{

(defun parse-int (str &key (junk-allowed nil))
  (with-input-from-string (in (mkstr str))
    (let ((val (if junk-allowed
                 (ignore-errors (read in))
                 (read in))))
      (values val))))

;; }}}
;; parse-sym {{{

(defun parse-sym (obj)
  (ignore-errors (intern (mkstr obj))))

;; }}}
;; getopt {{{

(defun getopt (args opts)
  (let* ((arg0 (first args))
         (args (rest args))
         (opts-parser (lambda ()
                        (let ((acc nil)
                              (buf nil))
                          (dostring (c opts)
                            (unless (char= c #\;)
                              (setq buf (mkstr buf c)))
                            (when (or (char= c #\:)
                                      (char= c #\;))
                              (push buf acc)
                              (setq buf nil)))
                          (awhen buf (push buf acc))
                          (nreverse acc))))
         (opt-list (funcall opts-parser))
         (opt-hash (let ((result (make-hash-table :test #'equal)))
                     (dolist (opt opt-list)
                       (setf (gethash opt result) nil))
                     result))
         (parsed-arg-list nil)
         (error-list nil)
         (find-opt-arg? nil)
         (seeked--? nil)
         (inhypen? nil)
         (buf ""))
    (labels ((contains? (key)
                        (multiple-value-bind (val contains?)
                          (gethash key opt-hash)
                          (declare (ignore val))
                          contains?))
             (opt? (arg)
                   (char= (char arg 0) #\-))
             (msg-arg-required ()
                               (format nil "~A: option requires an argument -- `~A~A'"
                                       arg0
                                       (if (> (length find-opt-arg?) 2) '-- '-)
                                       (before #\: find-opt-arg?)))
             (msg-invalid-opt (str)
                              (format nil "~A: `~A' invalid option"
                                      arg0 str))
             (seek-opts (str)
                        (dostring (c str)
                          (unless
                            (or (and (empty? buf)    ; initial hypen
                                     (char= c #\-))
                                (and find-opt-arg?    ; `=' in opt-with-arg
                                     (char= c #\=)))
                            (setq buf (mkstr buf c)))
                          (cond (find-opt-arg? 'do-nothing)   ; find opt arg
                                ((char= c #\-)    ; find hypen
                                 (setq inhypen?
                                       (if inhypen? '-- '-)))
                                ((and inhypen?    ; find opt needs arg
                                      (contains? (mkstr buf #\:)))
                                 (setq find-opt-arg? (mkstr buf #\:)
                                       buf ""))
                                ((and   ; find none arg opt
                                   (contains? buf)
                                   (or (and (eq inhypen? '-)
                                            (= (length buf) 1))
                                       (and (eq inhypen? '--)
                                            (> (length buf) 1))))
                                 (setf (gethash buf opt-hash) t)
                                 (setq buf ""))))
                        (unless (empty? buf)
                          (aif find-opt-arg?
                            (progn
                              (setf (gethash it opt-hash) buf)
                              (setq find-opt-arg? nil))
                            (aif inhypen?
                              (push (msg-invalid-opt str) error-list)
                              (push buf parsed-arg-list))))
                        (setq buf ""
                              inhypen? nil)))
      (dolist (arg args)
        (cond ((string= arg "--")
               (setq seeked--? t))
              (find-opt-arg?
                (if (opt? arg)    ; like `head -c -n'
                  (push (msg-arg-required) error-list)
                  (setf (gethash find-opt-arg? opt-hash) arg))
                (setq find-opt-arg? nil))
              ((or seeked--?    ; after `--'
                   (not (opt? arg)))
               (push arg parsed-arg-list))
              (t (seek-opts arg))))
      (awhen find-opt-arg?
        (push (msg-arg-required) error-list))
      (values (nreverse parsed-arg-list)
              (mapcar (lambda (x)
                        (list (aif (before #\: x)
                                it
                                x)
                              (gethash x opt-hash)))
                      opt-list)
              (nreverse error-list)))))

#| Function GETOPT
 |
 | Syntax:
 | getopt command-line-args option-string
 | => parsed-args, option-alist, error-messages
 |
 | Arguments and Values:
 | command-line-args---a list.
 | option-string---a string.
 | parsed-args---a list.
 | option-alist---an association-list.
 | error-messages---a list.
 |
 | Description:
 | The getopt function parses the command-line arguments.
 | option-string is a string containing the legitimate option characters.
 | option-string is separated by colon or semicolon.
 | If such a character is followed by a colon, the option requires an argument.
 |
 | Examples:
 | (getopt '("head" "-c" "20" "-q" "file-name") "c:bytes:n:lines:q;v;")
 | => ("file-name"),
 |    (("c" "20") ("bytes" NIL) ("n" NIL) ("lines" NIL) ("q" T) ("v" NIL)),
 |    NIL
 | (getopt '("head" "-x") "c:bytes:n:lines:q;v;")
 | => NIL,
 |    (("c" NIL) ("bytes" NIL) ("n" NIL) ("lines" NIL) ("q" NIL) ("v" NIL)),
 |    ("head: `-x' invalid option"))
 |
 | Side Effects:
 | None.
 |
 | Affected By:
 | None.
 |
 | Exceptional Situations:
 | Should signal an error of type type-error if command-line-args is not a
 | proper list or option-string is not a string.
 |
 | See Also:
 | defexe
 |
 | Notes:
 | None.
 |
 |#

;;}}}
;; expand-hyphen {{{

(defun expand-hyphen (str)
  (with-input-from-string (in str)
    (with-output-to-string (buf)
      (do ((c (read-char in nil 'EOF) (read-char in nil 'EOF)))
        ((eq c 'EOF) buf)
        (cond ((or (eq (peek-char nil in nil 'EOF) 'EOF)
                   (char/= (peek-char nil in) #\-))
               (write-char c buf))
              (t
                (read-char in)    ; move to #\-
                (aif (read-char in nil nil)
                  (dorange (cd (char-code c) (char-code it))    ; expand case
                    (write-char (code-char cd) buf))
                  (progn (write-char c buf)    ; when next char is 'EOF
                         (write-char #\- buf)))))))))


#| Function EXPAND-HYPHEN
 |
 | Syntax:
 | expand-hyphen str
 | => hyphen-expanded
 |
 | Arguments and Values:
 | str---a string.
 | hyphen-expanded---a string.
 |
 | Description:
 | The expand-hyphen expand hyphen in str.
 | See more information `Examples'.
 |
 | Examples:
 | (expand-hyphen "abc")
 | => "abc"
 | (expand-hyphen "a-zA-Z")
 | => "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" 
 | (expand-hyphen "a-b-c")
 | => "ab-c" 
 | (expand-hyphen "z-a")
 | => "zyxwvutsrqponmlkjihgfedcba"
 | (expand-hyphen "a-")
 | => "a-" 
 | (expand-hyphen "-a")
 | => "-a" 
 | (expand-hyphen "-")
 | => "-" 
 |
 | Side Effects:
 | None.
 |
 | Affected By:
 | None.
 |
 | Exceptional Situations:
 | None.
 |
 | See Also:
 | None.
 |
 | Notes:
 | None.
 |
 |#

;; }}}
;; defexe {{{

;; Anaphora: args, errors
(defmacro defexe (fn first second &body rest)
  (with-gensyms (gfn lambda-list opt-str)
    (let* ((lambda-list (aif first it))
           (opt-str (if lambda-list second ""))
           (body (if lambda-list rest (cons second rest))))
      `(progn
         (defun ,gfn ()
           (destructuring-bind (args ,(flatten lambda-list) errors)
             ((lambda (lis)
                (list (first lis)
                      (mapcar (lambda (x)
                                (second x))
                              (second lis))
                      (third lis)))
              (multiple-value-list (getopt (args) ,opt-str)))
             (declare (ignorable args errors))
             (awhen errors
               (format *error-output* "~{~A~%~}" it)
               (format *error-output* "Try `~A --help' for more information.~%" ,(mkstr fn)))
             ,@body))
         (let ((*print-case* :downcase))
           (to-executable (function ,gfn)
                       (mkstr ',fn (if (run-on? :windows) ".exe"))))))))

;; Examples: {{{
;;
;; (defexe hello ()
;;     (princ 'hello-world))
;;
;; (defexe getopt-test (-i -f -r -n)
;;   "i;f;r;n:"
;;   (if -i
;;     (princ "in interactive mode")
;;     (princ "not in interactive mode"))
;;   (if -f
;;     (princ "in force mode")
;;     (princ "not in force mode"))
;;   (if -r
;;     (princ "in recursive mode")
;;     (princ "not in recursive mode"))
;;   (if -n
;;     (princ "input filename " -n)
;;     (princ "no input filename")))
;;
;;
;; }}}

;; }}}
;; to-executable {{{

(defun to-executable (fn fname)
  #+ccl (ccl:save-application fname :toplevel-function fn :prepend-kernel t)
  #+sbcl (sb-ext:save-lisp-and-die fname :toplevel fn :executable t))

;; Examples:
;; (to-executable #'hello "hello.exe")

;; }}}
;; exit {{{

#+ccl
(defun exit ()
  (ccl:quit))

;; }}}
;; call {{{

(defun call (name args stream)
  #+ccl   (ccl:run-program name args :output stream)
  #+sbcl  (sb-ext:run-program name args :output stream)
  #+clisp (ext:shell name))

;; Examples:
;; (call "hello.exe" nil *standard-output*)

;; }}}
;; args {{{

(defun args ()
  #+allegro (system:command-line-arguments)
  #+sbcl sb-ext:*posix-argv*
  #+clisp ext:*args*
  #+ecl (si:command-args)
  #+cmu ext:*command-line-words*
  #+ccl ccl:*command-line-argument-list*
  #+lispworks system:*line-arguments-list*)

;; Notes:
;; you can't use option b like -b. (may be ccl's bug)

#| Function ARGS
 |
 | Syntax:
 | args
 | => arg-list
 |
 | Arguments and Values:
 | arg-list---a list.
 |
 | Description:
 | The args portable function that return a list of command-line arguments.
 |
 | Examples:
 | something-program written by lisp.
 | $ something-program -v file
 | => ("-v" "file")
 |
 | Side Effects:
 | None.
 |
 | Affected By:
 | None.
 |
 | Exceptional Situations:
 | None.
 |
 | See Also:
 | None.
 |
 | Notes:
 | None.
 |
 |#

;; }}}
;; usage {{{

(defun usage (&key (title nil) (desc nil) (opts nil) (foot nil))
  (lambda ()
    (format *error-output* "Usage: ~A~%" title)
    (format *error-output* "~{~A~%~}~%" (mklist desc))
    (dolist (i (group (append opts (list "    --help" "display this help and exit")) 2))
      (format *error-output* "~A~A~%" (format-string (first i) 25 :float :left :padding 2) (second i)))
    (format *error-output* "~%~{~A~%~}" (mklist foot))
    (exit)))

#| Function USAGE
 |
 | Syntax:
 | usage &key title desc opts foot
 | => function
 |
 | Arguments and Values:
 | title---a string.
 | desc---a string or list.
 | opts---a string or list.
 | foot---a string or list.
 | function---a function.
 |
 | Description:
 | The usage function create the function that describe how to use the
 | apprication.
 | Each keyword parameter to use explain the apprication.
 |
 | Examples:
 | (defvar x
 |   (usage :title "head [OPTION]... [FILE]"
 |          :desc  '("Print the first 10 lines of each FILE to standard output."
 |                   "With more than one FILE, precede each with a header giving the file name."
 |                   "With no FILE, or when FILE is -, read standard input.")
 |          :opts '("-c, --bytes=[-]N" "print the first N bytes of each file"
 |                  "-n, --lines=[-]N" "print the first N lines instead of the first 10"
 |                  "-q, --quiet" "never print headers giving file names"
 |                  "-v, --verbose" "always print headers giving file names")
 |          :foot "N may have a multiplier suffix: b 512, k 1024, m 1024*1024."))
 | => x
 | (funcall x)
 | => Usage: head [OPTION]... [FILE]
 |    Print the first 10 lines of each FILE to standard output.
 |    With more than one FILE, precede each with a header giving the file name.
 |    With no FILE, or when FILE is -, read standard input.
 |
 |      -c, --bytes=[-]N         print the first N bytes of each file
 |      -n, --lines=[-]N         print the first N lines instead of the first 10
 |      -q, --quiet              never print headers giving file names
 |      -v, --verbose            always print headers giving file names
 |          --help               display this help and exit
 |
 |    N may have a multiplier suffix: b 512, k 1024, m 1024*1024.
 |
 | Side Effects:
 | None.
 |
 | Affected By:
 | None.
 |
 | Exceptional Situations:
 | None.
 |
 | See Also:
 | None.
 |
 | Notes:
 | None.
 |
 |#

;; }}}

