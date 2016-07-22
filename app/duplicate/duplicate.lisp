
(load "../../lib/stdlib")

(defexe duplicate ((-s --show) --help)
  "s;show;help"
  (let ((usage (usage :title "duplicate FILE"
                      :desc  "Print duplicate line in file."
                      :opts  '("-s, --show" "show duplicate num")))
        (table (mkhash :test #'equal))
        (file (car args)))
    (when (or --help
              errors
              (/= (length args) 1))
      (funcall usage))
    (dofile (line file)
      (setf (gethash line table)
            (aif (gethash line table)
              (1+ it)
              1)))
    (maphash (lambda (k v)
               (when (and (> v 1))
                 (echo (if (or -s --show) (mkstr v ",")) k)))
             table)))

