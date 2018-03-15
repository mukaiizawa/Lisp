; mdrd makefile

(load "xmdrd")

(defparameter usage
  (usage :title "mdrd FILE"
         :desc '("Read xc markdown and Output html.")))

(defexe xmdrd (--help)
  "help"
  (if (or --help (/= (length args) 1))
    (funcall (usage))
    (read-xmd (first args))))
