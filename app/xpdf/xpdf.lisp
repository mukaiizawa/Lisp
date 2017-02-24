(load "../../lib/stdlib")

(defun pathname->pdf-file (pathname)
  (mkstr (pathname-name pathname) ".pdf"))

(defun pathname->txt-file (pathname)
  (mkstr (pathname-name pathname) ".txt"))

(defexe pdf-text ()
  (mapfile (lambda (pathname)
             (call "pdftotext" 
                   (list (mkstr pathname)
                         (pathname->txt-file pathname))
                   *standard-output*))
           :recursive t
           :extension 'pdf)
  (print "finish"))
