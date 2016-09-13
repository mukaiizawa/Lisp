
(require "ltk" *module-ltk*)
(require "stdlib" *module-stdlib*)

(defparameter *width* 40)

(with-ltk ()
  (bind *tk* "<Alt-q>" (alambda (event) (setf *exit-mainloop* t)))
  (let ((canvas (make-instance 'canvas  :width (* *width* 8) :height (* *width* 8)))
        (items))
    (pack canvas)
    (dorange (i 0 7)
      (dorange (j 0 7)
        (push 
          (create-rectangle canvas
                            (* i *width*)
                            (* j *width*)
                            (+ (* *width* i) *width*)
                            (+ (* *width* j) *width*))
          items)
        (itemconfigure
          canvas
          (first items)
          "fill" "green")))
    (bind canvas "<ButtonPress-1>"
          (ilambda (event)
            (itemdelete canvas (first items))))))

