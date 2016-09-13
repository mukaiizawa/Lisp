
(require "ltk" *module-ltk*)
(require "stdlib" *module-stdlib*)

(defparameter *width* 40)

(defmacro draw-board ()
  `(dorange (i 0 7)
     (dorange (j 0 7)
       (itemconfigure
         canvas
         (create-rectangle canvas
                           (* i *width*)
                           (* j *width*)
                           (+ (* *width* i) *width*)
                           (+ (* *width* j) *width*))
         "fill" "green"))))

(defmacro draw-oval (turn i j)
  `(itemconfigure
     canvas
     (create-oval canvas 0 0 (* i *width*) (* j *width*))
     "fill" (mkstr ,turn)))

(with-ltk ()
  (bind *tk* "<Alt-q>" (alambda (event) (setf *exit-mainloop* t)))
  (let ((canvas (make-instance 'canvas  :width (* *width* 8) :height (* *width* 8)))
        (turn 'black)
        (items))
    (pack canvas)
    ; (draw-board)
    (bind canvas "<ButtonPress-1>"
          (lambda (event)
            (let ((i (truncate (event-y event) *width*))
                  (j (truncate (event-x event) *width*)))
              (draw-oval 'black i j)
              )))))

