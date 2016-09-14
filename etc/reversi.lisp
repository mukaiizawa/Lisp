
(require "ltk" *module-ltk*)
(require "stdlib" *module-stdlib*)

(defparameter *width* 40)
(defparameter *board* (make-array '(8 8) :initial-element 'none))

(defmacro draw-board ()
  `(progn
     (dorange (i 0 7)
       (dorange (j 0 7)
         (itemconfigure
           canvas
           (create-rectangle canvas
                             (* i *width*)
                             (* j *width*)
                             (+ (* *width* i) *width*)
                             (+ (* *width* j) *width*))
           "fill" "green")))
     (put-oval 'black 3 3)
     (put-oval 'black 4 4)
     (put-oval 'white 3 4)
     (put-oval 'white 4 3)))

(defmacro put-oval (turn i j)
  `(itemconfigure canvas 
                  (create-oval canvas
                               (* ,i *width*)
                               (* ,j *width*)
                               (+ (* ,i *width*) *width*)
                               (+ (* ,j *width*) *width*))
                  "fill" (mkstr ,turn)))

(defun toggle (x y1 y2)
  (if (eq x y1) y2 y1))

(with-ltk ()
  (bind *tk* "<Alt-q>" (ilambda (event) (setf *exit-mainloop* t)))
  (let ((canvas (pack (make-instance 'canvas  :width (* *width* 8) :height (* *width* 8))))
        (turn 'black))
    (draw-board)
    (bind canvas "<ButtonPress-1>"
          (lambda (event)
            (let ((i (truncate (event-x event) *width*))
                  (j (truncate (event-y event) *width*)))
              (put-oval turn i j)
              (setq turn (toggle turn 'black 'white))
              )))))

