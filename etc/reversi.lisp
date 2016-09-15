
(require "ltk" *module-ltk*)
(require "stdlib" *module-stdlib*)

(defparameter *width* 40)
(defparameter *board* (make-array '(8 8) :initial-element 'green))

(defmacro draw-board ()
  `(progn
     (dorange (x 0 7)
       (dorange (y 0 7)
         (itemconfigure
           canvas
           (create-rectangle canvas
                             (* x *width*)
                             (* y *width*)
                             (+ (* *width* x) *width*)
                             (+ (* *width* y) *width*))
           "fill" "green")))
     (put-disc 'black 3 3)
     (put-disc 'black 4 4)
     (put-disc 'white 4 3)
     (put-disc 'white 3 4)))

(defmacro draw-disc (turn &rest cordinates)
  `(progn
     ,@(mapcar (lambda (cordinate)
                 (let ((x (first cordinate))
                       (y (second cordinate)))
                   `(itemconfigure canvas 
                                   (create-oval canvas
                                                (* ,x *width*)
                                                (* ,y *width*)
                                                (+ (* ,x *width*) *width*)
                                                (+ (* ,y *width*) *width*))
                                   "fill" (mkstr ,turn))))
               (group cordinates 2))))

(defmacro put-disc (turn x y)
  `(progn
     (draw-disc ,turn ,x ,y)
     (reverse-disc ,turn ,x ,y)
     (setf (aref *board* ,x ,y) ,turn)))

(defmacro reverse-disc (turn x y)
  `(progn
     ;; top
     (when (eq (safety-aref *board* ,x (1- ,y)) 
               (toggle ,turn))
       (do ((next (safety-aref *board* ,x (1- ,y)))
            (y ,y (1- ,y)))
         ((or (eq next ,turn)
              (eq (aref *board* ,x ,y) ,turn)))
         (setf (aref *board* ,x (1- ,y)) ,turn)
         (draw-disc ,turn ,x (1- ,y))))))

(defun safety-aref (board x y)
  (if (and (<= 0 x) (< x 8)
           (<= 0 y) (< y 8))
    (aref board x y)
    'wall))

(defun toggle (turn)
  (if (eq turn 'black) 'white 'black))

(with-ltk ()
  (bind *tk* "<Alt-q>" (ilambda (event) (setf *exit-mainloop* t)))
  (let ((canvas (pack (make-instance 'canvas  :width (* *width* 8) :height (* *width* 8))))
        (turn 'black))
    (draw-board)
    (bind canvas "<ButtonPress-1>"
          (lambda (event)
            (let ((x (truncate (event-x event) *width*))
                  (y (truncate (event-y event) *width*)))
              (print (mkstr x #\, y)) 
              (put-disc turn x y)
              (setq turn (toggle turn))
              )))))

