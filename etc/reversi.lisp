
(require "ltk" *module-ltk*)
(require "cordinate-manager" *module-cordinate-manager*)
(require "stdlib" *module-stdlib*)

(defparameter *width* 40)
(defparameter *board* (make-array '(8 8) :initial-element 'green))

;; draw-board {{{

(defmacro draw-board ()
  `(progn
     ; (dorange (x 0 7)
     ;   (dorange (y 0 7)
     ;     (itemconfigure
     ;       canvas
     ;       (create-rectangle canvas
     ;                         (* x *width*)
     ;                         (* y *width*)
     ;                         (+ (* *width* x) *width*)
     ;                         (+ (* *width* y) *width*))
     ;       "fill" "green")))
     (draw-disc 'black (make-cordinate :x 3 :y 3))
     (draw-disc 'black (make-cordinate :x 4 :y 4))
     (draw-disc 'white (make-cordinate :x 4 :y 3))
     (draw-disc 'white (make-cordinate :x 3 :y 4))))

;; }}}
;; draw-disc {{{

(defmacro draw-disc (turn cordinate)
  `(with-cordinates (,cordinate)
     (itemconfigure canvas 
                    (create-oval canvas
                                 (* x1 *width*)
                                 (* y1 *width*)
                                 (+ (* x1 *width*) *width*)
                                 (+ (* y1 *width*) *width*))
                    "fill" (mkstr ,turn))
     (setf (aref *board* x1 y1) ,turn)
     (echo #\( x1 #\, y1 #\))
     ; (echo *board*)
     (print-board *board*)
     (values)))

;; }}}
;; check-enemy-desk {{{

(defun check-enemy-desk (turn cordinate &key (x 0) (y 0))
  (labels ((rec (cordinate)
                (or (eq turn (safety-aref *board* (shift cordinate :x x :y y) turn))
                    (and (eq (toggle turn) (safety-aref *board* (shift cordinate :x x :y y) turn))
                         (rec (shift cordinate :x x :y y))))))
    (rec cordinate)))

;; }}}
;; can-put-disk? {{{

(defmacro can-put-disk? (turn cordinate)
  `(with-cordinates (,cordinate)
     (let (direction)
       (when (check-enemy-desk ,turn r1 :y -1) (push 'top direction))
       (when (check-enemy-desk ,turn r1 :y 1) (push 'bottom direction))
       (when (check-enemy-desk ,turn r1 :x -1) (push 'left direction))
       (when (check-enemy-desk ,turn r1 :x 1) (push 'right direction))
       direction)))

;; }}}
;; do-reverse {{{

(defmacro do-reverse (turn &key (x 0) (y 0))
  `(do* ((counter 1 (1+ counter))
         (next-cordinate (shift r1
                                :x (cond ((zerop ,x) 0)
                                         ((plusp ,x) counter)
                                         (t (- counter)))
                                :y (cond ((zerop ,y) 0)
                                         ((plusp ,y) counter)
                                         (t (- counter))))
                         (shift r1
                                :x (cond ((zerop ,x) 0)
                                         ((plusp ,x) counter)
                                         (t (- counter)))
                                :y (cond ((zerop ,y) 0)
                                         ((plusp ,y) counter)
                                         (t (- counter))))))
     ((eq (safety-aref *board* next-cordinate) ,turn))
     (draw-disc ,turn next-cordinate)))

;; }}}
;; reverse-disc {{{

(defmacro reverse-disc (turn cordinate directions)
  `(with-cordinates (,cordinate)
     (when (find 'top ,directions) (do-reverse ,turn :y -1))
     (when (find 'right ,directions) (do-reverse ,turn :x 1))
     (when (find 'left ,directions) (do-reverse ,turn :x -1))
     (when (find 'bottom ,directions) (do-reverse ,turn :y 1))
     ))

;; }}}
;; safety-aref {{{

(defun safety-aref (board cordinate)
  (with-cordinates (cordinate)
    (if (and (<= 0 x1) (< x1 8)
             (<= 0 y1) (< y1 8))
      (aref board x1 y1)
      'wall)))

;; }}}
;; toggle {{{

(defun toggle (turn)
  (if (eq turn 'black) 'white 'black))

;; }}}

(defun print-board (board)
  (fresh-line)
  (dorange (x 0 7)
    (dorange (y 0 7)
      (princ (mkstr (aref board y x) #\Space)))
    (princ #\Newline)))

(with-ltk ()
  (bind *tk* "<Alt-q>" (ilambda (event) (setf *exit-mainloop* t)))
  (let ((canvas (pack (make-instance 'canvas  :width (* *width* 8) :height (* *width* 8))))
        (turn 'black))
    (draw-board)
    (bind canvas "<ButtonPress-1>"
          (lambda (event)
            (with-cordinates ((make-cordinate :x (truncate (event-x event) *width*)
                                              :y (truncate (event-y event) *width*)))
              (awhen (can-put-disk? turn r1)
                (draw-disc turn r1)
                (reverse-disc turn r1 it))
              (setq turn (toggle turn)))))))

