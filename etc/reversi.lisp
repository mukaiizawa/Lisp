
(require "ltk" *module-ltk*)
(require "coordinate-manager" *module-coordinate-manager*)
(require "stdlib" *module-stdlib*)

(defparameter *width* 40)
(defparameter *board* (make-array '(8 8) :initial-element 'green))

;; direction {{{

(defmacro defdirection (direction x y)
  `(defmacro ,direction ()
     (make-coordinate :x ,x :y ,y)))

(defmacro defdirections (&rest directions)
  `(progn ,@(mapcar (lambda (direction)
                      `(defdirection ,@direction))
                    directions)))

(defdirections
  (top 0 -1)
  (bottom 0 1)
  (left -1 0)
  (right 1 0)
  (top-left -1 -1)
  (top-right 1 -1)
  (bottom-left -1 1)
  (bottom-right 1 1))

;; }}}
;; draw-board {{{

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
     (draw-disc 'black (make-coordinate :x 3 :y 3))
     (draw-disc 'black (make-coordinate :x 4 :y 4))
     (draw-disc 'white (make-coordinate :x 4 :y 3))
     (draw-disc 'white (make-coordinate :x 3 :y 4))))

;; }}}
;; draw-disc {{{

(defmacro draw-disc (turn coordinate)
  `(with-coordinates (,coordinate)
     (itemconfigure canvas 
                    (create-oval canvas
                                 (* x1 *width*)
                                 (* y1 *width*)
                                 (+ (* x1 *width*) *width*)
                                 (+ (* y1 *width*) *width*))
                    "fill" (mkstr ,turn))
     (setf (aref *board* x1 y1) ,turn)
     (echo #\( x1 #\, y1 #\))
     (print-board *board*)
     (values)))

;; }}}
;; find-puttable-direction {{{

(defun find-puttable-direction (turn coordinate search-direction)
  (labels ((rec (coordinate)
                (let1 (this-turn (safety-aref *board* coordinate))
                  (unless (eq this-turn 'wall)
                    (or (eq turn this-turn)
                        (rec (vector+ coordinate search-direction)))))))
    (let1 (next-coordinate (vector+ coordinate search-direction))
      (when (eq (toggle turn)
                (safety-aref *board* next-coordinate))
        (rec (vector+ next-coordinate search-direction))))))

;; }}}
;; can-put-disk? {{{

(defmacro can-put-disk? (turn coordinate)
  `(with-coordinates (,coordinate)
     (let (direction)
       (when (find-puttable-direction ,turn r1 (top)) (push 'top direction))
       (when (find-puttable-direction ,turn r1 (bottom)) (push 'bottom direction))
       (when (find-puttable-direction ,turn r1 (left)) (push 'left direction))
       (when (find-puttable-direction ,turn r1 (right)) (push 'right direction))
       (when (find-puttable-direction ,turn r1 (top-left)) (push 'top-left direction))
       (when (find-puttable-direction ,turn r1 (top-right)) (push 'top-right direction))
       (when (find-puttable-direction ,turn r1 (bottom-left)) (push 'bottom-left direction))
       (when (find-puttable-direction ,turn r1 (bottom-right)) (push 'bottom-right direction))
       direction)))

;; }}}
;; do-reverse {{{

(defmacro do-reverse (turn coordinate direction)
  `(do* ((next-coordinate (vector+ ,coordinate ,direction)
                         (vector+ next-coordinate ,direction)))
     ((eq (safety-aref *board* next-coordinate) ,turn))
     (draw-disc ,turn next-coordinate)))

;; }}}
;; reverse-disc {{{

(defmacro reverse-disc (turn coordinate directions)
  `(progn
     (when (find 'top ,directions) (do-reverse ,turn ,coordinate (top)))
     (when (find 'right ,directions) (do-reverse ,turn ,coordinate (right)))
     (when (find 'left ,directions) (do-reverse ,turn ,coordinate (left)))
     (when (find 'bottom ,directions) (do-reverse ,turn ,coordinate (bottom)))
     (when (find 'top-left ,directions) (do-reverse ,turn ,coordinate (top-left)))
     (when (find 'top-right ,directions) (do-reverse ,turn ,coordinate (top-right)))
     (when (find 'bottom-left ,directions) (do-reverse ,turn ,coordinate (bottom-left)))
     (when (find 'bottom-right ,directions) (do-reverse ,turn ,coordinate (bottom-right)))))

;; }}}
;; safety-aref {{{

(defun safety-aref (board coordinate)
  (with-coordinates (coordinate)
    (if (and (<= 0 x1) (< x1 8)
             (<= 0 y1) (< y1 8))
      (aref board x1 y1)
      'wall)))

;; }}}
;; toggle {{{

(defun toggle (turn)
  (if (eq turn 'black) 'white 'black))

;; }}}
;; print-board {{{

(defun print-board (board)
  (fresh-line)
  (dorange (x 0 7)
    (dorange (y 0 7)
      (princ (mkstr (aref board y x) #\Space)))
    (princ #\Newline)))

;; }}}

(with-ltk ()
  (bind *tk* "<Alt-q>" (ilambda (event) (setf *exit-mainloop* t)))
  (let ((canvas (pack (make-instance 'canvas  :width (* *width* 8) :height (* *width* 8))))
        (turn 'black))
    (draw-board)
    (bind canvas "<ButtonPress-1>"
          (lambda (event)
            (with-coordinates ((make-coordinate :x (truncate (event-x event) *width*)
                                              :y (truncate (event-y event) *width*)))
              (awhen (can-put-disk? turn r1)
                (draw-disc turn r1)
                (reverse-disc turn r1 it))
              (setq turn (toggle turn)))))))

