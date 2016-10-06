
(require :ltk *module-ltk*)
(require :coordinate-manager *module-coordinate-manager*)
(require :stdlib *module-stdlib*)

(defparameter *width* 40)
(defparameter *board* (make-array '(8 8) :initial-element 'green))
(defparameter *directions*
  (make-vector-list
    '((0 -1)    ;; top
      (0 1)     ;; bottom
      (-1 0)    ;; left
      (1 0)     ;; right
      (-1 -1)   ;; top-left
      (1 -1)    ;; top-right
      (-1 1)    ;; bottom-left
      (1 1))))  ;; bottom-right

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
      (and (eq (toggle turn)
               (safety-aref *board* next-coordinate))
           (rec (vector+ next-coordinate search-direction))
           search-direction))))

;; }}}
;; can-put-disk? {{{

(defmacro can-put-disk? (turn coordinate)
  `(let (reversible-direction)
     (dolist (direction *directions*)
       (awhen (find-puttable-direction ,turn ,coordinate direction)
         (push it reversible-direction)))
     (values reversible-direction)))

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
  `(dolist (direction ,directions)
     (do-reverse ,turn ,coordinate direction)))

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

(with-ltk ()
  (bind *tk* "<Control-c>" (ilambda (event) (setf *exit-mainloop* t)))
  (let ((canvas (pack
                  (make-instance 'canvas
                                 :width (* *width* 8)
                                 :height (* *width* 8))))
        (turn 'black))
    (draw-board)
    (force-focus canvas)
    (bind canvas "<ButtonPress-1>"
          (lambda (event)
            (with-coordinates ((make-coordinate :x (truncate (event-x event) *width*)
                                                :y (truncate (event-y event) *width*)))
              (awhen (can-put-disk? turn r1)
                (draw-disc turn r1)
                (reverse-disc turn r1 it)
                (setq turn (toggle turn))))))))

