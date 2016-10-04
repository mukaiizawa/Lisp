
(require "ltk" *module-ltk*)
(require "coordinate-manager" *module-coordinate-manager*)
(require "stdlib" *module-stdlib*)

(defvar *drawing-interval* 1)
(defvar *cell-width* 30)
; (defparameter *board-width* 10)
; (defparameter *board-height* 20)
(defvar *board-width* 6)
(defvar *board-height* 10)
(defparameter *board* (make-array '(20 10)))
(defparameter *tetriminos* nil)
(defparameter *falling-tetrimino* nil) 

(defstruct tetrimino shape color coordinate-origin vectors)

;; deftetrimino {{{

(defmacro deftetrimino (shape color &rest vectors)
  `(push ,(make-tetrimino 
            :shape shape
            :color color 
            :coordinate-origin (make-vector (/ *board-width* 2) 0)
            :vectors (make-vector-list vectors))
         *tetriminos*))

;; }}}
;; I {{{

(deftetrimino
  'I
  "green"
  (0 2)
  (0 1)
  (0 0)
  (0 -1))

;; }}}
;; J {{{

(deftetrimino
  'J
  "green"
         (0 2)
         (0 1)
  (-1 0) (0 0))

;; }}}
;; L {{{

(deftetrimino
  'L
  "green"
  (0 2)
  (0 1)
  (0 0) (1 0))

;; }}}
;; O {{{

(deftetrimino
  'O
  "green"
  (0 1) (1 1)
  (0 0) (1 0))

;; }}}
;; S {{{

(deftetrimino
  'S
  "green"
         (0 1) (1 1)
  (-1 0) (0 0))

;; }}}
;; T {{{

(deftetrimino
  'T
  "green"
  (-1 0) (0 0) (1 0)
         (0 -1))

;; }}}
;; Z {{{

(deftetrimino
  'Z
  "green"
  (-1 -1) (0 1)
          (0 0) (1 0))

;; }}}

;; draw-board {{{

(defmacro draw-board ()
  `(progn
     (dorange (x 0 *board-width*)
       (dorange (y 0 *board-height*)
         (create-rectangle canvas
                           (* x *cell-width*)
                           (* y *cell-width*)
                           (+ (* *cell-width* x) *cell-width*)
                           (+ (* *cell-width* y) *cell-width*))))))

;; }}}
;; draw-rectangle {{{

(defmacro draw-rectangle (coordinate)
  `(with-coordinates (,coordinate)
     (itemconfigure
       canvas
       (create-rectangle canvas
                         (* x1 *cell-width*)
                         (* y1 *cell-width*)
                         (+ (* *cell-width* x1) *cell-width*)
                         (+ (* *cell-width* y1) *cell-width*))
       "fill"
       (tetrimino-color *falling-tetrimino*))))

;; }}}
;; draw-tetrimino {{{

(defmacro draw-tetrimino ()
  `(dolist (coordinate (tetrimino-vectors *falling-tetrimino*))
     (with-coordinates ((to-absolute-coordinate coordinate))
       (when (safety-aref *board* r1)
         (setf (aref *board* x1 y1)
               (draw-rectangle r1))))))

;; }}}
;; safety-aref {{{

(defun safety-aref (board coordinate)
  (with-coordinates (coordinate)
    (if (and (<= 0 x1) (< x1 *board-height*)
             (<= 0 y1) (< y1 *board-height*))
      (aref board x1 y1))))

;; }}}
;; get-next-tetrimino {{{

(defun get-next-tetrimino ()
  (nth 2
    ; (random (length *tetriminos*))
       *tetriminos*))

;; }}}
;; to-absolute-coordinate {{{

(defun to-absolute-coordinate (coordinate)
  (vector+ (tetrimino-coordinate-origin *falling-tetrimino*) coordinate))

;; }}}
;; try-move-tetrimino {{{

(defmacro try-move-tetrimino (direction)
  `(progn
     (setf (tetrimino-coordinate-origin *falling-tetrimino*) 
           (vector+ (tetrimino-coordinate-origin *falling-tetrimino*) ,direction))
     (draw-tetrimino)))

;; }}}

(defmacro bind-keypress ((key) &body body)
  `(bind canvas (mkstr "<KeyPress-" ,key ">")
         (lambda (event)
           (with-coordinates ((make-vector (truncate (event-x event) *cell-width*)
                                          (truncate (event-y event) *cell-width*)))
             ,@body))))


(setq *falling-tetrimino* (get-next-tetrimino))

(with-ltk ()
  (bind *tk* "<KeyPress-q>" (ilambda (event) (setf *exit-mainloop* t)))
  (let1 (canvas (pack (make-instance 'canvas
                                     :width (* *cell-width* *board-width*)
                                     :height (* *cell-width* *board-height*))))
    (draw-board)
    (bind-keypress (#\h) (try-move-tetrimino (make-vector -1 0)))
    (bind-keypress (#\l) (try-move-tetrimino (make-vector 1 0)))
    (while t
      (try-move-tetrimino (make-vector 0 1))
      (sleep *drawing-interval*))))

