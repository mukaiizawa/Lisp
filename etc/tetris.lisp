
(require "ltk" *module-ltk*)
(require "coordinate-manager" *module-coordinate-manager*)
(require "stdlib" *module-stdlib*)

(defvar *drawing-interval* 1.0)
(defvar *cell-width* 30)
; (defparameter *board-width* 10)
; (defparameter *board-height* 20)
(defvar *board-width* 4)
(defvar *board-height* 6)
(defparameter *board* (make-array '(20 10) :initial-element 0))
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
  "#00ffff"
  (0 2)
  (0 1)
  (0 0)
  (0 -1))

;; }}}
;; J {{{

(deftetrimino
  'J
  "#ff00ff"
         (0 2)
         (0 1)
  (-1 0) (0 0))

;; }}}
;; L {{{

(deftetrimino
  'L
  "#ffff00"
  (0 2)
  (0 1)
  (0 0) (1 0))

;; }}}
;; O {{{

(deftetrimino
  'O
  "#ff0000"
  (0 1) (1 1)
  (0 0) (1 0))

;; }}}
;; S {{{

(deftetrimino
  'S
  "#00ff00"
         (0 1) (1 1)
  (-1 0) (0 0))

;; }}}
;; T {{{

(deftetrimino
  'T
  "#0000ff"
  (-1 0) (0 0) (1 0)
         (0 -1))

;; }}}
;; Z {{{

(deftetrimino
  'Z
  "#000000"
  (-1 1) (0 1)
         (0 0) (1 0))

;; }}}

;; draw-rectangle {{{

(defmacro draw-rectangle (coordinate color)
  `(with-coordinates (,coordinate)
     (let1 (item (create-rectangle canvas
                                   (* x1 *cell-width*)
                                   (* y1 *cell-width*)
                                   (+ (* *cell-width* x1) *cell-width*)
                                   (+ (* *cell-width* y1) *cell-width*)))
       (itemconfigure canvas item "fill" ,color)
       (values item))))

;; }}}
;; draw-tetrimino {{{

(defmacro draw-tetrimino ()
  `(dolist (coordinate (tetrimino-vectors *falling-tetrimino*))
     (with-coordinates ((to-absolute-coordinate coordinate))
       (when (safety-aref *board* r1)
         (setf (aref *board* x1 y1)
               (draw-rectangle r1 (tetrimino-color *falling-tetrimino*)))))))

;; }}}
;; draw-board {{{

(defmacro draw-board ()
  `(dorange (x 0 *board-width*)
     (dorange (y 0 *board-height*)
       (draw-rectangle (make-vector x y) "#a0a0a0"))))

;; }}}
;; safety-aref {{{

(defun safety-aref (board coordinate)
  (with-coordinates (coordinate)
    (if (and (<= 0 x1) (< x1 *board-height*)
             (<= 0 y1) (< y1 *board-height*))
      (aref board x1 y1))))

;; }}}
;; set-new-falling-tetrimino {{{

(defmacro set-new-falling-tetrimino ()
  `(progn
     (setf *falling-tetrimino*
           (copy-structure
             (nth (random (length *tetriminos*))
                  *tetriminos*)))
     (draw-tetrimino)))

;; }}}
;; to-absolute-coordinate {{{

(defun to-absolute-coordinate (coordinate)
  (vector+ (tetrimino-coordinate-origin *falling-tetrimino*) coordinate))

;; }}}
;; remove-rectangle {{{

(defmacro remove-rectangle ()
  `(dolist (coordinate (tetrimino-vectors *falling-tetrimino*))
     (with-coordinates ((to-absolute-coordinate coordinate))
       (when (safety-aref *board* r1)
         (itemdelete canvas (aref *board* x1 y1))))))

;; }}}
;; try-move-tetrimino {{{

(defmacro try-move-tetrimino (direction)
  `(unless (find nil (mapcar (lambda (coordinate)
                               (safety-aref *board* (vector+ (to-absolute-coordinate coordinate)
                                                             ,direction)))
                             (tetrimino-vectors *falling-tetrimino*)))
     (remove-rectangle)
     (setf (tetrimino-coordinate-origin *falling-tetrimino*) 
           (vector+ (tetrimino-coordinate-origin *falling-tetrimino*) ,direction))
     (draw-tetrimino)))

;; }}}
;; try-rotate-tetrimino {{{

(defmacro try-rotate-tetrimino ()
  `(progn
     (remove-rectangle)
     (setf (tetrimino-vectors *falling-tetrimino*)
           (mapcar (lambda (coordinate)
                     (vector-rotate coordinate (/ pi 2)))
                   (tetrimino-vectors *falling-tetrimino*)))
     (draw-tetrimino)))

;; }}}

(defmacro bind-keypress (key &body body)
  `(bind *tk* ,(mkstr "<KeyPress-" key ">")
         (ilambda (event)
           (print *board*)
           ,@body)))


(setq *falling-tetrimino* (make-tetrimino :coordinate-origin (make-vector (/ *board-width* 2) 0)
                                          :vectors (list (make-vector 0 0))
                                          :color "#000000"))

(with-ltk ()
  (bind *tk* "<KeyPress-q>" (ilambda (event) (setf *exit-mainloop* t)))
  (let1 (canvas (pack (make-instance 'canvas
                                     :width (* *cell-width* *board-width*)
                                     :height (* *cell-width* *board-height*))))
    (draw-board)
    (set-new-falling-tetrimino)
    (bind-keypress #\s (try-rotate-tetrimino))
    (bind-keypress #\o (try-rotate-tetrimino))
    (bind-keypress #\h (try-move-tetrimino (make-vector -1 0)))
    (bind-keypress #\k (try-move-tetrimino (make-vector 0 -1)))
    (bind-keypress #\j (try-move-tetrimino (make-vector 0 1)))
    (bind-keypress #\l (try-move-tetrimino (make-vector 1 0)))
    (bind-keypress #\i (set-new-falling-tetrimino))))

