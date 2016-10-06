
(require :ltk *module-ltk*)
(require :coordinate-manager *module-coordinate-manager*)
(require :stdlib *module-stdlib*)

(defvar *drawing-interval* 1.0)
(defvar *cell-width* 30)
(defvar *board-width* 6)
(defvar *board-height* 8)
(defparameter *board* (make-array (list *board-width* *board-height*) :initial-element 0))
(defparameter *tetriminos* nil)
(defparameter *current-tetrimino* nil) 

(defstruct tetrimino shape color coordinate-origin coordinates)

;; deftetrimino {{{

(defmacro deftetrimino (shape color &rest coordinates)
  `(push ,(make-tetrimino 
            :shape shape
            :color color 
            :coordinate-origin (make-vector (/ *board-width* 2) 0)
            :coordinates (make-vector-list coordinates))
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
;; draw-current-tetrimino {{{

(defmacro draw-current-tetrimino ()
  `(dolist (coordinate (get-current-coordinates))
     (when (safety-aref coordinate)
       (set-board coordinate
                  (draw-rectangle coordinate
                                  (tetrimino-color *current-tetrimino*))))))

;; }}}
;; draw-board {{{

(defmacro draw-board ()
  `(dorange (x 0 *board-width*)
     (dorange (y 0 *board-height*)
       (draw-rectangle (make-vector x y) "#a0a0a0"))))

;; }}}
;; set-board {{{

(defun set-board (coordinate val)
  (with-coordinates (coordinate)
    (setf (aref *board* x1 y1)
          val)))

;; }}}
;; safety-aref {{{

(defun safety-aref (coordinate)
  (with-coordinates (coordinate)
    (if (and (<= 0 x1) (< x1 *board-width*)
             (<= 0 y1) (< y1 *board-height*))
      (aref *board* x1 y1))))

;; }}}
;; get-current-coordinates {{{

(defun get-current-coordinates ()
  (mapcar (lambda (coordinate)
            (vector+ (tetrimino-coordinate-origin *current-tetrimino*)
                     coordinate))
          (tetrimino-coordinates *current-tetrimino*)))

;; }}}
;; get-next-coordinates {{{

(defun get-next-coordinates (direction)
  (mapcar (lambda (current-coordinate)
            (vector+ current-coordinate
                     (tetrimino-coordinate-origin *current-tetrimino*)
                     direction))
          (mapcar (lambda (current-coordinate)
                    (if (vector-origin? direction)
                      current-coordinate
                      (vector-rotate current-coordinate (/ pi 2))))
                  (tetrimino-coordinates *current-tetrimino*))))

;; }}}
;; set-new-current-tetrimino {{{

(defmacro set-new-current-tetrimino ()
  `(progn
     (setf *current-tetrimino*
           (copy-tetrimino
             (nth (random (length *tetriminos*))
                  *tetriminos*)))
     (draw-current-tetrimino)))

;; }}}
;; delete-rectangles {{{

(defmacro delete-rectangles (coordinates)
  `(dolist (coordinate ,coordinates)
     (awhen (safety-aref coordinate)
       (itemdelete canvas it)
       (set-board coordinate 0))))

;; }}}
;; delete-line {{{

(defmacro delete-lines ()
  `(do ((x 0)
        (y 0))
     ((>= y *board-height*))
     (cond ((= (aref x y) 0)
            (setf x 0
                  y (1+ y)))
           (t
             (setf x (mod (1+ x) *board-width*)
                   y (if (= (1+ x) *board-width*) (1+ y) y))
             (when (and (= x 0) (/= y *board-height*))
               (delete-rectangles (mapcar (lambda (x)
                                            (make-vector x y))
                                          (iota 0 *board-width*))))))))

;; }}}
;; movable? {{{

(defmacro movable? ()
  `(lambda (next-coordinate)
     (or (aand (safety-aref next-coordinate) (= it 0))
         (find-if (lambda (current-coordinate)
                    (vector= current-coordinate next-coordinate))
                  (get-current-coordinates)))))

;; }}}
;; try-move-tetrimino {{{

(defmacro try-move-tetrimino (&optional (direction (make-vector)))
  `(let ((current-coordinates (get-current-coordinates))
         (next-coordinates (get-next-coordinates ,direction)))
     (unless (find-if (complement (movable?)) next-coordinates)
       (delete-rectangles current-coordinates)
       (setf (tetrimino-coordinate-origin *current-tetrimino*)
             (vector+ (tetrimino-coordinate-origin *current-tetrimino*)
                      ,direction))
       (draw-current-tetrimino))))

;; }}}
;; bind-keypress {{{

(defmacro bind-keypress (key &body body)
  `(bind *tk* ,(mkstr "<KeyPress-" key ">")
         (ilambda (event)
           (print *board*)
           ,@body)))

;; }}}

(with-ltk ()
  (bind *tk* "<Control-c>" (ilambda (event) (setf *exit-mainloop* t)))
  (let1 (canvas (pack (make-instance 'canvas
                                     :width (* *cell-width* *board-width*)
                                     :height (* *cell-width* *board-height*))))
    (force-focus canvas)
    (draw-board)
    ; (after-time 3 (lambda ()
    ;                 (try-move-tetrimino (make-vector 0 1))))
    ; (after-time 3 (alambda ()
    ;                 (try-move-tetrimino (make-vector 0 1))
    ;                 (after-time 3 self)))
    (set-new-current-tetrimino)
    (bind-keypress #\o (try-move-tetrimino))
    (bind-keypress #\h (try-move-tetrimino (make-vector -1 0)))
    (bind-keypress #\k (try-move-tetrimino (make-vector 0 -1)))
    (bind-keypress #\j (try-move-tetrimino (make-vector 0 1)))
    (bind-keypress #\l (try-move-tetrimino (make-vector 1 0)))
    (bind-keypress #\i (set-new-current-tetrimino))))

