
(require :ltk *module-ltk*)
(require :coordinate-manager *module-coordinate-manager*)
(require :stdlib *module-stdlib*)

(defvar *drawing-interval* 1000)
(defvar *cell-width* 30)
(defvar *board-width* 4)
(defvar *board-height* 10)
(defparameter *board* (make-array (list *board-width* *board-height*) :initial-element 0))
(defparameter *tetrominos* nil)
(defparameter *current-tetromino* nil) 
(defparameter canvas nil) 

(defstruct tetromino shape color coordinate-origin coordinates)

;; deftetromino {{{

(defmacro deftetromino (shape color &rest coordinates)
  `(push ,(make-tetromino 
            :shape shape
            :color color 
            :coordinate-origin (make-vector (/ *board-width* 2) 0)
            :coordinates (make-vector-list coordinates))
         *tetrominos*))

;; }}}
;; I {{{

(deftetromino
  'I
  "#00ffff"
  (0 2)
  (0 1)
  (0 0)
  (0 -1))

;; }}}
;; J {{{

(deftetromino
  'J
  "#ff00ff"
         (0 2)
         (0 1)
  (-1 0) (0 0))

;; }}}
;; L {{{

(deftetromino
  'L
  "#ffff00"
  (0 2)
  (0 1)
  (0 0) (1 0))

;; }}}
;; O {{{

(deftetromino
  'O
  "#ff0000"
  (0 1) (1 1)
  (0 0) (1 0))

;; }}}
;; S {{{

(deftetromino
  'S
  "#00ff00"
         (0 1) (1 1)
  (-1 0) (0 0))

;; }}}
;; T {{{

(deftetromino
  'T
  "#0000ff"
  (-1 0) (0 0) (1 0)
         (0 -1))

;; }}}
;; Z {{{

(deftetromino
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
;; draw-current-tetromino {{{

(defmacro draw-current-tetromino ()
  `(dolist (coordinate (get-current-coordinates))
     (when (safety-aref coordinate)
       (set-board coordinate
                  (draw-rectangle coordinate
                                  (tetromino-color *current-tetromino*))))))

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
            (vector+ (tetromino-coordinate-origin *current-tetromino*)
                     coordinate))
          (tetromino-coordinates *current-tetromino*)))

;; }}}
;; get-next-coordinates {{{

(defun get-next-coordinates (direction)
  (mapcar (lambda (current-coordinate)
            (vector+ current-coordinate
                     direction))
          (get-current-coordinates)))

;; }}}
;; get-rotate-coordinates {{{

(defun get-rotate-coordinates ()
  (mapcar (lambda (current-coordinate)
            (vector+ (tetromino-coordinate-origin *current-tetromino*)
                     (vector-rotate current-coordinate (/ pi 2))))
          (tetromino-coordinates *current-tetromino*)))

;; }}}
;; set-new-current-tetromino {{{

(defmacro set-new-current-tetromino ()
  `(progn
     (setf *current-tetromino*
           (copy-tetromino
             (nth (random (length *tetrominos*))
                  *tetrominos*)))
     (draw-current-tetromino)))

;; }}}
;; delete-rectangles {{{

(defmacro delete-rectangles (coordinates)
  `(dolist (coordinate ,coordinates)
     (awhen (safety-aref coordinate)
       (itemdelete canvas it)
       (set-board coordinate 0))))

;; }}}
;; delete-line {{{

;; include bug
(defmacro delete-lines ()
  `(dotimes (y *board-height*)
     (when (not
             (find-if #'zerop
                      (mapcar (lambda (x)
                                (aref *board* x y))
                              (iota 0 (1- *board-width*)))))
       (delete-rectangles
         (mapcar (lambda (x)
                   (make-vector x y))
                 (iota 0 (1- *board-width*)))))))

;; }}}
;; movable? {{{

(defmacro movable? ()
  `(lambda (next-coordinate)
     (or (aand (safety-aref next-coordinate) (= it 0))
         (find-if (lambda (current-coordinate)
                    (vector= current-coordinate next-coordinate))
                  (get-current-coordinates)))))

;; }}}
;; try-move {{{

(defmacro try-move (direction)
  `(let ((current-coordinates (get-current-coordinates))
         (next-coordinates (get-next-coordinates ,direction)))
     (cond ((not (find-if (complement (movable?)) next-coordinates))
            (delete-rectangles current-coordinates)
            (setf (tetromino-coordinate-origin *current-tetromino*)
                  (vector+ (tetromino-coordinate-origin *current-tetromino*)
                           ,direction))
            (draw-current-tetromino))
           ((vector= ,direction (make-vector 0 1))
            (delete-lines)
            (set-new-current-tetromino))
           (t nil))))

;; }}}
;; try-rotate {{{

(defmacro try-rotate ()
  `(let ((current-coordinates (get-current-coordinates))
         (rotate-coordinates (get-rotate-coordinates)))
     (unless (find-if (complement (movable?)) rotate-coordinates)
       (delete-rectangles current-coordinates)
       (setf (tetromino-coordinates *current-tetromino*)
             (mapcar (lambda (coordinate)
                       (vector-rotate coordinate (/ pi 2)))
                     (tetromino-coordinates *current-tetromino*)))
       (draw-current-tetromino))))

;; }}}
;; main {{{

(defun main ()
  (after-time *drawing-interval*
              (lambda ()
                (try-move (make-vector 0 1))
                (main))))

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
  (setf canvas (pack (make-instance 'canvas
                                    :width (* *cell-width* *board-width*)
                                    :height (* *cell-width* *board-height*))))
  (force-focus canvas)
  (draw-board)
  (set-new-current-tetromino)
  (bind-keypress #\h (try-move (make-vector -1 0)))
  (bind-keypress #\j (try-move (make-vector 0 1)))
  (bind-keypress #\k (try-rotate))
  (bind-keypress #\l (try-move (make-vector 1 0)))
  (main))

