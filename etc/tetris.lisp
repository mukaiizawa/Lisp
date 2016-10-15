
(require :ltk *module-ltk*)
(require :coordinate-manager *module-coordinate-manager*)
(require :stdlib *module-stdlib*)

(defvar *drawing-interval* 500)
(defvar *cell-width* 30)
(defvar *board-width* 10)
(defvar *board-height* 20)
(defparameter *board* (make-array (list *board-width* *board-height*) :initial-element 0))
(defparameter *tetrominos* nil)
(defparameter *current-tetromino* nil) 
(defparameter *canvas* nil) 

(defstruct tetromino shape color coordinate-origin coordinates)

;; deftetromino {{{

(defmacro deftetromino (shape color &rest coordinates)
  `(push ,(make-tetromino 
            :shape shape
            :color color 
            :coordinate-origin (make-vector (/ *board-width* 2)
                                            (- (most #'- (mapcar #'second coordinates))))
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

;; in-board? {{{

(defun in-board? (x &optional y)
  (with-coordinates ((if (coordinate-p x)
                       x
                       (make-vector x y)))
    (and (<= 0 x1) (< x1 *board-width*)
         (<= 0 y1) (< y1 *board-height*))))

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
    (when (in-board? r1)
      (aref *board* x1 y1))))

;; }}}
;; draw-rectangle {{{

(defmacro draw-rectangle (coordinate color)
  `(with-coordinates (,coordinate)
     (let1 (item (create-rectangle *canvas*
                                   (* x1 *cell-width*)
                                   (* y1 *cell-width*)
                                   (+ (* *cell-width* x1) *cell-width*)
                                   (+ (* *cell-width* y1) *cell-width*)))
       (itemconfigure *canvas* item "fill" ,color)
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
  `(dolist (coordinate (mklist ,coordinates))
     (awhen (safety-aref coordinate)
       (itemdelete *canvas* it)
       (set-board coordinate 0))))

;; }}}
;; delete-line {{{

(defmacro delete-lines ()
  `(do* ((range-x (iota 0 (1- *board-width*)))
         (range-y (filter (lambda (y)
                            (unless (find-if #'zerop
                                             (mapcar (lambda (x)
                                                       (aif (safety-aref (make-vector x y)) it 0))
                                                     range-x))
                              y))
                          (sort (remove-duplicates
                                  (mapcar #'coordinate-y (get-current-coordinates)))
                                #'<))
                  (rest range-y))
         (y (first range-y) (first range-y)))
     ((null range-y))
     (dolist (x range-x)
       (delete-rectangles (make-vector x y)))
     (dorange (y y 1)
       (dolist (x range-x)
         (move-rectangle (make-vector x (1- y))
                         (make-vector 0 1))))))

;; }}}
;; move-rectangle {{{

(defmacro move-rectangle (coordinates direction)
  `(let* ((coordinates (mklist ,coordinates))
          (coordinates-copy (mapcar (lambda (coordinate)
                                      (list coordinate (safety-aref coordinate)))
                                    coordinates)))
     (dolist (coordinate coordinates)
       (set-board coordinate 0))
     (dolist (coordinate (mklist ,coordinates))
       (with-coordinates (coordinate ,direction)
         (set-board (vector+ r1 r2)
                    (second (find-if (lambda (copy)
                                       (vector= r1 (first copy)))
                                     coordinates-copy)))
         (itemmove *canvas*
                   (safety-aref (vector+ r1 r2))
                   (* x2 *cell-width*)
                   (* y2 *cell-width*))))))

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
            (move-rectangle current-coordinates ,direction)
            (asetf (tetromino-coordinate-origin *current-tetromino*)
                   (vector+ it ,direction)))
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
       (asetf (tetromino-coordinates *current-tetromino*)
              (mapcar (lambda (coordinate)
                        (vector-rotate coordinate (/ pi 2)))
                      it))
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

(print *tetrominos*)
(with-ltk ()
  (bind *tk* "<Control-c>" (ilambda (event) (setf *exit-mainloop* t)))
  (setf *canvas* (pack (make-instance 'canvas
                                      :width (* *cell-width* *board-width*)
                                      :height (* *cell-width* *board-height*))))
  (force-focus *canvas*)
  (draw-board)
  (set-new-current-tetromino)
  (bind-keypress #\h (try-move (make-vector -1 0)))
  (bind-keypress #\j (try-move (make-vector 0 1)))
  (bind-keypress #\k (try-rotate))
  (bind-keypress #\l (try-move (make-vector 1 0)))
  (main))
