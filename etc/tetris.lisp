
(require :ltk *module-ltk*)
(require :coordinate-manager *module-coordinate-manager*)
(require :stdlib *module-stdlib*)

;; game config
(defparameter *cell-width* 30)
(defparameter *board-width* 10)
(defparameter *board-height* 20)

;; ltk widgets
(defparameter *widgets* nil)

;; global parameter
(defparameter *tetrominos* nil)
(defparameter *score* 0)
(defparameter *game-over?* nil) 
(defparameter *drawing-interval* 500)

(defstruct tetris-widgets
  frame canvas-left canvas-right text-area)

(defstruct tetris-canvas
  widget board tetromino width height)

(defstruct tetromino
  shape color coordinate-origin coordinates)

;; deftetromino {{{

(defmacro deftetromino (shape color &rest coordinates)
  `(push ,(make-tetromino 
            :shape shape
            :color color 
            :coordinate-origin nil
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

;; set-board {{{

(defmethod set-board ((c tetris-canvas) (r coordinate) (val number))
  (with-coordinates (r)
    (setf (aref (tetris-canvas-board c) x1 y1)
          val)))

;; }}}
;; safety-aref {{{

(defmethod safety-aref ((c tetris-canvas) x &optional y)
  (with-coordinates ((if y (make-vector x y) x))
    (and (<= 0 x1) (< x1 (tetris-canvas-width c))
         (<= 0 y1) (< y1 (tetris-canvas-height c))
         (aref (tetris-canvas-board c) x1 y1))))

;; }}}
;; draw-rectangle {{{

(defmethod draw-rectangle ((canvas canvas) (r coordinate) (color string))
  (with-coordinates (r)
    (let1 (item (create-rectangle canvas
                                  (* x1 *cell-width*)
                                  (* y1 *cell-width*)
                                  (+ (* *cell-width* x1) *cell-width*)
                                  (+ (* *cell-width* y1) *cell-width*)))
      (itemconfigure canvas item "fill" color)
      (values item))))

;; }}}
;; draw-tetromino {{{

(defmethod draw-tetromino ((c tetris-canvas))
  (print (tetris-canvas-board c))
  (let1 (tetromino (tetris-canvas-tetromino c))
    (dolist (coordinate (to-absolute-coordinates tetromino))
      (when (safety-aref c coordinate)
        (set-board c
                   coordinate
                   (draw-rectangle (tetris-canvas-widget c)
                                   coordinate
                                   (tetromino-color tetromino)))))))

;; }}}
;; draw-board {{{

(defmethod draw-board ((c tetris-canvas))
  (dorange (x 0 (1- (tetris-canvas-width c)))
    (dorange (y 0 (1- (tetris-canvas-height c)))
      (draw-rectangle (tetris-canvas-widget c)
                      (make-vector x y)
                      "#a0a0a0"))))

;; }}}
;; update-drawing-interval {{{

(defun update-drawing-interval (msec)
  (when (>= *drawing-interval* 150)
    (decf *drawing-interval* msec)))

;; }}}
;; update-score {{{

(defun update-score (score)
  (incf *score* (* score 1000))
  (clear-text (tetris-widgets-text-area *widgets*))
  (append-text (tetris-widgets-text-area *widgets*)
               (mkstr "Score: " *score*)))

;; }}}
;; to-absolute-coordinates {{{

(defmethod to-absolute-coordinates ((tetromino tetromino))
  (mapcar (lambda (coordinate)
            (vector+ (tetromino-coordinate-origin tetromino)
                     coordinate))
          (tetromino-coordinates tetromino)))

;; }}}
;; get-next-coordinates {{{ 

(defmethod get-next-coordinates ((direction coordinate))
  (mapcar (lambda (current-coordinate)
            (vector+ current-coordinate direction))
          (to-absolute-coordinates (tetris-canvas-tetromino (tetris-widgets-canvas-left *widgets*)))))

;; }}}
;; get-rotate-coordinates {{{

(defun get-rotate-coordinates ()
  (mapcar (lambda (current-coordinate)
            (vector+ (tetromino-coordinate-origin
                       (tetris-canvas-tetromino (tetris-widgets-canvas-left *widgets*)))
                     (vector-rotate current-coordinate (/ pi 2))))
          (tetromino-coordinates (tetris-canvas-tetromino (tetris-widgets-canvas-left *widgets*)))))

;; }}}
;; get-next-tetromino {{{

(defmethod get-next-tetromino ((c tetris-canvas) &optional new-tetromino)
  (let1 (copy (copy-tetromino (or new-tetromino (nth (random (length *tetrominos*)) *tetrominos*))))
    (setf (tetromino-coordinate-origin copy) 
          (make-vector (/ (tetris-canvas-width c) 2)
                       (- (most #'- (mapcar #'coordinate-y (tetromino-coordinates copy))))))
    copy))

;; }}}
;; set-new-tetromino {{{

(defun set-new-tetromino ()
  (setf (tetris-canvas-tetromino (tetris-widgets-canvas-left *widgets*))
        (get-next-tetromino (tetris-widgets-canvas-left *widgets*)
                            (tetris-canvas-tetromino (tetris-widgets-canvas-right *widgets*)))
        (tetris-canvas-tetromino (tetris-widgets-canvas-right *widgets*))
        (get-next-tetromino (tetris-widgets-canvas-right *widgets*)))
  (update-drawing-interval 5)
  (update-score 1)
  (clear-canvas (tetris-widgets-canvas-right *widgets*))
  (draw-tetromino (tetris-widgets-canvas-right *widgets*))
  (draw-tetromino (tetris-widgets-canvas-left *widgets*)))

;; }}}
;; delete-rectangles {{{

(defmethod delete-rectangles ((c tetris-canvas) coordinates)
  (dolist (coordinate (mklist coordinates))
    (awhen (safety-aref c coordinate)
      (itemdelete (tetris-canvas-widget c) it)
      (set-board c coordinate 0))))

;; }}}
;; clear-canvas {{{

(defmethod clear-canvas ((c tetris-canvas))
  (dotimes (x (tetris-canvas-width c))
    (dotimes (y (tetris-canvas-height c))
      (aif (safety-aref c (make-vector x y))
        (itemdelete (tetris-canvas-widget c) it)
        (set-board c (make-vector x y) 0)))))

;; }}}
;; delete-line {{{

(defun delete-lines ()
  (do* ((range-x (iota 0 (1- *board-width*)))
        (range-y (remove-if (lambda (y)
                              (not (every (lambda (x)
                                            (aand (safety-aref (tetris-widgets-canvas-left *widgets*) x y)
                                                  (not (zerop it))))
                                          range-x)))
                            (sort (remove-duplicates
                                    (mapcar #'coordinate-y (to-absolute-coordinates (tetris-canvas-tetromino (tetris-widgets-canvas-left *widgets*)))))
                                  #'<))
                 (rest range-y))
        (y (first range-y) (first range-y)))
    ((null range-y))
    (update-drawing-interval 10)
    (update-score (* (expt 2 (length range-y))))
    (dolist (x range-x)
      (delete-rectangles (tetris-widgets-canvas-left *widgets*) (make-vector x y)))
    (dorange (y y 1)
      (dolist (x range-x)
        (move-rectangle (make-vector x (1- y))
                        (make-vector 0 1))))))

;; }}}
;; move-rectangle {{{

(defun move-rectangle (coordinates direction)
  (let* ((c (tetris-widgets-canvas-left *widgets*))
         (coordinates (mklist coordinates))
         (coordinates-move-before (mapcar (lambda (coordinate)
                                            (list coordinate (safety-aref c coordinate)))
                                          coordinates)))
    (dolist (coordinate coordinates)
      (set-board c coordinate 0))
    (dolist (coordinate (mklist coordinates))
      (with-coordinates (coordinate direction)
        (set-board c
                   (vector+ r1 r2)
                   (second (find-if (lambda (copy)
                                      (vector= r1 (first copy)))
                                    coordinates-move-before)))
        (itemmove (tetris-canvas-widget (tetris-widgets-canvas-left *widgets*))
                  (safety-aref c (vector+ r1 r2))
                  (* x2 *cell-width*)
                  (* y2 *cell-width*))))))

;; }}}
;; movable? {{{

(defun movable? ()
  (lambda (next-coordinate)
    (or (aand (safety-aref (tetris-widgets-canvas-left *widgets*) next-coordinate) (= it 0))
        (find-if (lambda (current-coordinate)
                   (vector= current-coordinate next-coordinate))
                 (to-absolute-coordinates (tetris-canvas-tetromino (tetris-widgets-canvas-left *widgets*)))))))

;; }}}
;; try-move {{{

(defmethod try-move ((direction coordinate))
  (let ((current-coordinates (to-absolute-coordinates (tetris-canvas-tetromino (tetris-widgets-canvas-left *widgets*))))
        (next-coordinates (get-next-coordinates direction)))
    (cond ((every (movable?) next-coordinates)
           (move-rectangle current-coordinates direction)
           (asetf (tetromino-coordinate-origin (tetris-canvas-tetromino (tetris-widgets-canvas-left *widgets*)))
                  (vector+ it direction)))
          ((vector= direction (make-vector 0 1))
           (delete-lines)
           (set-new-tetromino)
           (when (find-if (complement (movable?))
                          (get-next-coordinates (make-vector 0 1)))
             (setq *game-over?* t)))
          (t nil))))

;; }}}
;; try-rotate {{{

(defun try-rotate ()
  (let ((current-coordinates (to-absolute-coordinates (tetris-canvas-tetromino (tetris-widgets-canvas-left *widgets*))))
        (rotate-coordinates (get-rotate-coordinates)))
    (when (every (movable?) rotate-coordinates)
      (delete-rectangles (tetris-widgets-canvas-left *widgets*) current-coordinates)
      (asetf (tetromino-coordinates (tetris-canvas-tetromino (tetris-widgets-canvas-left *widgets*)))
             (mapcar (lambda (coordinate)
                       (vector-rotate coordinate (/ pi 2)))
                     it))
      (draw-tetromino (tetris-widgets-canvas-left *widgets*)))))

;; }}}
;; change-tetromino {{{

(defun change-tetromino ()
  (let* ((curr-tetromino (tetris-canvas-tetromino (tetris-widgets-canvas-left *widgets*)))
         (next-tetromino (tetris-canvas-tetromino (tetris-widgets-canvas-right *widgets*))))
    (when 'can-chage?-fixme
      (delete-rectangles (tetris-widgets-canvas-left *widgets*)
                         (to-absolute-coordinates curr-tetromino))
      (setf 
        (tetris-canvas-tetromino (tetris-widgets-canvas-left *widgets*))
        next-tetromino
        (tetris-canvas-tetromino (tetris-widgets-canvas-right *widgets*))
        curr-tetromino
        (tetromino-coordinate-origin (tetris-canvas-tetromino (tetris-widgets-canvas-left *widgets*)))
        (tetromino-coordinate-origin (tetris-canvas-tetromino (tetris-widgets-canvas-right *widgets*)))
        (tetromino-coordinate-origin (tetris-canvas-tetromino (tetris-widgets-canvas-right *widgets*)))
        (make-vector (/ (tetris-canvas-width (tetris-widgets-canvas-right *widgets*)) 2)
                     (- (most #'- (mapcar #'coordinate-y (tetromino-coordinates (tetris-canvas-tetromino (tetris-widgets-canvas-right *widgets*))))))))
      (clear-canvas (tetris-widgets-canvas-right *widgets*))
      (draw-tetromino (tetris-widgets-canvas-right *widgets*))
      (draw-tetromino (tetris-widgets-canvas-left *widgets*)))))

;; }}}
;; main {{{

(defun main ()
  (after-time *drawing-interval*
              (lambda ()
                (unless *game-over?*
                  (try-move (make-vector 0 1))
                  (main)))))

;; }}}
;; bind-keypress {{{

(defmacro bind-keypress (key &body body)
  `(bind *tk* ,(mkstr "<KeyPress-" key ">")
         (ilambda (event)
           (unless *game-over?*
             ,@body))))

;; }}}
;; initialize-widgets! {{{

(defmethod initialize-widgets! ((w tetris-widgets))
  (setf (tetris-widgets-frame w)
        (pack (make-instance 'frame))
        (tetris-widgets-text-area w)
        (pack (make-text (tetris-widgets-frame w) :width nil :height 2) :side :bottom)
        (tetris-widgets-canvas-left w)
        (create-tetris-canvas (tetris-widgets-frame w) *board-width* *board-height*)
        (tetris-widgets-canvas-right w)
        (create-tetris-canvas (tetris-widgets-frame w) 4 4)))

;; }}}
;; create-tetris-canvas {{{

(defun create-tetris-canvas (frame width height)
  (let ((canvas (make-tetris-canvas)))
    (setf (tetris-canvas-widget canvas)
          (pack (make-canvas frame :width (* *cell-width* *board-width*) :height (* *cell-width* *board-height*)) :side :left)
          (tetris-canvas-board canvas)
          (make-array (list width height) :initial-element 0)
          (tetris-canvas-width canvas)
          width
          (tetris-canvas-height canvas)
          height
          (tetris-canvas-tetromino canvas)
          (get-next-tetromino canvas))
    canvas))

;; }}}

(with-ltk ()
  (bind *tk* "<Control-c>" (ilambda (event) (setf *exit-mainloop* t)))
  (setq *widgets* (make-tetris-widgets))
  (initialize-widgets! *widgets*)
  (force-focus (tetris-widgets-frame *widgets*))
  (draw-board (tetris-widgets-canvas-left *widgets*))
  (draw-board (tetris-widgets-canvas-right *widgets*))
  (set-new-tetromino)
  (bind-keypress #\o (change-tetromino))
  (bind-keypress #\h (try-move +vector-left+))
  (bind-keypress #\j (try-move +vector-top+))
  (bind-keypress #\k (try-rotate))
  (bind-keypress #\l (try-move +vector-right+))
  (main))

