
(require :ltk *module-ltk*)
(require :coordinate-manager *module-coordinate-manager*)
(require :stdlib *module-stdlib*)

;; game config
(defparameter *cell-width* 30)
(defparameter *board-width* 10)
(defparameter *board-height* 20)
(defparameter *drawing-interval* 500)
(defparameter *score* 0)

;; ltk widget
(defparameter *frame* nil)
(defparameter *canvas-left* nil)
(defparameter *canvas-right* nil)
(defparameter *text-area* nil)
(defparameter *board* (make-array (list *board-width* *board-height*) :initial-element 0))

;; global parameter
(defparameter *tetrominos* nil)
(defparameter *current-tetromino* nil)
(defparameter *next-tetromino* nil)
(defparameter *game-over?* nil) 

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

;; set-board {{{

(defun set-board (coordinate val)
  (with-coordinates (coordinate)
    (setf (aref *board* x1 y1)
          val)))

;; }}}
;; safety-aref {{{

(defun safety-aref (x &optional y)
  (with-coordinates ((if (coordinate-p x)
                       x
                       (make-vector x y)))
    (and (<= 0 x1) (< x1 *board-width*)
         (<= 0 y1) (< y1 *board-height*)
         (aref *board* x1 y1))))

;; }}}
;; draw-rectangle {{{

(defun draw-rectangle (canvas coordinate color)
  (with-coordinates (coordinate)
    (let1 (item (create-rectangle canvas
                                  (* x1 *cell-width*)
                                  (* y1 *cell-width*)
                                  (+ (* *cell-width* x1) *cell-width*)
                                  (+ (* *cell-width* y1) *cell-width*)))
      (itemconfigure *canvas-right* item "fill" color)
      (values item))))

;; }}}
;; draw-tetromino {{{

(defun draw-tetromino (canvas coordinates)
  (dolist (coordinate coordinates)
    (when (safety-aref coordinate)
      (set-board coordinate
                 (draw-rectangle canvas
                                 coordinate
                                 (tetromino-color *current-tetromino*))))))

;; }}}
;; draw-board {{{

(defun draw-board (canvas)
  (dorange (x 0 *board-width*)
    (dorange (y 0 *board-height*)
      (draw-rectangle canvas (make-vector x y) "#a0a0a0"))))

;; }}}
;; update-drawing-interval {{{

(defun update-drawing-interval (msec)
  (when (>= *drawing-interval* 200)
    (decf *drawing-interval* msec)))

;; }}}
;; update-score {{{

(defun update-score (score)
  (incf *score* (* score 1000))
  (clear-text *text-area*)
  (append-text *text-area* (mkstr "Score: " *score*)))

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
            (vector+ current-coordinate direction))
          (get-current-coordinates)))

;; }}}
;; get-rotate-coordinates {{{

(defun get-rotate-coordinates ()
  (mapcar (lambda (current-coordinate)
            (vector+ (tetromino-coordinate-origin *current-tetromino*)
                     (vector-rotate current-coordinate (/ pi 2))))
          (tetromino-coordinates *current-tetromino*)))

;; }}}
;; get-random-tetromino {{{

(defun get-random-tetromino ()
  (copy-tetromino
    (nth (random (length *tetrominos*))
         *tetrominos*)))

;; }}}
;; set-new-current-tetromino {{{

(defun set-new-current-tetromino ()
  (setf *current-tetromino* (copy-tetromino *next-tetromino*)
        *next-tetromino* (get-random-tetromino))
  (update-drawing-interval 5)
  (update-score 1)
  (draw-tetromino *canvas-right* (tetromino-coordinates *next-tetromino*))
  (draw-tetromino *canvas-left* (get-current-coordinates)))

;; }}}
;; delete-rectangles {{{

(defun delete-rectangles (coordinates)
  (dolist (coordinate (mklist coordinates))
    (awhen (safety-aref coordinate)
      (itemdelete *canvas-left* it)
      (set-board coordinate 0))))

;; }}}
;; delete-line {{{

(defun delete-lines ()
  (do* ((range-x (iota 0 (1- *board-width*)))
        (range-y (remove-if (lambda (y)
                              (not (every (lambda (x)
                                            (aand (safety-aref x y)
                                                  (not (zerop it))))
                                          range-x)))
                            (sort (remove-duplicates
                                    (mapcar #'coordinate-y (get-current-coordinates)))
                                  #'<))
                 (rest range-y))
        (y (first range-y) (first range-y)))
    ((null range-y))
    (update-drawing-interval 10)
    (update-score (* (expt 2 (length range-y))))
    (dolist (x range-x)
      (delete-rectangles (make-vector x y)))
    (dorange (y y 1)
      (dolist (x range-x)
        (move-rectangle (make-vector x (1- y))
                        (make-vector 0 1))))))

;; }}}
;; move-rectangle {{{

(defun move-rectangle (coordinates direction)
  (let* ((coordinates (mklist coordinates))
         (coordinates-copy (mapcar (lambda (coordinate)
                                     (list coordinate (safety-aref coordinate)))
                                   coordinates)))
    (dolist (coordinate coordinates)
      (set-board coordinate 0))
    (dolist (coordinate (mklist coordinates))
      (with-coordinates (coordinate direction)
        (set-board (vector+ r1 r2)
                   (second (find-if (lambda (copy)
                                      (vector= r1 (first copy)))
                                    coordinates-copy)))
        (itemmove *canvas-left*
                  (safety-aref (vector+ r1 r2))
                  (* x2 *cell-width*)
                  (* y2 *cell-width*))))))

;; }}}
;; movable? {{{

(defun movable? ()
  (lambda (next-coordinate)
    (or (aand (safety-aref next-coordinate) (= it 0))
        (find-if (lambda (current-coordinate)
                   (vector= current-coordinate next-coordinate))
                 (get-current-coordinates)))))

;; }}}
;; try-move {{{

(defun try-move (direction)
  (let ((current-coordinates (get-current-coordinates))
        (next-coordinates (get-next-coordinates direction)))
    (cond ((every (movable?) next-coordinates)
           (move-rectangle current-coordinates direction)
           (asetf (tetromino-coordinate-origin *current-tetromino*)
                  (vector+ it direction)))
          ((vector= direction (make-vector 0 1))
           (delete-lines)
           (set-new-current-tetromino)
           (when (find-if (complement (movable?))
                          (get-next-coordinates (make-vector 0 1)))
             (setq *game-over?* t)))
          (t nil))))

;; }}}
;; try-rotate {{{

(defun try-rotate ()
  (let ((current-coordinates (get-current-coordinates))
        (rotate-coordinates (get-rotate-coordinates)))
    (when (every (movable?) rotate-coordinates)
      (delete-rectangles current-coordinates)
      (asetf (tetromino-coordinates *current-tetromino*)
             (mapcar (lambda (coordinate)
                       (vector-rotate coordinate (/ pi 2)))
                     it))
      (draw-tetromino *canvas-left* (get-current-coordinates)))))

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
           (print *board*)
           (unless *game-over?*
             ,@body))))

;; }}}

(with-ltk ()
  (bind *tk* "<Control-c>" (ilambda (event) (setf *exit-mainloop* t)))
  (setf *frame* (pack (make-instance 'frame))
        *canvas-left* (pack (make-canvas *frame*
                                         :width (* *cell-width* *board-width*)
                                         :height (* *cell-width* *board-height*))
                            :side :left)
        *canvas-right* (pack (make-canvas *frame*
                                          :width (* *cell-width* 4)
                                          :height (* *cell-width* 4))
                             :side :left)
        *text-area* (pack (make-text *frame* :width nil :height 2)
                          :side :bottom)
        *next-tetromino* (get-random-tetromino))
  (force-focus *canvas-left*)
  (draw-board *canvas-left*)
  (draw-board *canvas-right*)
  (set-new-current-tetromino)
  (bind-keypress #\h (try-move (make-vector -1 0)))
  (bind-keypress #\j (try-move (make-vector 0 1)))
  (bind-keypress #\k (try-rotate))
  (bind-keypress #\l (try-move (make-vector 1 0)))
  (main))

