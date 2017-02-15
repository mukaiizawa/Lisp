(require :ltk *module-ltk*)
(require :coordinate-manager *module-coordinate-manager*)
(require :stdlib *module-stdlib*)

(defstruct puyo-canvas
  widget puyo-puyo width height)

(defmacro instance-vars (&rest vars)
  `(progn
     ,@(mapcar (lambda (x)
                 `(defvar ,x))
               vars)))

(instance-vars
  cell-size drawing-interval end-game?
  frame text-area canvas canvas-next
  puyos canvas-puyos curr-puyos next-puyos)

(defstruct puyo id sym color point)

(defstruct puyo-canavs width height widget)

(defmethod width ((pc puyo-canvas))
  (puyo-canvas-width pc))

(defmethod height ((pc puyo-canvas))
  (puyo-canvas-height pc))

(defmethod widget ((pc puyo-canvas))
  (puyo-canvas-widget pc))

;; cutting-puyo {{{

(defun cutting-puyo ()
  (dolist (puyo curr-puyos)
    (while (puttable? (vector+ (puyo-point puyo) +vector-top+))
      (with-coordinates ((vector+ (puyo-point puyo) +vector-top+))
        (setf (puyo-point puyo) (vector+ (puyo-point puyo) +vector-top+))
        (itemmove (widget canvas)
                  (puyo-id puyo)
                  0
                  (* y1 cell-size))))))

;; }}}
;; move-bottom {{{

(defun move-bottom ()
  (while (movable? +vector-top+)
    (move-puyos +vector-top+)))

;; }}}
;; puttable? {{{

(defun puttable? (points)
  (every (lambda (point)
           (and (in-canvas? point)
                (not
                  (some (lambda (puyo)
                          (vector= (puyo-point puyo) point))
                        canvas-puyos))))
         (mklist points)))

;; }}}
;; in-canvas? {{{

(defun in-canvas? (p)
  (with-coordinates (p)
    (and (<= 0 x1) (< x1 (width canvas))
         (<= 0 y1) (< y1 (height canvas)))))

;; }}}
;; get-rotated-outer-puyo-point {{{

(defun get-rotated-outer-puyo-point (rot-dir)
  (let* ((axis-puyo (first curr-puyos))
         (outer-puyo (second curr-puyos))
         (vector-axis (puyo-point axis-puyo))
         (vector-outer (copy-coordinate (puyo-point outer-puyo))))
    (vector+ vector-axis
             (vector-rotate (vector- (puyo-point outer-puyo)
                                     (puyo-point axis-puyo))
                            (if (eq rot-dir 'right)
                              (/ pi 2)
                              (- (/ pi 2)))))))

;; }}}
;; rotate-puyos {{{

(defun rotate-puyos (rot-dir)
  (let* ((outer-puyo (second curr-puyos))
         (vector-outer (copy-coordinate (puyo-point outer-puyo))))
    (setf (puyo-point outer-puyo) (get-rotated-outer-puyo-point rot-dir))
    (with-coordinates ((vector- (puyo-point outer-puyo) vector-outer))
      (itemmove (widget canvas)
                (puyo-id outer-puyo)
                (* x1 cell-size)
                (* y1 cell-size)))))

;; }}}
;; rotatable? {{{

(defun rotatable? (rot-dir)
  (puttable? (get-rotated-outer-puyo-point rot-dir)))

;; }}}
;; try-rotate {{{

(defun try-rotate (rot-dir)
  (if (rotatable? rot-dir)
    (rotate-puyos rot-dir)))

;; }}}
;; move-puyos {{{

(defun move-puyos (dir)
  (with-coordinates (dir)
    (dolist (puyo curr-puyos)
      (setf (puyo-point puyo) (vector+ (puyo-point puyo) dir))
      (itemmove (widget canvas)
                (puyo-id puyo)
                (* x1 cell-size)
                (* y1 cell-size)))))

;; }}}
;; movable? {{{

(defun movable? (dir)
  (puttable? (mapcar (lambda (puyo)
                       (vector+ dir (puyo-point puyo)))
                     curr-puyos)))

;; }}}
;; try-move {{{

(defun try-move (dir)
  (if (movable? dir)
    (move-puyos dir)
    (when (vector= dir +vector-top+)
      (cutting-puyo)
      (setf canvas-puyos (append canvas-puyos curr-puyos))
      (put-next-puyos))))

;; }}}
;; put-puyo {{{

(defmethod put-puyo ((pc puyo-canvas) (puyo puyo) (point coordinate))
  (with-coordinates (point)
    (setf (puyo-point puyo) point
          (puyo-id puyo) (create-oval (widget pc)
                                      (* x1 cell-size)
                                      (* y1 cell-size)
                                      (+ (* cell-size x1) cell-size)
                                      (+ (* cell-size y1) cell-size)))
    (itemconfigure (widget pc)
                   (puyo-id puyo)
                   "fill"
                   (puyo-color puyo))
    (values)))

;; }}}
;; put-next-puyos {{{

(defun put-next-puyos ()
  (setf curr-puyos (loop for i from 0 to 1 collect
                         (let ((puyo (nth i next-puyos)))
                           (itemdelete (widget canvas-next) (puyo-id puyo))
                           (put-puyo canvas puyo (make-vector 2 i))
                           puyo))
        next-puyos (create-next-puyos)))

;; }}}
;; create-next-puyos {{{

(defun create-next-puyos ()
  (loop for i from 0 to 1 collect
        (let ((puyo (copy-puyo (nth (random (length puyos) (make-random-state t))
                                    puyos))))
          (put-puyo canvas-next puyo (make-vector 0 i))
          puyo)))

;; }}}
;; init-canvas {{{

(defmethod init-canvas ((pc puyo-canvas))
  (let* ((widget (widget pc))
         (width (width pc))
         (height (height pc))
         (canvas-width (* cell-size width))
         (canvas-height (* cell-size height)))
    (itemconfigure widget
                   (create-rectangle widget 0 0 canvas-width canvas-height)
                   "fill" "#c0c0c0")
    (dorange (x 0 width)
      (let1 (dx (* x cell-size))
        (create-line widget (list dx 0 dx canvas-height))))
    (dorange (y 0 height)
      (let1 (dy (* y cell-size))
        (create-line widget (list 0 dy canvas-width dy))))))

;; }}}
;; init {{{

(defun init ()
  (setf cell-size 30
        drawing-interval 500
        end-game? nil
        frame (pack (make-instance 'frame))
        text-area (pack
                    (make-text frame :width nil :height 2)
                    :side :bottom)
        canvas (make-puyo-canvas
                 :width 6
                 :height 12
                 :widget (pack
                           (make-canvas frame
                                        :width (* cell-size 10)
                                        :height (* cell-size 20))
                           :side :left))
        canvas-next (make-puyo-canvas
                      :width 1
                      :height 2
                      :widget (pack
                                (make-canvas frame
                                             :width (* cell-size 1)
                                             :height (* cell-size 2))
                                :side :left))
        puyos (list
                (make-puyo :sym 'red :color "#ff0000")
                (make-puyo :sym 'green :color "#00ff00")
                (make-puyo :sym 'blue :color "#0000ff")
                (make-puyo :sym 'purple :color "#ff00ff"))
        canvas-puyos nil
        curr-puyos nil
        next-puyos (create-next-puyos))
  (init-canvas canvas)
  (init-canvas canvas-next)
  (put-next-puyos))

;; }}}
;; bind-key {{{

(defmacro bind-key (key &body body)
  `(bind *tk* ,(mkstr "<KeyPress-" key ">")
         (ilambda (event)
           (unless end-game?
             ,@body))))

;; }}}
;; bind-keys {{{

(defmacro bind-keys ()
  `(progn
     (bind *tk* "<Control-c>" (ilambda (event) (setf *exit-mainloop* t)))
     (bind-key #\h (try-move +vector-left+))
     (bind-key #\j (move-bottom))
     (bind-key #\k (try-rotate 'right))
     (bind-key #\l (try-move +vector-right+))))

;; }}}
;; main {{{

(defun main ()
  (after-time drawing-interval
              (lambda ()
                (unless end-game?
                  (try-move +vector-top+)
                  (main)))))

;; }}}

(defun puyo-puyo ()
  (with-ltk ()
    (init)
    (bind-keys)
    (force-focus frame)
    (main)))

(puyo-puyo)
