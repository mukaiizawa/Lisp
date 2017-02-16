(require :ltk *module-ltk*)
(require :coordinate-manager *module-coordinate-manager*)
(require :stdlib *module-stdlib*)

(defmacro instance-vars (&rest vars)
  `(progn
     ,@(mapcar (lambda (x)
                 `(defvar ,x))
               vars)))

(instance-vars
  drawing-interval end-game?
  cell-size width height
  frame text-area
  puyo-origin player1 player2)

(defstruct puyo
  id sym color point)

(defstruct player
  canvas canvas-next puyos curr-puyos next-puyos)

(defmacro with-player ((num) &body body)
  `(ilet* ((player (symbol-value (mksym 'player ,num)))
           (canvas (player-canvas player))
           (canvas-next (player-canvas-next player))
           (puyos (player-puyos player))
           (curr-puyos (player-curr-puyos player))
           (next-puyos (player-next-puyos player)))
     ,@body))

;; cutting-puyo {{{

(defun cutting-puyo (num)
  (with-player (num)
    (dolist (puyo curr-puyos)
      (while (puttable? num (vector+ (puyo-point puyo) +vector-top+))
        (with-coordinates ((vector+ (puyo-point puyo) +vector-top+))
          (setf (puyo-point puyo) r1)
          (itemmove canvas
                    (puyo-id puyo)
                    0
                    (* y1 cell-size)))))))

;; }}}
;; move-bottom {{{

(defun move-bottom (num)
  (while (movable? num +vector-top+)
    (move-puyos num +vector-top+)))

;; }}}
;; puttable? {{{

(defun puttable? (num points)
  (with-player (num)
    (every (lambda (point)
             (and (in-canvas? point)
                  (not
                    (some (lambda (puyo)
                            (vector= (puyo-point puyo) point))
                          puyos))))
           (mklist points))))

;; }}}
;; in-canvas? {{{

(defun in-canvas? (p)
  (with-coordinates (p)
    (and (<= 0 x1) (< x1 width)
         (<= 0 y1) (< y1 height))))

;; }}}
;; get-rotated-outer-puyo-point {{{

(defun get-rotated-outer-puyo-point (num rot-dir)
  (with-player (num)
    (let* ((axis-puyo (first curr-puyos))
           (outer-puyo (second curr-puyos))
           (vector-axis (puyo-point axis-puyo))
           (vector-outer (copy-coordinate (puyo-point outer-puyo))))
      (vector+ vector-axis
               (vector-rotate (vector- (puyo-point outer-puyo)
                                       vector-outer)
                              (if (eq rot-dir 'right)
                                (/ pi 2)
                                (- (/ pi 2))))))))

;; }}}
;; rotate-puyos {{{

(defun rotate-puyos (num rot-dir)
  (with-player (num)
    (let* ((outer-puyo (second curr-puyos))
           (vector-outer (copy-coordinate (puyo-point outer-puyo))))
      (setf (puyo-point outer-puyo) (get-rotated-outer-puyo-point num rot-dir))
      (with-coordinates ((vector- (puyo-point outer-puyo) vector-outer))
        (itemmove canvas
                  (puyo-id outer-puyo)
                  (* x1 cell-size)
                  (* y1 cell-size))))))

;; }}}
;; rotatable? {{{

(defun rotatable? (num rot-dir)
  (with-player (num)
    (puttable? num (get-rotated-outer-puyo-point num rot-dir))))

;; }}}
;; try-rotate {{{

(defun try-rotate (num rot-dir)
  (with-player (num)
    (if (rotatable? num rot-dir)
      (rotate-puyos num rot-dir))))

;; }}}
;; move-puyos {{{

(defun move-puyos (num dir)
  (with-player (num)
    (with-coordinates (dir)
      (dolist (puyo curr-puyos)
        (setf (puyo-point puyo) (vector+ (puyo-point puyo) dir))
        (itemmove canvas
                  (puyo-id puyo)
                  (* x1 cell-size)
                  (* y1 cell-size))))))

;; }}}
;; movable? {{{

(defun movable? (num dir)
  (with-player (num)
    (puttable? num (mapcar (lambda (puyo)
                             (vector+ dir (puyo-point puyo)))
                           curr-puyos))))

;; }}}
;; try-move {{{

(defun try-move (num dir)
  (with-player (num)
    (if (movable? num dir)
      (move-puyos num dir)
      (when (vector= dir +vector-top+)
        (cutting-puyo num)
        (setf puyos (append puyos curr-puyos))
        (put-next-puyos num)))))

;; }}}
;; put-puyo {{{

(defmethod put-puyo (num canvas (puyo puyo) (point coordinate))
  (with-player (num)
    (with-coordinates (point)
      (setf (puyo-point puyo) point
            (puyo-id puyo) (create-oval canvas
                                        (* x1 cell-size)
                                        (* y1 cell-size)
                                        (+ (* cell-size x1) cell-size)
                                        (+ (* cell-size y1) cell-size)))
      (itemconfigure canvas
                     (puyo-id puyo)
                     "fill"
                     (puyo-color puyo))
      (values))))

;; }}}
;; put-next-puyos {{{

(defun put-next-puyos (num)
  (with-player (num)
    (setf curr-puyos (loop for i from 0 to 1 collect
                           (let ((puyo (nth i next-puyos)))
                             (itemdelete canvas-next (puyo-id puyo))
                             (put-puyo num canvas puyo (make-vector 2 i))
                             puyo))
          next-puyos (create-next-puyos num))))

;; }}}
;; create-next-puyos {{{

(defun create-next-puyos (num)
  (with-player (num)
    (loop for i from 0 to 1 collect
          (let ((puyo (copy-puyo (nth (random (length puyo-origin) (make-random-state t))
                                      puyo-origin))))
            (put-puyo num canvas-next puyo (make-vector 0 i))
            puyo))))

;; }}}
;; init-canvas {{{

(defmethod init-canvas (canvas)
  (let* ((canvas-width (* cell-size width))
         (canvas-height (* cell-size height)))
    (itemconfigure canvas
                   (create-rectangle canvas 0 0 canvas-width canvas-height)
                   "fill" "#c0c0c0")
    (dorange (x 0 width)
      (let1 (dx (* x cell-size))
        (create-line canvas (list dx 0 dx canvas-height))))
    (dorange (y 0 height)
      (let1 (dy (* y cell-size))
        (create-line canvas (list 0 dy canvas-width dy))))))

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
        width 6
        height 12
        player1 (make-player
                  :canvas (pack
                            (make-canvas frame
                                         :width (* cell-size width)
                                         :height (* cell-size height))
                            :side :left)
                  :canvas-next (pack
                                 (make-canvas frame
                                              :width (* cell-size 1)
                                              :height (* cell-size 2))
                                 :side :left)
                  :puyos nil
                  :curr-puyos nil
                  :next-puyos (create-next-puyos 1))
        puyo-origin (list
                (make-puyo :sym 'red :color "#ff0000")
                (make-puyo :sym 'green :color "#00ff00")
                (make-puyo :sym 'blue :color "#0000ff")
                (make-puyo :sym 'purple :color "#ff00ff")))
  (init-canvas (player-canvas player1))
  (put-next-puyos 1)
  ; (put-next-puyos 2)
  )

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
     (bind-key #\h (try-move 1 +vector-left+))
     (bind-key #\j (move-bottom 1))
     (bind-key #\k (try-rotate 1 'right))
     (bind-key #\l (try-move 1 +vector-right+))))

;; }}}
;; main {{{

(defun main ()
  (after-time drawing-interval
              (lambda ()
                (unless end-game?
                  (try-move 1 +vector-top+)
                  ; (try-move 2 +vector-top+)
                  (main)))))

;; }}}

(defun puyo-puyo ()
  (with-ltk ()
    (init)
    (bind-keys)
    (force-focus frame)
    (main)))

(puyo-puyo)
