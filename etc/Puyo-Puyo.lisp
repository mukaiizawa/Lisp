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

;; cutting-puyo {{{

 (defmethod cutting-puyo ((player player))
   (dotimes (i 2)
     (let ((target-puyo (nth i (player-curr-puyos player)))
           (onother-puyo (nth (- 1 i) (player-curr-puyos player))))
       (unless (vector= (vector+ (puyo-point target-puyo)
                                 +vector-top+)
                        (puyo-point onother-puyo))
         (while (puttable? player (vector+ (puyo-point target-puyo) +vector-top+))
           (setf (puyo-point target-puyo)
                 (vector+ (puyo-point target-puyo) +vector-top+))
           (itemmove (player-canvas player)
                     (puyo-id target-puyo)
                     0
                     cell-size))))))

;; }}}
;; move-bottom {{{

(defmethod move-bottom ((player player))
  (while (movable? player +vector-top+)
    (move-puyos player +vector-top+)))

;; }}}
;; puttable? {{{

(defmethod puttable? ((player player) points)
  (every (lambda (point)
           (and (in-canvas? point)
                (not
                  (some (lambda (puyo)
                          (vector= (puyo-point puyo) point))
                        (player-puyos player)))))
         (mklist points)))

;; }}}
;; in-canvas? {{{

(defmethod in-canvas? ((point coordinate))
  (with-coordinates (point)
    (and (<= 0 x1) (< x1 width)
         (<= 0 y1) (< y1 height))))

;; }}}
;; get-rotated-outer-puyo-point {{{

(defmethod get-rotated-outer-puyo-point ((player player) (rot-dir symbol))
  (let* ((axis-puyo (first (player-curr-puyos player)))
         (outer-puyo (second (player-curr-puyos player)))
         (vector-axis (puyo-point axis-puyo))
         (vector-outer (puyo-point outer-puyo)))
    (vector+ vector-axis
             (vector-rotate (vector- vector-outer vector-axis)
                            (if (eq rot-dir 'right)
                              (/ pi 2)
                              (- (/ pi 2)))))))

;; }}}
;; rotate-puyos {{{

(defmethod rotate-puyos ((player player) (rot-dir symbol))
  (let1 (rotated-outer-puyo-point (get-rotated-outer-puyo-point player rot-dir))
    (with-coordinates ((vector- rotated-outer-puyo-point
                                (puyo-point (second (player-curr-puyos player)))))
      (itemmove (player-canvas player)
                (puyo-id (second (player-curr-puyos player)))
                (* x1 cell-size)
                (* y1 cell-size)))
    (setf (puyo-point (second (player-curr-puyos player)))
          rotated-outer-puyo-point)))

;; }}}
;; rotatable? {{{

(defmethod rotatable? ((player player) (rot-dir symbol))
  (puttable? player (get-rotated-outer-puyo-point player rot-dir)))

;; }}}
;; try-rotate {{{

(defmethod try-rotate ((player player) (rot-dir symbol))
  (if (rotatable? player rot-dir)
    (rotate-puyos player rot-dir)))

;; }}}
;; move-puyos {{{

(defmethod move-puyos ((player player) (dir coordinate))
  (with-coordinates (dir)
    (dotimes (i 2)
      (asetf (puyo-point (nth i (player-curr-puyos player))) (vector+ it dir))
      (itemmove (player-canvas player)
                (puyo-id (nth i (player-curr-puyos player)))
                (* x1 cell-size)
                (* y1 cell-size)))))

;; }}}
;; movable? {{{

(defmethod movable? ((player player) (dir coordinate))
  (puttable? player (mapcar (lambda (puyo)
                              (vector+ dir (puyo-point puyo)))
                            (player-curr-puyos player))))

;; }}}
;; try-move {{{

(defmethod try-move ((player player) (dir coordinate))
  (if (movable? player dir)
    (move-puyos player dir)
    (when (vector= dir +vector-top+)
      (cutting-puyo player)
      (asetf (player-puyos player) (append it (player-curr-puyos player)))
      (put-next-puyos player))))

;; }}}
;; put-puyo {{{

(defmethod put-puyo ((player player) (canvas-kind symbol) (puyo puyo) (point coordinate))
  (with-coordinates (point)
    (setf (puyo-point puyo) point
          (puyo-id puyo) (create-oval (if (eq canvas-kind 'canvas)
                                        (player-canvas player)
                                        (player-canvas-next player))
                                      (* x1 cell-size)
                                      (* y1 cell-size)
                                      (+ (* cell-size x1) cell-size)
                                      (+ (* cell-size y1) cell-size)))
    (itemconfigure (if (eq canvas-kind 'canvas)
                     (player-canvas player)
                     (player-canvas-next player))
                   (puyo-id puyo)
                   "fill"
                   (puyo-color puyo))
    puyo))

;; }}}
;; put-next-puyos {{{

(defmethod put-next-puyos ((player player))
  (setf (player-curr-puyos player) (loop for i from 0 to 1 collect
                                         (let1 (puyo (nth i (player-next-puyos player)))
                                           (itemdelete (player-canvas-next player) (puyo-id puyo))
                                           (put-puyo player 'canvas puyo (make-vector 2 i))))
        (player-next-puyos player) (create-next-puyos player)))

;; }}}
;; create-next-puyos {{{

(defmethod create-next-puyos ((player player))
  (loop for i from 0 to 1 collect
        (let1 (puyo (copy-puyo (nth (random (length puyo-origin) (make-random-state t))
                                    puyo-origin)))
          (put-puyo player 'canvas-next puyo (make-vector 0 i)))))

;; }}}
;; init-player {{{

(defmethod init-player ((player player))
  (let* ((canvas-width (* cell-size width))
         (canvas-height (* cell-size height)))
    ; (itemconfigure (player-canvas player)
    ;                (create-rectangle (player-canvas player)
    ;                                  0 0 canvas-width canvas-height)
    ;                "fill" "#c0c0c0")
    (dorange (x 0 width)
      (let1 (dx (* x cell-size))
        (create-line (player-canvas player)
                     (list dx 0 dx canvas-height))))
    (dorange (y 0 height)
      (let1 (dy (* y cell-size))
        (create-line (player-canvas player)
                     (list 0 dy canvas-width dy))))))

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
                  :next-puyos nil
                  :curr-puyos nil)
        puyo-origin (list
                      (make-puyo :sym 'red :color "#ff0000")
                      (make-puyo :sym 'green :color "#00ff00")
                      (make-puyo :sym 'blue :color "#0000ff")
                      (make-puyo :sym 'purple :color "#ff00ff")))
  (init-player player1)
  (setf (player-next-puyos player1) (create-next-puyos player1))
  (print player1)
  (put-next-puyos player1)
  (print player1))

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
     (bind-key #\h (try-move player1 +vector-left+))
     (bind-key #\j (move-bottom player1))
     (bind-key #\k (try-rotate player1 'right))
     (bind-key #\l (try-move player1 +vector-right+))))

;; }}}
;; main {{{

(defun main ()
  (after-time drawing-interval
              (lambda ()
                (unless end-game?
                  (try-move player1 +vector-top+)
                  ; (try-move player2 +vector-top+)
                  (main)))))

;; }}}

(defun puyo-puyo ()
  (with-ltk ()
    (init)
    (bind-keys)
    (force-focus frame)
    (main)))

(puyo-puyo)
