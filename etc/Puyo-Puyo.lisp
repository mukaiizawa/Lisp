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
(defmethod width ((pc puyo-canvas ))
  (puyo-canvas-width pc))
(defmethod height ((pc puyo-canvas ))
  (puyo-canvas-height pc))
(defmethod widget ((pc puyo-canvas ))
  (puyo-canvas-widget pc))

; ;; put-next-puyo {{{
;
; (defun put-next-puyo ()
;   (setf
;     (puyo-canvas-puyo-puyo (puyo-root-canvas-right *puyo-root*))
;     (loop for i from 0 to 1 collect
;           (let ((puyo (copy-puyo (nth (random (length *puyos*)) *puyos*))))
;             (setf (puyo-p puyo)
;                   (make-vector (/ *board-width* 2) i))
;             (put-puyo (puyo-root-canvas-right *puyo-root*) puyo)
;             puyo))))
;
; ;; }}}
; ;; move-puyo {{{
;
; (defun move-puyo (puyo r)
;   (with-coordinates (r)
;     (itemmove (puyo-canvas-widget (puyo-root-canvas-left *puyo-root*))
;               (puyo-id puyo)
;               (* x1 *cell-size*)
;               (* y1 *cell-size*))))
;
; ;; }}}
; ;; move-puyo-puyo {{{
;
; (defun move-puyo-puyo (dir)
;   (dolist (puyo (puyo-canvas-puyo-puyo (puyo-root-canvas-left *puyo-root*)))
;     (move-puyo puyo dir)))
;
; ;; }}}
; ;; movable? {{{
;
; (defun movable? (dir)
;   (let* ((puyo-puyo (mapcar (lambda (x)
;                               (vector+ dir (puyo-p x)))
;                             (puyo-canvas-puyo-puyo (puyo-root-canvas-left *puyo-root*))))
;          (puyo1 (first puyo-puyo))
;          (puyo2 (second puyo-puyo)))
;     (not
;       (some (lambda (x)
;               (or (vector= x puyo1)
;                   (vector= x puyo2)))
;             (puyo-root-puyo-list *puyo-root*)))))
;
; ;; }}}
;; try-move {{{

(defun try-move (arg) )
; (defun try-move (dir)
;   (if (movable? dir)
;     (move-puyo-puyo dir)
;     (progn
;       (set-puyo-from-next)
;       (put-next-puyo))))

;; }}}
;; try-rotate {{{

(defun try-rotate ())

;; }}}
; ;; put-puyo {{{
;
; (defmethod put-puyo ((pc puyo-canvas) (puyo puyo))
;   (with-coordinates ((puyo-p puyo))
;     (setf (puyo-id puyo)
;           (create-oval (puyo-canvas-widget pc)
;                        (* x1 *cell-size*)
;                        (* y1 *cell-size*)
;                        (+ (* *cell-size* x1) *cell-size*)
;                        (+ (* *cell-size* y1) *cell-size*)))
;     (itemconfigure (puyo-canvas-widget pc)
;                    (puyo-id puyo)
;                    "fill"
;                    (puyo-color puyo))
;     (values)))
;
; ;; }}}

;; create-puyo {{{

(defun create-puyo ()
  (loop for i from 0 to 1 collect
        (let ((puyo (copy-puyo (nth (random (length puyos)) puyos))))
          (setf (puyo-point puyo) (make-vector (/ (width canvas) 2) i))
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
                 :width 10
                 :height 20
                 :widget (pack
                           (make-canvas frame
                                        :width (* cell-size 10)
                                        :height (* cell-size 20))
                           :side :left))
        canvas-next (make-puyo-canvas
                      :width 3
                      :height 3
                      :widget (pack
                                (make-canvas frame
                                             :width 3
                                             :height 3)
                                :side :left))
        puyos (list
                (make-puyo :sym 'red :color "#ff0000")
                (make-puyo :sym 'green :color "#00ff00")
                (make-puyo :sym 'blue :color "#0000ff")
                (make-puyo :sym 'purple :color "#ff00ff"))
        next-puyos (create-puyo))
  (init-canvas canvas)
  (init-canvas canvas-next))

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
     (bind-key #\h (try-move +vector-left+))
     (bind-key #\j (try-move +vector-top+))
     (bind-key #\k (try-rotate))
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
    (bind *tk* "<Control-c>" (ilambda (event) (setf *exit-mainloop* t)))
    (bind-keys)
    (force-focus frame)
    (main)))

(puyo-puyo)
