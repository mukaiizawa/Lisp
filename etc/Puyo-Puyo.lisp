(require :ltk *module-ltk*)
(require :coordinate-manager *module-coordinate-manager*)
(require :stdlib *module-stdlib*)

(defparameter *cell-size* 30)
(defparameter *board-width* 4)
(defparameter *board-height* 8)
(defparameter *drawing-interval* 500)
(defparameter *puyo-root* nil)

(defstruct puyo-root
  frame canvas-left canvas-right text-area
  puyo-list game-over?)

(defstruct puyo-canvas
  widget puyo-puyo width height)

(defstruct puyo id sym color p)

(defparameter *puyos*
  (list
    (make-puyo :sym 'red :color "#ff0000")
    (make-puyo :sym 'green :color "#00ff00")
    (make-puyo :sym 'blue :color "#0000ff")
    (make-puyo :sym 'purple :color "#ff00ff")))

;; set-puyo-from-next {{{

(defun set-puyo-from-next ()
  (setf
    (puyo-canvas-puyo-puyo (puyo-root-canvas-left *puyo-root*))
    (puyo-canvas-puyo-puyo (puyo-root-canvas-right *puyo-root*))))

;; }}}
;; put-next-puyo {{{

(defun put-next-puyo ()
  (setf
    (puyo-canvas-puyo-puyo (puyo-root-canvas-right *puyo-root*))
    (loop for i from 0 to 1 collect
          (let ((puyo (copy-puyo (nth (random (length *puyos*)) *puyos*))))
            (setf (puyo-p puyo)
                  (make-vector (/ *board-width* 2) i))
            (put-puyo (puyo-root-canvas-right *puyo-root*) puyo)
            puyo))))

;; }}}
;; move-puyo {{{

(defun move-puyo (puyo r)
  (with-coordinates (r)
    (itemmove (puyo-canvas-widget (puyo-root-canvas-left *puyo-root*))
              (puyo-id puyo)
              (* x1 *cell-size*)
              (* y1 *cell-size*))))

;; }}}
;; move-puyo-puyo {{{

(defun move-puyo-puyo (dir)
  (dolist (puyo (puyo-canvas-puyo-puyo (puyo-root-canvas-left *puyo-root*)))
    (move-puyo puyo dir)))

;; }}}
;; movable? {{{

(defun movable? (dir)
  (let* ((puyo-puyo (mapcar (lambda (x)
                              (vector+ dir (puyo-p x)))
                            (puyo-canvas-puyo-puyo (puyo-root-canvas-left *puyo-root*))))
         (puyo1 (first puyo-puyo))
         (puyo2 (second puyo-puyo)))
    (not
      (some (lambda (x)
              (or (vector= x puyo1)
                  (vector= x puyo2)))
            (puyo-root-puyo-list *puyo-root*)))))

;; }}}
;; try-move {{{

(defun try-move (dir)
  (if (movable? dir)
    (move-puyo-puyo dir)
    (progn
      (set-puyo-from-next)
      (put-next-puyo))))

;; }}}
;; try-rotate {{{

(defun try-rotate ()
  (print 'rotate))

;; }}}
;; put-puyo {{{

(defmethod put-puyo ((pc puyo-canvas) (puyo puyo))
  (with-coordinates ((puyo-p puyo))
    (setf (puyo-id puyo)
          (create-oval (puyo-canvas-widget pc)
                       (* x1 *cell-size*)
                       (* y1 *cell-size*)
                       (+ (* *cell-size* x1) *cell-size*)
                       (+ (* *cell-size* y1) *cell-size*)))
    (itemconfigure (puyo-canvas-widget pc)
                   (puyo-id puyo)
                   "fill"
                   (puyo-color puyo))
    (values)))

;; }}}
;; init-canvas {{{

(defmethod init-canvas ((pc puyo-canvas))
  (let* ((c (puyo-canvas-widget pc))
         (board-width (puyo-canvas-width pc))
         (board-height (puyo-canvas-height pc))
         (canvas-width (* *cell-size* board-width))
         (canvas-height (* *cell-size* board-height)))
    (itemconfigure c
                   (create-rectangle c 0 0 canvas-width canvas-height)
                   "fill" "#c0c0c0")
    (dorange (x 0 board-width)
      (let1 (x (* x *cell-size*))
        (create-line c (list x 0 x canvas-height))))
    (dorange (y 0 board-height)
      (let1 (y (* y *cell-size*))
        (create-line c (list 0 y canvas-width y))))))

;; }}}
;; init {{{

(defun init ()
  (init-canvas (puyo-root-canvas-left *puyo-root*))
  (init-canvas (puyo-root-canvas-right *puyo-root*))
  (put-next-puyo)
  (set-puyo-from-next))

;; }}}
;; create-puyo-root {{{

(defun create-puyo-root ()
  (let1 (frame (pack (make-instance 'frame)))
    (make-puyo-root
      :frame frame
      :text-area (pack
                   (make-text frame :width nil :height 2)
                   :side :bottom)
      :canvas-left (make-puyo-canvas
                     :widget (pack
                               (make-canvas frame
                                            :width (* *cell-size* *board-width*)
                                            :height (* *cell-size* *board-height*))
                               :side :left)
                     :width *board-width*
                     :height *board-height*)
      :canvas-right (make-puyo-canvas
                      :widget (pack
                                (make-canvas frame
                                             :width (* *cell-size* *board-width*)
                                             :height (* *cell-size* *board-height*))
                                :side :left)
                      :width 4
                      :height 4))
    :game-over? nil))

;; }}}
;; bind-keypress {{{

(defmacro bind-keypress (key &body body)
  `(bind *tk* ,(mkstr "<KeyPress-" key ">")
         (ilambda (event)
           (unless (puyo-root-game-over? *puyo-root*)
             ,@body))))

;; }}}
;; main {{{

(defun main ()
  (after-time *drawing-interval*
              (lambda ()
                (unless (puyo-root-game-over? *puyo-root*)
                  (try-move +vector-top+)
                  (main)))))

;; }}}

(with-ltk ()
  (bind *tk* "<Control-c>" (ilambda (event) (setf *exit-mainloop* t)))
  (setf *puyo-root* (create-puyo-root))
  (force-focus (puyo-root-frame *puyo-root*))
  (init)
  (bind-keypress #\h (try-move +vector-left+))
  (bind-keypress #\j (try-move +vector-top+))
  (bind-keypress #\k (try-rotate))
  (bind-keypress #\l (try-move +vector-right+))
  (main))
