
(require "ltk" *module-ltk*)
(require "cordinate-manager" *module-cordinate-manager*)
(require "stdlib" *module-stdlib*)

(defstruct board-status
  (value 0 :type integer)
  (color "green" :type string)
  (obj nil))

(defparameter *cell-width* 30)
(defparameter *board-width* 10)
(defparameter *board-height* 20)
(defparameter *board* (make-array '(20 10) :initial-element (make-board-status)))
(defparameter *tetriminos* nil)

;; deftetrimino {{{

(defmacro deftetrimino (&rest vectors)
  `(push (make-vector-list ,@vectors)
         *tetriminos*))

;; }}}
;; I {{{

(deftetrimino
  (0 2)
  (0 1)
  (0 0)
  (0 -1))

;; }}}
;; J {{{

(deftetrimino
         (0 2)
         (0 1)
  (-1 0) (0 0))

;; }}}
;; L {{{

(deftetrimino
  (0 2)
  (0 1)
  (0 0) (1 0))

;; }}}
;; O {{{

(deftetrimino
  (0 1) (1 1)
  (0 0) (1 0))

;; }}}
;; S {{{

(deftetrimino
          (0 1) (1 1)
  (-1 -1) (0 0))

;; }}}
;; T {{{

(deftetrimino
  (-1 0) (0 0) (1 0)
         (0 -1))

;; }}}
;; Z {{{

(deftetrimino
  (-1 -1) (0 1)
          (0 0) (1 0))

;; }}}

;; direction {{{

(defmacro defdirection (direction x y)
  `(defmacro ,direction ()
     (make-cordinate :x ,x :y ,y)))

(defmacro defdirections (&rest directions)
  `(progn ,@(mapcar (lambda (direction)
                      `(defdirection ,@direction))
                    directions)))

(defdirections
  (top 0 -1)
  (bottom 0 1)
  (left -1 0)
  (right 1 0)
  (top-left -1 -1)
  (top-right 1 -1)
  (bottom-left -1 1)
  (bottom-right 1 1))

;; }}}
;; draw-board {{{

(defmacro draw-board ()
  `(progn
     (dorange (x 0 *board-width*)
       (dorange (y 0 *board-height*)
         (create-rectangle canvas
                           (* x *cell-width*)
                           (* y *cell-width*)
                           (+ (* *cell-width* x) *cell-width*)
                           (+ (* *cell-width* y) *cell-width*))))))

;; }}}
;; safety-aref {{{

(defun safety-aref (board cordinate)
  (with-cordinates (cordinate)
    (if (and (<= 0 x1) (< x1 8)
             (<= 0 y1) (< y1 8))
      (aref board x1 y1)
      'wall)))

;; }}}
;; print-board {{{

(defun print-board (board)
  (fresh-line)
  (dorange (x 0 7)
    (dorange (y 0 7)
      (princ (mkstr (aref board y x) #\Space)))
    (princ #\Newline)))

;; }}}

(with-ltk ()
  (bind *tk* "<KeyPress-q>" (ilambda (event) (setf *exit-mainloop* t)))
  (let1 (canvas (pack (make-instance 'canvas
                                     :width (* *cell-width* *board-width*)
                                     :height (* *cell-width* *board-height*))))
    (draw-board)))


