
(require "ltk" *module-ltk*)
(require "stdlib" *module-stdlib*)
(require "test-utils" *module-test-utils*)

(defparameter *board* (make-array '(8 8)))
(defvar *board-stack-back*)
(defvar *board-stack-forward*)

(defparameter cell-size 55)
(defparameter margin-size 10)

(defconstant BLACK 1)
(defconstant WHITE 2)

(defun init-board! (board)
  (setf *board-stack-back* nil)
  (setf *board-stack-forward* nil)
  (dorange (i 0 (1- (array-dimension board 0)))
        (dorange (j 0 (1- (array-dimension board 1)))
              (cond ((and (= i (1- (/ (array-dimension board 0) 2)))
                          (= j (1- (/ (array-dimension board 1) 2))))
                     (setf (aref board i j) BLACK))
                    ((and (= i (/ (array-dimension board 0) 2))
                          (= j (/ (array-dimension board 1) 2)))
                     (setf (aref board i j) BLACK))
                    ((and (= i (1- (/ (array-dimension board 0) 2)))
                          (= j (/ (array-dimension board 1) 2)))
                     (setf (aref board i j) WHITE))
                    ((and (= i (/ (array-dimension board 0) 2))
                          (= j (1- (/ (array-dimension board 1) 2))))
                     (setf (aref board i j) WHITE))
                    (t (setf (aref board i j) 0))))))

(defun h-line-plus-check (board i j self-b/w &key (cnt 0))
  (let ((another-b/w (if (= self-b/w BLACK) WHITE BLACK)))
    (cond ((= j (1- (array-dimension board 1))) nil)
          ((= (aref board i (1+ j)) 0) nil)
          ((= (aref board i (1+ j)) another-b/w)
           (h-line-plus-check board i (1+ j) self-b/w :cnt (1+ cnt)))
          ((= (aref board i (1+ j)) self-b/w) cnt))))

(defun h-line-plus-reverse! (board i j self-b/w)
  (let ((another-b/w (if (= self-b/w BLACK) WHITE BLACK)))
    (if (= (aref board i (1+ j)) another-b/w)
      (progn
        (setf (aref board i (1+ j)) self-b/w)
        (h-line-plus-reverse! board i (1+ j) self-b/w)))))

(defun h-line-minus-check (board i j self-b/w  &key (cnt 0))
  (let ((another-b/w (if (= self-b/w BLACK) WHITE BLACK)))
    (cond ((= j 0) nil)
          ((= (aref board i (1- j)) 0) nil)
          ((= (aref board i (1- j)) another-b/w)
           (h-line-minus-check board i (1- j) self-b/w :cnt (1+ cnt)))
          ((= (aref board i (1- j)) self-b/w) cnt))))

(defun h-line-minus-reverse! (board i j self-b/w)
  (let ((another-b/w (if (= self-b/w BLACK) WHITE BLACK)))
    (if (= (aref board i (1- j)) another-b/w)
      (progn
        (setf (aref board i (1- j)) self-b/w)
        (h-line-minus-reverse! board i (1- j) self-b/w)))))

(defun v-line-plus-check (board i j self-b/w  &key (cnt 0))
  (let ((another-b/w (if (= self-b/w BLACK) WHITE BLACK)))
    (cond ((= i (1- (array-dimension board 0))) nil)
          ((= (aref board (1+ i) j) 0) nil)
          ((= (aref board (1+ i) j) another-b/w)
           (v-line-plus-check board (1+ i) j self-b/w :cnt (1+ cnt)))
          ((= (aref board (1+ i) j) self-b/w) cnt))))

(defun v-line-plus-reverse! (board i j self-b/w)
  (let ((another-b/w (if (= self-b/w BLACK) WHITE BLACK)))
    (if (= (aref board (1+ i) j) another-b/w)
      (progn
        (setf (aref board (1+ i) j) self-b/w)
        (v-line-plus-reverse! board (1+ i) j self-b/w)))))

(defun v-line-minus-check (board i j self-b/w  &key (cnt 0))
  (let ((another-b/w (if (= self-b/w BLACK) WHITE BLACK)))
    (cond ((= i 0) nil)
          ((= (aref board (1- i) j) 0) nil)
          ((= (aref board (1- i) j) another-b/w)
           (v-line-minus-check board (1- i) j self-b/w :cnt (1+ cnt)))
          ((= (aref board (1- i) j) self-b/w) cnt))))

(defun v-line-minus-reverse! (board i j self-b/w)
  (let ((another-b/w (if (= self-b/w BLACK) WHITE BLACK)))
    (if (= (aref board (1- i) j) another-b/w)
      (progn
        (setf (aref board (1- i) j) self-b/w)
        (v-line-minus-reverse! board (1- i) j self-b/w)))))

(defun right-naname-plus-check (board i j self-b/w  &key (cnt 0))
  (let ((another-b/w (if (= self-b/w BLACK) WHITE BLACK)))
    (cond ((or (= i (1- (array-dimension board 0)))
               (= j (1- (array-dimension board 1)))) nil)
          ((= (aref board (1+ i) (1+ j)) 0) nil)
          ((= (aref board (1+ i) (1+ j)) another-b/w)
           (right-naname-plus-check board (1+ i) (1+ j) self-b/w :cnt (1+ cnt)))
          ((= (aref board (1+ i) (1+ j)) self-b/w) cnt))))

(defun right-naname-plus-reverse! (board i j self-b/w)
  (let ((another-b/w (if (= self-b/w BLACK) WHITE BLACK)))
    (if (= (aref board (1+ i) (1+ j)) another-b/w)
      (progn
        (setf (aref board (1+ i) (1+ j)) self-b/w)
        (right-naname-plus-reverse! board (1+ i) (1+ j) self-b/w)))))

(defun right-naname-minus-check (board i j self-b/w  &key (cnt 0))
  (let ((another-b/w (if (= self-b/w BLACK) WHITE BLACK)))
    (cond ((or (= i 0)
               (= j 0)) nil)
          ((= (aref board (1- i) (1- j)) 0) nil)
          ((= (aref board (1- i) (1- j)) another-b/w)
           (right-naname-minus-check board (1- i) (1- j) self-b/w :cnt (1+ cnt)))
          ((= (aref board (1- i) (1- j)) self-b/w) cnt))))

(defun right-naname-minus-reverse! (board i j self-b/w)
  (let ((another-b/w (if (= self-b/w BLACK) WHITE BLACK)))
    (if (= (aref board (1- i) (1- j)) another-b/w)
      (progn
        (setf (aref board (1- i) (1- j)) self-b/w)
        (right-naname-minus-reverse! board (1- i) (1- j) self-b/w)))))

(defun left-naname-plus-check (board i j self-b/w  &key (cnt 0))
  (let ((another-b/w (if (= self-b/w BLACK) WHITE BLACK)))
    (cond ((or (= i 0)
               (= j (1- (array-dimension board 1)))) nil)
          ((= (aref board (1- i) (1+ j)) 0) nil)
          ((= (aref board (1- i) (1+ j)) another-b/w)
           (left-naname-plus-check board (1- i) (1+ j) self-b/w :cnt (1+ cnt)))
          ((= (aref board (1- i) (1+ j)) self-b/w) cnt))))

(defun left-naname-plus-reverse! (board i j self-b/w)
  (let ((another-b/w (if (= self-b/w BLACK) WHITE BLACK)))
    (if (= (aref board (1- i) (1+ j)) another-b/w)
      (progn
        (setf (aref board (1- i) (1+ j)) self-b/w)
        (left-naname-plus-reverse! board (1- i) (1+ j) self-b/w)))))

(defun left-naname-minus-check (board i j self-b/w  &key (cnt 0))
  (let ((another-b/w (if (= self-b/w BLACK) WHITE BLACK)))
    (cond ((or (= i (1- (array-dimension board 0)))
               (= j 0)) nil)
          ((= (aref board (1+ i) (1- j)) 0) nil)
          ((= (aref board (1+ i) (1- j)) another-b/w)
           (left-naname-minus-check board (1+ i) (1- j) self-b/w :cnt (1+ cnt)))
          ((= (aref board (1+ i) (1- j)) self-b/w) cnt))))

(defun left-naname-minus-reverse! (board i j self-b/w)
  (let ((another-b/w (if (= self-b/w BLACK) WHITE BLACK)))
    (if (= (aref board (1+ i) (1- j)) another-b/w)
      (progn
        (setf (aref board (1+ i) (1- j)) self-b/w)
        (left-naname-minus-reverse! board (1+ i) (1- j) self-b/w)))))

(defun put-stone! (board i j b/w)
  (let ((h-line-plus-check?        (and (h-line-plus-check        board i j b/w)
                                        (> (h-line-plus-check        board i j b/w) 0)))
        (h-line-minus-check?       (and (h-line-minus-check       board i j b/w)
                                        (> (h-line-minus-check       board i j b/w) 0)))
        (v-line-plus-check?        (and (v-line-plus-check        board i j b/w)
                                        (> (v-line-plus-check        board i j b/w) 0)))
        (v-line-minus-check?       (and (v-line-minus-check       board i j b/w)
                                        (> (v-line-minus-check       board i j b/w) 0)))
        (right-naname-plus-check?  (and (right-naname-plus-check  board i j b/w)
                                        (> (right-naname-plus-check  board i j b/w) 0)))
        (right-naname-minus-check? (and (right-naname-minus-check board i j b/w)
                                        (> (right-naname-minus-check board i j b/w) 0)))
        (left-naname-plus-check?   (and (left-naname-plus-check   board i j b/w)
                                        (> (left-naname-plus-check   board i j b/w) 0)))
        (left-naname-minus-check?  (and (left-naname-minus-check  board i j b/w)
                                        (> (left-naname-minus-check  board i j b/w) 0))))

    (if h-line-plus-check?  (h-line-plus-reverse! board i j b/w))
    (if h-line-minus-check? (h-line-minus-reverse! board i j b/w))
    (if v-line-plus-check?  (v-line-plus-reverse! board i j b/w))
    (if v-line-minus-check? (v-line-minus-reverse! board i j b/w))
    (if right-naname-plus-check?  (right-naname-plus-reverse! board i j b/w))
    (if right-naname-minus-check? (right-naname-minus-reverse! board i j b/w))
    (if left-naname-plus-check?   (left-naname-plus-reverse! board i j b/w))
    (if left-naname-minus-check?  (left-naname-minus-reverse! board i j b/w))

    (if (and
          (zerop (aref board i j))
          (or h-line-plus-check?       h-line-minus-check?
              v-line-plus-check?       v-line-minus-check?
              right-naname-plus-check? right-naname-minus-check? 
              left-naname-plus-check?  left-naname-minus-check?  ))
      (progn (setf (aref board i j) b/w)
             b/w)
      nil)))

(defun put-able? (board i j b/w)
  (let ((h-line-plus-check?        (and (h-line-plus-check        board i j b/w)
                                        (> (h-line-plus-check        board i j b/w) 0)))
        (h-line-minus-check?       (and (h-line-minus-check       board i j b/w)
                                        (> (h-line-minus-check       board i j b/w) 0)))
        (v-line-plus-check?        (and (v-line-plus-check        board i j b/w)
                                        (> (v-line-plus-check        board i j b/w) 0)))
        (v-line-minus-check?       (and (v-line-minus-check       board i j b/w)
                                        (> (v-line-minus-check       board i j b/w) 0)))
        (right-naname-plus-check?  (and (right-naname-plus-check  board i j b/w)
                                        (> (right-naname-plus-check  board i j b/w) 0)))
        (right-naname-minus-check? (and (right-naname-minus-check board i j b/w)
                                        (> (right-naname-minus-check board i j b/w) 0)))
        (left-naname-plus-check?   (and (left-naname-plus-check   board i j b/w)
                                        (> (left-naname-plus-check   board i j b/w) 0)))
        (left-naname-minus-check?  (and (left-naname-minus-check  board i j b/w)
                                        (> (left-naname-minus-check  board i j b/w) 0))))

    (and
      (zerop (aref board i j))
      (or h-line-plus-check?       h-line-minus-check?
          v-line-plus-check?       v-line-minus-check?
          right-naname-plus-check? right-naname-minus-check? 
          left-naname-plus-check?  left-naname-minus-check?  ))))

(defun print-board (board)
  (format t "   ")
  (dorange (j 0 (1- (array-dimension board 1)))
        (format t "~A " j))
  (format t "~%~%")

  (dorange (i 0 (1- (array-dimension board 0)))
        (format t "~A  " i)
        (dorange (j 0 (1- (array-dimension board 1)))
              (format t "~A " (aref board i j)))
        (format t "~%")))

(defun create-board (canvas board margin-size cell-size stone-color)
  (dorange (i 0 (1- (array-dimension board 0)))
        (dorange (j 0 (1- (array-dimension board 1)))
              (if (put-able? board i j stone-color)
                (itemconfigure
                  canvas
                  (create-rectangle canvas
                                    (+ margin-size (* j cell-size))
                                    (+ margin-size (* i cell-size))
                                    (+ margin-size (* j cell-size) cell-size)
                                    (+ margin-size (* i cell-size) cell-size))
                  "fill" "#008000")
                (itemconfigure
                  canvas
                  (create-rectangle canvas
                                    (+ margin-size (* j cell-size))
                                    (+ margin-size (* i cell-size))
                                    (+ margin-size (* j cell-size) cell-size)
                                    (+ margin-size (* i cell-size) cell-size))
                  "fill" "dark green")))))

(defun reflect-board (canvas board margin-size cell-size)
  (dorange (i 0 (1- (array-dimension board 0)))
        (dorange (j 0 (1- (array-dimension board 1)))
              (cond ((= (aref board i j) BLACK)
                     (itemconfigure
                       canvas
                       (create-oval canvas
                                    (+ margin-size (* j cell-size) (/ (- cell-size (* cell-size 0.8)) 2))
                                    (+ margin-size (* i cell-size) (/ (- cell-size (* cell-size 0.8)) 2))
                                    (+ margin-size (* j cell-size)
                                       (- cell-size (/ (- cell-size (* cell-size 0.8)) 2)))
                                    (+ margin-size (* i cell-size)
                                       (- cell-size (/ (- cell-size (* cell-size 0.8)) 2))))
                       "fill" "black"))
                    ((= (aref board i j) WHITE)
                     (itemconfigure
                       canvas
                       (create-oval canvas
                                    (+ margin-size (* j cell-size) (/ (- cell-size (* cell-size 0.8)) 2))
                                    (+ margin-size (* i cell-size) (/ (- cell-size (* cell-size 0.8)) 2))
                                    (+ margin-size (* j cell-size)
                                       (- cell-size (/ (- cell-size (* cell-size 0.8)) 2)))
                                    (+ margin-size (* i cell-size)
                                       (- cell-size (/ (- cell-size (* cell-size 0.8)) 2))))
                       "fill" "white"))))))

(defun copy-array (a)
  (let ((new-a (make-array (array-dimensions a))))
    (dorange (i 0 (1- (array-dimension new-a 0)))
          (dorange (j 0 (1- (array-dimension new-a 1)))
                (setf (aref new-a i j) (aref a i j))))
    new-a))

(defun display-board (board)
  (with-ltk ()
    (let* ((board-frame (make-instance 'frame))
           (canvas (make-canvas board-frame
                                :width  (+ (* (array-dimension board 1) cell-size)
                                           (* 2 margin-size))
                                :height (+ (* (array-dimension board 0) cell-size)
                                           (* 2 margin-size))))
           (stone-color BLACK)
           (button-frame (make-instance 'frame)))
      (pack board-frame)
      (pack canvas :side :left)
      (pack button-frame)
      (create-board canvas board margin-size cell-size stone-color)
      (reflect-board canvas board margin-size cell-size)
      (bind canvas "<ButtonPress-1>"
            (lambda (evt)	      
              (let ((i (truncate (- (event-y evt) margin-size) cell-size))
                    (j (truncate (- (event-x evt) margin-size) cell-size)))
                (push (copy-array board) *board-stack-back*)
                (setf *board-stack-forward* nil)
                (when (put-stone! board i j stone-color)
                  (clear canvas)
                  (create-board canvas board margin-size cell-size
                                (if (= stone-color BLACK) WHITE BLACK))
                  (reflect-board canvas board margin-size cell-size)
                  (let ((b-cnt 0) (w-cnt 0))
                    (dorange (i 0 (1- (array-dimension board 0)))
                      (dorange (j 0 (1- (array-dimension board 1)))
                        (cond ((= (aref board i j) BLACK) (incf b-cnt))
                              ((= (aref board i j) WHITE) (incf w-cnt)))))
                    (if (= stone-color BLACK)
                      (setf stone-color WHITE)
                      (setf stone-color BLACK))))))))))

(defun reversi-board ()
  (init-board! *board*)
  (display-board *board*))

(reversi-board)

