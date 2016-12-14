
;; library point
(defstruct point x y)
(defmethod x ((p point)) (point-x p))
(defmethod y ((p point)) (point-y p))
(defun @ (x y) (make-point :x x :y y))
(defmethod eq? ((p1 point) (p2 point)) (and (= (x p1) (x p2)) (= (y p1) (y p2))))

;; library matrix
(defstruct matrix width height contents)
(defmethod height ((m matrix)) (matrix-height m))
(defmethod width ((m matrix)) (matrix-width m))
(defmethod contents ((m matrix)) (matrix-contents m))
(defmethod init ((m matrix) (p point))
  (setf (matrix-width m) (x p)
        (matrix-height m) (y p)
        (matrix-contents m) (make-array (* (x p) (y p))))
  m)
(defmethod inside? ((m matrix) (p point))
  (and (<= 0 (x p)) (< (x p) (width m))
       (<= 0 (y p)) (< (y p) (height m))))
(defmethod contentsIndex ((m matrix) (p point))
  (if (inside? m p)
    (+ (x p) (* (y p) (width m)))
    (error "contentsIndex: illegal index `~A' out of bounds ([0-~A], [0-~A])."
           p (width m) (height m))))
(defmacro doMatrix ((matrix point &optional value) &rest body)
  (let ((g (gensym)))
    `(progn
       (dotimes (,g (* (width ,matrix) (height ,matrix)))
         (let (
               ,@(remove nil
                         (list
                           `(,point (@ (mod ,g (width ,matrix)) (truncate ,g (width ,matrix))))
                           (when value `(,value (aref (matrix-contents ,matrix) ,g))))
                         :key #'first))
           ,@body))
       ,matrix)))
; (defmethod doMatrix ((m matrix) (fn function))
;   (dotimes (i (* (width m) (height m)))
;     (funcall fn (@ (mod i (width m)) (truncate i (height m))) (aref (matrix-contents m) i)))
;   m)
(defmethod at ((m matrix) (p point))
  (aref (matrix-contents m) (contentsIndex m p)))
(defmethod putAt ((m matrix) (p point) (v t))
  (setf (aref (matrix-contents m) (contentsIndex m p)) v)
  m)

;; library stream
(defvar Out *standard-output*)
(defmethod put ((stream stream) (obj t))
  (princ obj stream)
  stream)
(defmethod putLn ((stream stream) (obj t))
  (put (put stream obj) #\Newline))

;; life game main
(defvar n 17)
(defvar board (init (make-matrix) (@ n n)))
(defvar neighbors (init (make-matrix) (@ n n)))
(defun setNeighbors (neighbors)
  (doMatrix (neighbors p)
    (putAt neighbors p
           (count-if (lambda (q)
                       (and (inside? board q) (at board q)))
                     (apply #'append
                            (loop for i from -1 to 1
                                  collect (loop for j from -1 to 1 when (not (= i j 0))
                                                collect (@ (+ (x p) i) (+ (y p) j)))))))))
(defun nextGeneration (neighbors)
  (doMatrix (board p v)
    (let ((n (at neighbors p)))
      (putAt board p (or (and v (= n 2)) (= n 3))))))
(defun showBoard (board)
  (doMatrix (board p v)
    (funcall (if (= (x p) (1- (width board))) #'putLn #'put) Out (if v #\# #\Space))))
(defun setup (board)
  (doMatrix (board p)
    (putAt board p (evenp (random 1000 (make-random-state t))))))
(defun main (&optional args)
  (setup board)
  (dotimes (i 100)
    #+clisp(shell "clear")    ; depend on clisp
    (showBoard (nextGeneration (setNeighbors neighbors)))
    (sleep 0.1)))

(main)


