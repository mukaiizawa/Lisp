
(load "../lib/stdlib")

(defmacro bind-cordinate (area &body body)
  `(destructuring-bind
     (x1 y1 x2 y2)
     (list (point-x (area-p1 ,area))
           (point-y (area-p1 ,area))
           (point-x (area-p2 ,area))
           (point-y (area-p2 ,area)))
     (declare (ignorable x1 x2 y1 y2))
     ,@body))

;; from area to ordered-area(p1:bottm-left, p2:top-right)
(defun ordered-area (area)
  (bind-cordinate area
                  ; Two points to determine a rectangle domain uniquely
                  (cond ((and (< x1 x2)    ; p1:bottom-left, p2:top-right
                              (< y1 y2))
                         area)
                        ((and (> x1 x2)    ; p1:top-right, p2:bottom-left
                              (> y1 y2))
                         (make-area (area-p2 area) (area-p1 area)))
                        ((and (< x1 x2)    ; p1:top-left, p2:bottom-right
                              (> y1 y2))
                         (make-area (make-point x1 y2)
                                    (make-point x2 y1)))
                        ((and (> x1 x2)    ; p1:bottom-right, p2:top-left
                              (< y1 y2))
                         (make-area (make-point x2 y1)
                                    (make-point x1 y2)))
                        (t (error "illegal area surround with (~A , ~A), (~A, ~A)" x1 y1 x2 y2)))))


(defun make-point (x y)
  (cons (+ x 0.0) (+ y 0.0)))

(defun point-x (p)
  (car p))

(defun point-y (p)
  (cdr p))

(defun make-area (p1 p2)
  (cons p1 p2))

(defun area-p1 (area)
  (car area))

(defun area-p2 (area)
  (cdr area))

(defun random-in-range (low high)
  (let ((range (- high low)))
    (+ low (random range) 0.0)))

(defun random-point (area)
  (bind-cordinate  (ordered-area area)
                   (make-point (random-in-range (min x1 x2) (max x1 x2))
                               (random-in-range (min y1 y2) (max y1 y2)))))

(defun size-of-area (area)
  (bind-cordinate  (ordered-area area)
                   (abs (* (- x2 x1) (- y2 y1)))))

(defun in-area? (fn p)
  (let* ((x (point-x p))
         (y (point-y p))
         (fx (funcall fn x)))
    (and (<= (abs y) (abs fx))
         (or (zerop y)
             (and (minusp y) (minusp fx))
             (and (plusp y) (plusp fx))))))

(defun monte-carlo-integration (fn p1 p2 &key (trial-time 1000000))
  (let ((area (make-area p1 p2))
        (in-area 0))
    (dotimes (i trial-time)
      (when (in-area? fn (random-point area))
        (incf in-area)))
    (* (/ in-area trial-time 1.0) (size-of-area area))))


;; monte-carlo-integration
;; 一価関数のみ対応
;; 積分したい範囲を長方形の対角の点を指定して囲む
;; 領域は狭い方が制度が高い
;; プロット数は多い方が制度が高い
;;

#o(monte-carlo-integration (lambda (x)
                             x)
                           (make-point 0 0) (make-point 1 1)
                           :trial-time 10)
; => 0.5

#o(monte-carlo-integration (lambda (x)
                             x)
                           (make-point 0 0) (make-point 1 1))
; => 0.5

#o(monte-carlo-integration (lambda (x)
                             (* x x))
                           (make-point 0 0) (make-point 6 36))
; => 72

#o(monte-carlo-integration (lambda (x)
                             (- 1 (* x x)))
                           (make-point -1 -1) (make-point 1 1))
; => 4/3

#o(monte-carlo-integration (lambda (x)
                             1)
                           (make-point 0 0) (make-point 2 2))
; => 2

