
(require "stdlib" *module-stdlib*)
(require "math" *module-math*)

;; queueing-theory
;; A/B/C
;; A := distribution of arrival time
;; B := distribution of service time
;; C := the number of parallel servers

;; Wq := mean waiting time
;; c := number of server
;; mu  := the average service rate of a single service
;; Wt (<=>1/mu)  := the average service time of a single task
;; rho := traffic intensity

;; Wq := p / c*mu(1 - rho) = c*(1-rho)*Wt
;; p := (c^c)*(rho^c)p0/c!(1-rho)
;; p0 := [Sum_c-1_k=0{ (c^k)(rho^k)/k! + (c^c)(rho^c)/c!(1 - rho) }]^-1

(defun p0 (c rho)
  (/ 1
     (+
       (sigma 0 (1- c)
              (lambda (k)
                (/ (* (expt c k) (expt rho k))
                   (fact k))))
       (/ (* (expt c c) (expt rho c))
          (* (fact c) (- 1 rho))))))

(defun p (c rho)
  (/ (* (expt c c) (expt rho c) (p0 c rho))
     (* (fact c) (- 1 rho))))

(defun a (c rho)
  (/ (p c rho)
     (* c (- 1 rho))))

(defmacro rho (c)
  `(/ (* lambda wt)
      ,c))

;; c = 2
; (defmacro wt ()
;   `(+ wt
;       (*
;         (/ (* 2 (expt (rho 2) 2))
;            (- 1 (expt (rho 2) 2)))
;         wt)))

; c = 1
(defmacro wt ()
  `(-
     (+ wt
        (*
          (/ (rho 1)
             (- 1 (rho 1)))
          wt))
     1))

(for (i 0.1 (< i 0.2) :step 0.0001)
  (let ((lambda 7)
        (wt i))
    (echo i #\tab (ignore-errors (wt)))))

