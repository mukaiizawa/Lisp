
(require :stdlib *module-stdlib*)
(require :math *module-math*)

;; queueing-theory
;; A/B/C
;; A := distribution of arrival time
;; B := distribution of service time
;; C := the number of parallel servers

;; Wr := response time
;; Wq := mean waiting time
;; Wt (<=>1/mu)  := the average service time of a single task
;; c := number of server
;; mu  := the average service rate of a single service
;; rho := traffic intensity

;; Wr := Wt + Wq
;; Wq := (p/c*(1-rho))Wt
;; p := ((c*rho)^c * p0) / c!(1 - rho)
;; p0 := [Sigma_c-1_n=0{ (c*rho)^n / n! } + (c*rho)^c / c!(1 - rho) ]^-1

(defparameter c 2)
(defparameter lambda 7.778)

(defmacro rho ()
  `(/ (* lambda wt)
      c))

(defmacro p0 ()
  `(/ 1
      (+
        (sigma 0 (1- c)
               (lambda (n)
                 (/ (expt (* c (rho)) n)
                    (fact n))))
        (/ (expt (* c (rho)) c)
           (* (fact c) (- 1 (rho)))))))

(defmacro p ()
  `(/ (* (expt (* c (rho)) c) (p0))
      (* (fact c) (- 1 (rho)))))

(defmacro Wq ()
  `(* (/ (p)
         (* c (- 1 (rho))))
      Wt))

(defmacro Wr ()
  `(+ Wt (Wq)))

;; estimate Wt
(for (Wt 0.2 (< Wt 0.25) :step 0.001)
  (echo Wt #\tab (ignore-errors (Wr))))

