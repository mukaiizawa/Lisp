
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
;; μ  := the average service rate of a single service
;; ρ := traffic intensity

;; Wr := Wt + Wq
;; Wq := (p/c*(1-rho))Wt
;; p := ((c*rho)^c * p0) / c!(1 - ρ)
;; p0 := [Sigma_c-1_n=0{ (c*rho)^n / n! } + (c*rho)^c / c!(1 - ρ) ]^-1

(defparameter C 2)
(defparameter λ 7.778)

(defmacro ρ ()
  `(/ (* λ Wt)
      C))

(defmacro p0 ()
  `(/ 1
      (+
        (sigma 0 (1- C)
               (lambda (n)
                 (/ (expt (* C (ρ)) n)
                    (fact n))))
        (/ (expt (* C (ρ)) C)
           (* (fact C) (- 1 (ρ)))))))

(defmacro p ()
  `(/ (* (expt (* C (ρ)) C) (p0))
      (* (fact C) (- 1 (ρ)))))

(defmacro Wq ()
  `(* (/ (p)
         (* C (- 1 (ρ))))
      Wt))

(defmacro Wr ()
  `(+ Wt (Wq)))

;; estimate Wt
(for (Wt 0.2 (< Wt 0.25) :step 0.001)
  (echo Wt #\tab (ignore-errors (Wr))))

