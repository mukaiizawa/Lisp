
(require :stdlib *module-stdlib*)

(defun fizz? (n)
  (zerop (mod n 3)))

(defun buzz? (n)
  (zerop (mod n 5)))

(deftype fizz ()
  '(and (integer 0 *)
        (satisfies fizz?)))

(deftype buzz ()
  '(and (integer 0 *)
        (satisfies buzz?)))

(deftype fizz-buzz ()
  '(and
     (integer 0 *)
     (satisfies fizz?)
     (satisfies buzz?)))

(dorange (i 1 30)
  (print 
    (typecase i
      (fizz-buzz 'fizzbuzz)
      (fizz 'fizz)
      (buzz 'buzz)
      (t i))))


