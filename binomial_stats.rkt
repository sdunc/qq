(define (n-choose-k n k)
  (define (factorial n)
    (define (iter product counter)
      (if (> counter n)
          product
          (iter (* counter product)
                (+ counter 1))))
    (iter 1 1))
  (cond ((< n k) 0)
        ((< k 0) 0)
        (else (/ (factorial n)(* (factorial (- n k)) (factorial k))))))

(define (binomial x n p)
  (let ((q (- 1 p)))
   (* (n-choose-k n x) (expt p x) (expt q (- n x)))))

(binomial 5 250 0.02)

(define (poisson x n p)
  (define (factorial n)
    (define (iter product counter)
      (if (> counter n)
          product
          (iter (* counter product)
                (+ counter 1))))
    (iter 1 1))
  (let* ((e 2.7182818)
         (lam (* n p)))
    (* (expt e (- lam)) (/ (expt lam x) (factorial x)))))

(poisson 5 250 0.02)

