; get the roots of an overdamped RLC circuit

(define (alpha R C)
  (/ 1 (* 2  R C)))

(define (omega0 L C)
  (/ 1 (sqrt (* L C))))

(define (od-get-roots R L C)
  (let* ((a (alpha R C))
         (w (omega0 L C)))
    (cons (+ (- a) (sqrt (- (* a a) (* w w))))
          (- (- a) (sqrt (- (* a a) (* w w))))
          )))

(od-get-roots 100 0.0625 0.000001)