; Cramer's rule
; part of ss_calc
; 2x2
; https://www.coolmath.com/algebra/14-determinants-cramers-rule/01-determinants-cramers-rule-2x2-03

; define two rows of the coefficent matrix
; r1 = [ 3 -1]
; r2 = [-5  4]
(define (make-row n1 n2)
  (cons n1 n2))

; define the column vector 'right member vector'
; c = [ 7]
;     [-2]
(define (make-col c1 c2)
  (cons c1 c2))

; eq1:  3x - 1y =  7
; eq2: -5x + 4y = -2

(define (det2x2 r1 r2)
  ; returns the det of a 2x2 matrix
  ; r1 and r2 represent rows 1 and 2 of the coeff. matrix
  (- (* (car r1) (cdr r2)) (* (car r2) (cdr r1))))

(define (cramers-rule r1 r2 c)
  ; r1 and r2 represent rows 1 and 2 of the coeff. matrix
  ; c pair represents col vector of ='s
  (let* ((d  (det2x2 r1 r2))
         (dx (det2x2 (cons (car c) (cdr r1)) (cons (cdr c) (cdr r2))))
         (dy (det2x2 (cons (car r1) (car c)) (cons (car r2) (cdr c)))))
    (cons (/ dx d) (/ dy d))))

(define cr (cramers-rule (make-row 3 -1)
              (make-row -5 4) (make-col 7 -2)))

(define (make-dec p)
  ; convert a (gag) mixed number into a decimal value
  (cons (* 1.0 (car p)) (* 1.0 (cdr p)) ))

(make-dec cr)