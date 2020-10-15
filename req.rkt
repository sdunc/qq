; Stephen Duncanson
; Find Req for series or parallel resistors

; a list of resistors having values of 2,3, and 4 ohms.
(define res-list (list 2 2 2))

; series resisors are just added up
(define (sum-list list)
  (if (= (length list) 1)
      (car list)
      (+ (car list) (sum-list (cdr list)))))

(define (get-series-req rlist)
  ; move sum-list in here? to be a helper?
  ; returns the equiv. resistance of a list of resistors in series.
  (sum-list rlist))

(define (get-parallel-req rlist)
  ; returns req of list of parallel resistors.
  (let ((irlist (map (lambda (x) (/ 1 x)) rlist)))
    (/ 1 (sum-list irlist))))

(get-series-req res-list)
(get-parallel-req res-list)

; add interval calculations for tolerances
; add a resistor lookup calculator
; and a reverse lookup
; give 4 or 5 colors = value
; give value = 4 / 5 colors