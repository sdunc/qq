(define (mean n)
  ; fix this to handle numbers AND LISTS
  ; a bit messy but works!!
  ; nice
  (define (add-lower k)
    (if (= k 1)
        1
        (+ k (add-lower (- k 1)))))
  (define (sum-list list)
    (if (= (length list) 1)
        (car list)
        (+ (car list) (sum-list (cdr list)))))
  (cond ((number? n) (/ (add-lower n) n))
        ((list? n) (/ (sum-list n) (length n)))))
         
(define (sum-list list)
  (if (= (length list) 1)
      (car list)
      (+ (car list) (sum-list (cdr list)))))  
;(mean 6.0)

; a cool way to do this would be have a list
; (list (cons x1 p1) (cons x2 p2))
; where each x is an outcome of our random variable X
; and p is the probability mass of that happening
; also look at https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance

; very WIP function for variance
; this is for independent repeated trials
; and is hardcoded to work for a hw problem!!
(define (var xlist)
  ; a neat thing here would be to map
  ; the (x - mu)^2 onto a list of x's (list 1 2 3 4 5 6)
  ; and then sum that list
  (let ((mu (mean xlist))) 
    (* (sum-list (map (lambda (x) (expt (- x mu) 2)) xlist)) (/ 1 (length xlist)))))

;(* 10 (var (list 1 2 3 4 5 6)))

(define (repeat-ind-trials v n-trials)
  ; with repeated ind trials we just multiply by num of trials
  ; this is a simple interface to do that
  (* v n-trials))

(repeat-ind-trials (mean (list 1 2 3 4 5 6)) 10)
(repeat-ind-trials (var (list 1 2 3 4 5 6)) 10)
