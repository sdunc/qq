; Stephen Duncanson
; qq - Hamming Codes

; seven bits total
; 4 data, 3 parity

(define (sum-selected p l)
  ; accept a list of positions in a list p
  ; l list of all things to pick from
  ; return the sum of those selected positions
  (if (= (length p) 0)
      0
      (+ (list-ref l (car p)) (sum-selected (cdr p) l))))

;(define l (list 2 3 4 5 2 1 9 8 4))
;(sum-selected (list 1 3) l)

; use lists?
(define (get-4bit-parity l)
  ; even parity
  ; takes a list of length 4
  ; (list p3 p5 p6 p7) ; data bits
  ; p1 p2 p4 ; parity bits
   (let* ((p1 (if (= (modulo (sum-selected (list 0 1 3) l) 2) 0) 0 1))
          (p2 (if (= (modulo (sum-selected (list 0 2 3) l) 2) 0) 0 1))
          (p4 (if (= (modulo (sum-selected (list 1 2 3) l) 2) 0) 0 1)))
     (list p1 p2 p4))) ; return parity bits

; tested and working! 
;(get-4bit-parity (list 0 1 0 1))

(define (get-index l n)
  ; return the index of integer n in list l
  ; assuming that it IS in the list
  (define (index-counter l n c)
    ; c = total number of steps through list = index
    (if (= (car l) n)
        c
        (index-counter (cdr l) n (+ c 1))))
  (index-counter l n 0))

(define (list-arranger elements order)
  ; take 2 lists of equal length
  ; elements and order
  ; returns a single list of the elements arranged in the order
  ; Example:
  ; (list-arranger (list 2 4 6) (list 2 0 1))
  ; (4 6 2)
  (define (list-treader elements order n-wanted total-wanted)
    ; when total-wanted = 0, '()
    (if (= total-wanted 0)
        '()
        (cons (list-ref elements (get-index order n-wanted)) (list-treader elements order (+ n-wanted 1) (- total-wanted 1)))))
  (list-treader elements order 0 (length elements))) ; see if the -1 fixes indexing

(define (4bit-hamming-code l)
  ; l is a list of 4 bits, to be encoded
  (let ((parity-bit-list (get-4bit-parity l)))
    (let ((unordered-bits (append parity-bit-list l)))
      
      ; nested lets nice, lol.
      ; (list p1 p2 p4 p3 p5 p6 p7)
      (list-arranger unordered-bits (list 0 1 3 2 4 5 6)))))

(4bit-hamming-code (list 0 0 1 0))
(4bit-hamming-code (list 0 1 0 1))
(4bit-hamming-code (list 1 0 0 1))
