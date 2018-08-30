;; ================================================================== ;;
;; Section 1.3: Formulating Abstractions with Higher-Order Proceudres ;;
;; ================================================================== ;;


;; ----------- ;;
;; Sum Example ;;
;; ----------- ;;


;; Higher-order sum procedure:

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))


;; Sum of integers:

(define (identity x)
  x)

(define (inc n)
  (+ n 1))

(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 2 5)


;; Sum of cubes:

(define (cube x)
  (* x x x))

(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 3)


;; Pi function sum:

(define (pi-term x)
  (/ 1.0 (* x (+ x 2))))

(define (pi-next x)
  (+ x 4))

(define (pi-sum a b)
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))
