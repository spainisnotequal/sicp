;; ============= ;;
;; Exercise 1.29 ;;
;; ============= ;;

;; Integration limits definitions to use as inputs in the examples:
(define a 0)
(define b 0.999999)

;; Cube procedure
(define (cube x)
  (* x x x))

;; Higher-order sum procedure
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))


;; ---------------------------- ;;
;; First method for integration ;;
;; ---------------------------- ;;

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

;; Example:
(newline)
(newline)
(display "First method:")
(newline)
(display "Integral when dx is 0.01   = ")
(display (integral cube a b 0.01))
(newline)
(display "Integral when dx is 0.001  = ")
(display (integral cube a b 0.001))
(newline)


;; ----------------------------- ;;
;; Second method: Simpson's Rule ;;
;; ----------------------------- ;;

(define (inc n)
  (+ n 1))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define h3 (/ h 3))
  (define (y k)
    (f (+ a (* k h))))
  (define (coefficient k)
    (cond ((= k 0) 1)
	  ((= k n) 1)
	  ((even? k) 2)
	  (else 4)))
  (define (term k)
    (* (coefficient k) (y k)))
  (* h3 (sum term 0 inc n)))

(newline)
(display "Simpson's Rule method:")
(newline)
(display "Integral using 100 terms   = ")
(display (integral cube a b 100))
(newline)
(display "Integral using 10000 terms = ")
(display (integral cube a b 10000))
(newline)
(newline)
