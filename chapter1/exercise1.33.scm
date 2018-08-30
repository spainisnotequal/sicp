;; ============= ;;
;; Exercise 1.33 ;;
;; ============= ;;

(define (filtered-accumulate combiner null-value term a next b filter)
  (cond ((> a b) null-value)
	((filter a) (combiner (term a)
			      (filtered-accumulate combiner null-value term (next a) next b filter)))
        (else (filtered-accumulate combiner null-value term (next a) next b filter))))

(define (sum-of-odds a b)
  (filtered-accumulate + 0 identity a add1 b odd?))

(define (product-of-evens a b)
  (filtered-accumulate * 1 identity a add1 b even?))

(define (identity x) x)
(define (add1 x) (+ x 1))


(newline)
(newline)
(display "Sum of odd intergers from 1 to 5 = ")
(display (sum-of-odds 1 5))
(newline)
(display "Product of even intergers from 1 to 5 = ")
(display (product-of-evens 1 5))
(newline)
(newline)


