;; ============= ;;
;; Exercise 1.32 ;;
;; ============= ;;


;; --------------------------------------- ;;
;; 1.32 (a) Accumulate (recursive process) ;;
;; --------------------------------------- ;;

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))


(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))


;; Examples:

(define (identity x)
  x)

(define (add1 x)
  (+ x 1))

(define (int-sum a b)
  (sum identity a add1 b))

(define (int-product a b)
  (product identity a add1 b))


(define (cube x)
  (* x x x))

(define (cube-sum a b)
  (sum cube a add1 b))

(define (cube-product a b)
  (product cube a add1 b))


(newline)
(newline)
(display "Sum of intergers from 2 to 3 = ")
(display (int-sum 2 3))
(newline)
(display "Sum of cubes of intergers from 2 to 3 = ")
(display (cube-sum 2 3))
(newline)
(display "Product of intergers from 2 to 3 = ")
(display (int-product 2 3))
(newline)
(display "Product of cubes of intergers from 2 to 3 = ")
(display (cube-product 2 3))
(newline)


;; --------------------------------------- ;;
;; 1.32 (b) Accumulate (iterative process) ;;
;; --------------------------------------- ;;


(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a)
	      (combiner (term a) result))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))


;; Examples:

(define (add2 x)
  (+ x 2))

(define (odds-sum n)
  (sum identity 1 add2 n))

(define (evens-product n)
  (product identity 2 add2 n))


(newline)
(display "Sum of odd intergers from 1 to 3 = ")
(display (odds-sum 3))
(newline)
(display "Product of even intergers from 2 to 5 = ")
(display (evens-product 5))
(newline)
(newline)
