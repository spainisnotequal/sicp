;; ============= ;;
;; Exercise 1.29 ;;
;; ============= ;;


;; Iterative higher-order sum procedure
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ (term a) result))))
  (iter a 0))


;; ----------------------- ;;
;; Sum of integers example ;;
;; ----------------------- ;;

(define (identity n)
  n)

(define (add1 n)
  (+ n 1))

(define (int-sum a b)
  (sum identity a add1 b))

(newline)
(newline)
(display "Summation from 1 to 4 = ")
(display (int-sum 1 4))
(newline)


;; -------------------- ;;
;; Sum of cubes example ;;
;; -------------------- ;;

(define (cube n)
  (* n n n))

(define (cube-sum a b)
  (sum cube a add1 b))

(display "Summation of the cubes from 1 to 4 = ")
(display (cube-sum 1 4))
(newline)
(newline)
