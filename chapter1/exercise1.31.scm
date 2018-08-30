;; ============= ;;
;; Exercise 1.31 ;;
;; ============= ;;


;; -------- ;;
;; 1.31 (a) ;;
;; -------- ;;

;; Higher-order sum procedure (recursive process)

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

;; Factorial:

(define (identity n)
  n)

(define (add1 n)
  (+ n 1))

(define (factorial n)
  (product identity 1 add1 n))

(newline)
(newline)
(display "Factorial of 5 = ")
(display (factorial 5))
(newline)


;; Pi aproximation using John Wallis' formula:

(define (pi n)
  (define (pi-term k) 
    (if (even? k) 
	(/ (+ k 2) (+ k 1)) 
	(/ (+ k 1) (+ k 2))))
  (* 4.0 (product pi-term 1 add1 n)))


(newline)
(display "Pi (n=10)    = ")
(display (pi 10))
(newline)
(display "Pi (n=1000)  = ")
(display (pi 1000))
(newline)
(display "Pi (n=10000) = ")
(display (pi 10000))
(newline)


;; -------- ;;
;; 1.31 (b) ;;
;; -------- ;;


;; Higher-order sum procedure (iterative process)

(define (product term a next b)
  (define (iter a result)    
    (if (> a b)
	result
	(iter (next a) (* a result))))
  (iter a 1))

(define (factorial n)
  (product identity 1 add1 n))

(newline)
(display "Factorial of 5 = ")
(display (factorial 5))
(newline)
(newline)
