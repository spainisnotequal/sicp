;; ====================== ;;
;; Exercises of Chapter 1 ;;
;; ====================== ;;

;; ------------ ;;
;; Exercise 1.8 ;;
;; ------------ ;;


;; Modified version of the cube roots by Newton's method

(define (cubert-iter guess x)
  (if (good-enough? guess x)
      guess
      (cubert-iter (improve guess x) x)))

(define (improve guess x)
  (/ (+ (/ x (square guess))
	(* 2 guess))
     3))

(define (square x)
  (* x x))

(define (good-enough? guess x)
  (< (abs (- guess (improve guess x)))
     (* 0.001 guess))) ;; stop the algorithm when the change between guesses is less than 0.1% of the last guess.

(define (cubert x)
  (cubert-iter 1.0 x))
