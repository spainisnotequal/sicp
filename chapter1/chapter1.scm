;; ================================================ ;;
;; Chapter 1: Building Abstractions with Procedures ;;
;; ================================================ ;;

;; ---------------------------------------- ;;
;; Section 1.1: The Elements of Programming ;;
;; ---------------------------------------- ;;

(define pi 3.14)
(define r 10)
(define circumference (* 2 pi r))


;; Compound procedures

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))


;; Absolute value

(define (abs1 x)
  (cond
   ((> x 0) x)
   ((= x 0) 0)
   ((< x 0) (- x))))

(define (abs2 x)
  (cond
   ((< x 0) (- x))
   (else x)))

(define (abs3 x)
  (if (< x 0)
      (- x)
      x))


;; Square roots by Newton's method

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))
