;; ============= ;;
;; Exercise 1.40 ;;
;; ============= ;;

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))


(define (cubic a b c)
  (lambda (x) (+ (cube x)
		 (* a (square x))
		 (* b x)
		 c)))

(define (cube x)
  (* x x x))


;; Example: x³ - 7x² + 4x +12 = 0
;; Solutions: x = -1; x = 2; x = 6
(newtons-method (cubic -7 4 12) -3)
(newtons-method (cubic -7 4 12) 1)
(newtons-method (cubic -7 4 12) 9)
