;; ============= ;;
;; Exercise 1.36 ;;
;; ============= ;;

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display guess)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(define starting-point 10.0)

(newline)
(newline)
(display "x maps to log(1000)/log(x):")

(fixed-point (lambda (x) (/ (log 1000) (log x)))
	     starting-point)

(define (average x y)
  (/ (+ x y) 2))

(newline)
(newline)
(display "x maps to log(1000)/log(x), using average dumping:")

(fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
	     starting-point)

(newline)
(newline)
