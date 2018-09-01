;; ============= ;;
;; Exercise 1.39 ;;
;; ============= ;;


(define (cont-frac n d k)
  (define (frac i)
    (if (< i k)
	(/ (n i) (+ (d i) (frac (+ i 1))))
	(/ (n i) (d i))))
  (frac 1))

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1)
			     x
			     (- (* x x))))
	     (lambda (i) (- (* 2 i) 1))
	     k))


(define pi 3.1415926535897932384626433832795)

(newline)
(newline)
(display "tan(pi/4) (Scheme) =   ")
(display (tan (/ pi 4)))
(newline)
(display "tan-cf(pi/4) (k=1) =   ")
(display (tan-cf (/ pi 4) 1))
(newline)
(display "tan-cf(pi/4) (k=3) =   ")
(display (tan-cf (/ pi 4) 3))
(newline)
(display "tan-cf(pi/4) (k=5) =   ")
(display (tan-cf (/ pi 4) 5))
(newline)
(newline)
