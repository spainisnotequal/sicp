;; ============= ;;
;; Exercise 1.38 ;;
;; ============= ;;


(define (cont-frac n d k)
  (define (frac i)
    (if (< i k)
	(/ (n i) (+ (d i) (frac (+ i 1))))
	(/ (n i) (d i))))
  (frac 1))


(define (d k)
  (if (is-divisible-by-3? (+ k 1))
      (* 2 (/ (+ k 1) 3))
      1))

(define (e-aproximation k)
  (+ 2 (cont-frac (lambda (i) 1.0)
		  d
		  k)))

(define (is-divisible-by-3? x)
  (= (remainder x 3) 0))


(newline)
(newline)
(display "e aproximation (k=3) =   ")
(display (e-aproximation 3))
(newline)
(display "e aproximation (k=6) =   ")
(display (e-aproximation 6))
(newline)
(display "e aproximation (k=9) =   ")
(display (e-aproximation 9))
(newline)
(display "e aproximation (k=99) =  ")
(display (e-aproximation 99))
(newline)
(newline)
