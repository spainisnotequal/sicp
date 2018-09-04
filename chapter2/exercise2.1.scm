;; ============ ;;
;; Exercise 2.1 ;;
;; ============ ;;


(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (make-rat n d)
  (let ((g (abs (gcd n d)))) ;; to try to simplify the solution, I get rid of the sign of the greatest common divisor
    (if (negative? d)
	(cons (/ (- n) g) (/ (- d) g))
	(cons (/ n g) (/ d g)))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (numer rat)
  (car rat))

(define (denom rat)
  (cdr rat))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-rat (make-rat 1 2))
(print-rat (make-rat -1 2))
(print-rat (make-rat 1 -2))
(print-rat (make-rat -1 -2))
