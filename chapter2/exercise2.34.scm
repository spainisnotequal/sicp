;; ============= ;;
;; Exercise 2.34 ;;
;; ============= ;;

(define (accumulate proc initial sequence)
  (if (null? sequence)
      initial
      (proc (car sequence)
	    (accumulate proc initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1)) ; 79
