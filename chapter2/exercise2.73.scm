;; ============= ;;
;; Exercise 2.73 ;;
;; ============= ;;

;; --------- ;;
;; Section b ;;
;; --------- ;;

(define (install-deriv-of-sum-package)
  ;; internal procedures
  (define (deriv exp var)
    (make-sum (deriv (addend exp) var)
	      (deriv (augand exp) var)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag '+ x))
  (put 'deriv '+ deriv)
  'done)


(define (install-deriv-of-product-package)
  ;; internal procedures
  (define (deriv exp var)
    (make-sum (make-product (multiplier exp)
			    (deriv (multiplicand exp) var))
	      (make-product (deriv (multiplier exp) var)
			    (multiplicand exp))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag '* x))
  (put 'deriv '* deriv)
  'done)

(define (install-deriv-of-exponent-package)
  ;; internal procedures
  (define (deriv exp var)
    (cond ((number? exp) 0)
	  ((variable? exp)
	   (if (same-variable? exp var) 1 0))
	  ((sum? exp)
	   (make-sum (deriv (addend exp) var)
		     (deriv (augend exp) var)))
	  ((product? exp)
	   (make-sum
	    (make-product (multiplier exp)
			  (deriv (multiplicand exp) var))
	    (make-product (deriv (multiplier exp) var)
			  (multiplicand exp))))
	  ((exponentiation? exp)
	   (make-product (make-product (exponent exp)
				       (make-exponentiation (base exp)
							    (- (exponent exp) 1)))
			 (deriv (base exp) var)))
	  (else
	   (error "unknown expression type -- DERIV" exp))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag '** x))
  (put 'deriv '** deriv)
  'done)

(define (install-deriv-of-exponent-package)
  ;; internal procedures
  (define (deriv exp var)
    (cond ((number? exp) 0)
	  ((variable? exp)
	   (if (same-variable? exp var) 1 0))
	  ((sum? exp)
	   (make-sum (deriv (addend exp) var)
		     (deriv (augend exp) var)))
	  ((product? exp)
	   (make-sum
	    (make-product (multiplier exp)
			  (deriv (multiplicand exp) var))
	    (make-product (deriv (multiplier exp) var)
			  (multiplicand exp))))
	  ((exponentiation? exp)
	   (make-product (make-product (exponent exp)
				       (make-exponentiation (base exp)
							    (- (exponent exp) 1)))
			 (deriv (base exp) var)))
	  (else
	   (error "unknown expression type -- DERIV" exp))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag '** x))
  (put 'deriv '** deriv)
  'done)

;; --------- ;;
;; Section c ;;
;; --------- ;;

(define (install-deriv-of-exponent-package)
  ;; internal procedures
  (define (deriv exp var)
    (cond ((number? exp) 0)
	  ((variable? exp)
	   (if (same-variable? exp var) 1 0))
	  ((sum? exp)
	   (make-sum (deriv (addend exp) var)
		     (deriv (augend exp) var)))
	  ((product? exp)
	   (make-sum
	    (make-product (multiplier exp)
			  (deriv (multiplicand exp) var))
	    (make-product (deriv (multiplier exp) var)
			  (multiplicand exp))))
	  ((exponentiation? exp)
	   (make-product (make-product (exponent exp)
				       (make-exponentiation (base exp)
							    (- (exponent exp) 1)))
			 (deriv (base exp) var)))
	  (else
	   (error "unknown expression type -- DERIV" exp))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag '** x))
  (put 'deriv '** deriv)
  'done)

;; --------- ;;
;; Section d ;;
;; --------- ;;

;; We only have to change the corresponding "put expressions" to:
(put '+ 'deriv deriv)
(put '* 'deriv deriv)
(put '** 'deriv deriv)
