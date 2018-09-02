;; ============= ;;
;; Exercise 1.46 ;;
;; ============= ;;

(define (iterative-improve test improve)
  ; we don't use lambda in this case because we need to give a name to this procedure in order to call itself recurrsively, so it is much simpler just to return "iter"
  (define (iter guess)
    (if (test guess)
	guess
	(iter (improve guess))))
  iter)


;; Square root:
;; ------------

(define tolerance 0.001)

(define (sqrt x)
  ((iterative-improve (lambda (guess) (< (abs (- (square guess) x)) tolerance))
		      (lambda (guess) (average guess (/ x guess))))
   1.0))

(sqrt 25)


;; Fixed point:
;; ------------

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  ((iterative-improve (lambda (guess) (< (abs (- guess (f guess))) tolerance))
		      (lambda (guess) (f guess)))
   first-guess))

(fixed-point cos 1.0)

