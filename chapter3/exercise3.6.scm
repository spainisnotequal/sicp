;; ============ ;;
;; Exercise 3.6 ;;
;; ============ ;;

(define (rand message)
  (let ((x random-init))
    (cond ((equal? message 'generate) (set! x (rand-update x)))
	  ((equal? message 'reset) (lambda (new-value)
				     (set! x new-value)))
	  (else (error "Unknown request: RAND" m)))))
