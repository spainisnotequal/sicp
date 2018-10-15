;; ============ ;;
;; Exercise 3.3 ;;
;; ============ ;;

(define (make-account balance password)
  (define password-attemps 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (call-the-cops)
    (newline)
    (display "Calling the cops, mate...")
    (newline))
  (define (dispatch password-guess m)
    (if (not (equal? password-guess password))
	(begin (display "Incorrect Password")
	       (set! password-attemps (+ password-attemps 1))
	       (if (> password-attemps 7)
		   (call-the-cops)))
	(begin (set! password-attemps 0)
	       (cond ((eq? m 'withdraw) withdraw)
		     ((eq? m 'deposit) deposit)
		     (else (error "Unknown request -- MAKE-ACCOUNT"
				  m))))))
  dispatch)

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
((acc 'wrong-password 'deposit) 50)
