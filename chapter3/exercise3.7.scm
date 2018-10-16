;; ============ ;;
;; Exercise 3.7 ;;
;; ============ ;;

(define (make-joint account password password-new-account)
  (define (dispatch password-guess m)
    (if (equal? password-guess password-new-account)
	(account password m)
	(display "Incorrect Password")))
  dispatch)

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch password-guess m)
    (if (not (equal? password-guess password))
	(display "Incorrect Password")
	(cond ((eq? m 'withdraw) withdraw)
	      ((eq? m 'deposit) deposit)
	      (else (error "Unknown request -- MAKE-ACCOUNT"
			   m)))))
  dispatch)

(define peter-acc (make-account 100 'open-sesame))
((peter-acc 'open-sesame 'withdraw) 33)

; the procedure make-joint doesn't detect at this point that the password of the given account is incorrect
(define paul-acc  (make-joint peter-acc 'wrong-password 'rosebud))
((paul-acc 'rosebud 'deposit) 33) ; here is detected, but should have been detected before...

(define paul-acc  (make-joint peter-acc 'open-sesame 'rosebud))
((paul-acc 'wrong-password 'deposit) 33) ; in the new account, if the password is wrong, it is correctly detected
((paul-acc 'rosebud 'deposit) 33)
