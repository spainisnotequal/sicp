;; ============ ;;
;; Exercise 3.8 ;;
;; ============ ;;

(define f
  (let ((status 1))
    (lambda (x)
      (set! status (* status x))
      status)))

(+ (f 0) (f 1)) ; 1
(+ (f 1) (f 0)) ; 0

; so in my Scheme interpreter (mit-scheme 9.1.1) the arguments to + are evaluated from right to left
